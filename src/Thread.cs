﻿using Key = System.UInt64;
using Bitboard = System.UInt64;
using Move = System.Int32;
using File = System.Int32;
using Rank = System.Int32;
using Score = System.Int32;
using Square = System.Int32;
using Color = System.Int32;
using Value = System.Int32;
using PieceType = System.Int32;
using Piece = System.Int32;
using CastleRight = System.Int32;
using Depth = System.Int32;
using Result = System.Int32;
using ScaleFactor = System.Int32;
using Phase = System.Int32;
using NodeType = System.Int32;

#if WINDOWS_RT
using Windows.System.Threading;
#endif

namespace Portfish
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Runtime.CompilerServices;
    using System.Threading;

    internal sealed class SplitPoint
    {
        // Const data after splitPoint has been setup
        internal SplitPoint parent;

        internal Position pos;

        internal int depth;

        internal int beta;

        internal int nodeType;

        internal Thread master;

        internal int threatMove;

        // Const pointers to shared data
        internal MovePicker mp;

        internal Stack[] ss;

        internal int ssPos;

        // Shared data
        internal readonly object Lock = new object();

        internal Position[] activePositions = new Position[Constants.MAX_THREADS];

        internal ulong slavesMask;

        internal long nodes;

        internal volatile int alpha;

        internal volatile int bestValue;

        internal volatile int bestMove;

        internal volatile int moveCount;

        internal volatile bool cutoff;

#if ACTIVE_REPARENT
        internal volatile bool allSlavesRunning;
#endif
    };

    /// Thread struct keeps together all the thread related stuff like locks, state
    /// and especially split points. We also use per-thread pawn and material hash
    /// tables so that once we get a pointer to an entry its life time is unlimited
    /// and we don't have to care about someone changing the entry under our feet.
    //internal delegate void ThreadInit();
    internal enum ThreadLoopType
    {
        Main,

        Idle,

        Timer
    }

    class LimitedSizeDictionary<TKey, TValue> : Dictionary<TKey, TValue>
    {
        Queue<TKey> queue;
        int size;

        public LimitedSizeDictionary(int size) 
            : base(size + 1)
        {
            this.size = size;
            queue = new Queue<TKey>(size);
        }

        public void Add(TKey key, TValue value)
        {
            base.Add(key, value);
            if (queue.Count == size)
                base.Remove(queue.Dequeue());
            queue.Enqueue(key);
        }

        public bool Remove(TKey key)
        {
            if (base.Remove(key))
            {
                Queue<TKey> newQueue = new Queue<TKey>(size);
                foreach (TKey item in queue)
                    if (!base.Comparer.Equals(item, key))
                        newQueue.Enqueue(item);
                queue = newQueue;
                return true;
            }
            else
                return false;
        }
    }
    internal sealed class Thread
    {
        const int TableSize = 262144;
        internal LimitedSizeDictionary<Key, Entry> evalTable = new LimitedSizeDictionary<Key, Entry>(262144);

        internal readonly SplitPoint[] splitPoints = new SplitPoint[Constants.MAX_SPLITPOINTS_PER_THREAD];

        internal readonly MaterialTable materialTable = new MaterialTable();

        internal readonly PawnTable pawnTable = new PawnTable();

        internal int idx;

        internal ThreadLoopType loopType;

        internal int maxPly;

        internal readonly object sleepLock = new object();

        internal readonly object sleepCond = new object();

        internal volatile SplitPoint curSplitPoint;

        internal volatile int splitPointsCnt;

        internal volatile bool is_searching;

        internal volatile bool do_sleep;

        internal volatile bool do_exit;

        internal Thread(ThreadLoopType lt, ManualResetEvent initEvent)
        {
            this.is_searching = this.do_exit = false;
            this.maxPly = this.splitPointsCnt = 0;
            this.curSplitPoint = null;
            this.loopType = lt;
            this.idx = Threads.size();

            this.do_sleep = this.loopType != ThreadLoopType.Main; // Avoid a race with start_searching()

            for (var j = 0; j < Constants.MAX_SPLITPOINTS_PER_THREAD; j++)
            {
                this.splitPoints[j] = new SplitPoint();
            }

#if WINDOWS_RT
            Windows.Foundation.IAsyncAction action = Windows.System.Threading.ThreadPool.RunAsync(delegate { StartThread(initEvent); }, WorkItemPriority.Normal);
#else
            ThreadPool.QueueUserWorkItem(this.StartThread, initEvent);
#endif
        }

        internal void StartThread(object state)
        {
            var initEvent = (ManualResetEvent)state;
            BrokerManager.Warmup();
            if (this.loopType == ThreadLoopType.Timer)
            {
                this.timer_loop(initEvent);
            }
            if (this.loopType == ThreadLoopType.Main)
            {
                this.main_loop(initEvent);
            }
            if (this.loopType == ThreadLoopType.Idle)
            {
                this.idle_loop(initEvent);
            }
        }

        internal void exit()
        {
            Debug.Assert(this.do_sleep);

            this.do_exit = true; // Search must be already finished
            this.wake_up();
        }

        // Thread::wake_up() wakes up the thread, normally at the beginning of the search
        // or, if "sleeping threads" is used at split time.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal void wake_up()
        {
            ThreadHelper.lock_grab(this.sleepLock);
            ThreadHelper.cond_signal(this.sleepCond);
            ThreadHelper.lock_release(this.sleepLock);
        }

        // Thread::wait_for_stop_or_ponderhit() is called when the maximum depth is
        // reached while the program is pondering. The point is to work around a wrinkle
        // in the UCI protocol: When pondering, the engine is not allowed to give a
        // "bestmove" before the GUI sends it a "stop" or "ponderhit" command. We simply
        // wait here until one of these commands (that raise StopRequest) is sent and
        // then return, after which the bestmove and pondermove will be printed.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal void wait_for_stop_or_ponderhit()
        {
            Search.SignalsStopOnPonderhit = true;

            ThreadHelper.lock_grab(this.sleepLock);

            while (!Search.SignalsStop)
            {
                ThreadHelper.cond_wait(this.sleepCond, this.sleepLock);
            }

            ThreadHelper.lock_release(this.sleepLock);
        }

        // Thread::cutoff_occurred() checks whether a beta cutoff has occurred in the
        // current active split point, or in some ancestor of the split point.
        internal bool cutoff_occurred()
        {
            for (var sp = this.curSplitPoint; sp != null; sp = sp.parent)
            {
                if (sp.cutoff)
                {
                    return true;
                }
            }
            return false;
        }

        // Thread::is_available_to() checks whether the thread is available to help the
        // thread 'master' at a split point. An obvious requirement is that thread must
        // be idle. With more than two threads, this is not sufficient: If the thread is
        // the master of some active split point, it is only available as a slave to the
        // slaves which are busy searching the split point at the top of slaves split
        // point stack (the "helpful master concept" in YBWC terminology).
        internal bool is_available_to(Thread master)
        {
            if (this.is_searching)
            {
                return false;
            }

            // Make a local copy to be sure doesn't become zero under our feet while
            // testing next condition and so leading to an out of bound access.
            var spCnt = this.splitPointsCnt;

            // No active split points means that the thread is available as a slave for any
            // other thread otherwise apply the "helpful master" concept if possible.
            return (spCnt == 0) || ((this.splitPoints[spCnt - 1].slavesMask & (1UL << master.idx)) != 0);
        }

        #region Loops

        // Thread::main_loop() is where the main thread is parked waiting to be started
        // when there is a new search. Main thread will launch all the slave threads.
        internal void main_loop(ManualResetEvent initEvent)
        {
            // Initialize the TT here
            var ttSize = uint.Parse(OptionMap.Instance["Hash"].v);
            if (TT.size != ttSize)
            {
                TT.set_size(ttSize);
            }

            // Signal done
            initEvent.Set();

            while (true)
            {
                ThreadHelper.lock_grab(this.sleepLock);

                this.do_sleep = true; // Always return to sleep after a search
                this.is_searching = false;

                while (this.do_sleep && !this.do_exit)
                {
                    ThreadHelper.cond_signal(Threads.sleepCond); // Wake up UI thread if needed
                    ThreadHelper.cond_wait(this.sleepCond, this.sleepLock);
                }

                ThreadHelper.lock_release(this.sleepLock);

                if (this.do_exit)
                {
                    return;
                }

                this.is_searching = true;

                Search.think(); // This is the search entry point
            }
        }

        // Thread::timer_loop() is where the timer thread waits maxPly milliseconds and
        // then calls do_timer_event(). If maxPly is 0 thread sleeps until is woken up.
        internal void timer_loop(ManualResetEvent initEvent)
        {
            // Signal done
            initEvent.Set();

            while (!this.do_exit)
            {
                ThreadHelper.lock_grab(this.sleepLock);
                ThreadHelper.cond_timedwait(
                    this.sleepCond,
                    this.sleepLock,
                    this.maxPly != 0 ? this.maxPly : Constants.INT_MAX);
                ThreadHelper.lock_release(this.sleepLock);
                Search.check_time();
            }
        }

        /// Thread::idle_loop() is where the thread is parked when it has no work to do
        internal void idle_loop(ManualResetEvent initEvent)
        {
            var sp_master = this.splitPointsCnt > 0 ? this.curSplitPoint : null;
            Debug.Assert(sp_master == null || (sp_master.master == this && this.is_searching));

            if (initEvent != null)
            {
                // Signal done
                initEvent.Set();
            }

            var use_sleeping_threads = Threads.useSleepingThreads;

            // If this thread is the master of a split point and all slaves have
            // finished their work at this split point, return from the idle loop.
            while ((sp_master == null) || (sp_master.slavesMask != 0))
            {
                // If we are not searching, wait for a condition to be signaled
                // instead of wasting CPU time polling for work.
                while (this.do_sleep || this.do_exit || (!this.is_searching && use_sleeping_threads))
                {
                    if (this.do_exit)
                    {
                        Debug.Assert(sp_master == null);
                        return;
                    }

                    // Grab the lock to avoid races with Thread::wake_up()
                    ThreadHelper.lock_grab(this.sleepLock);

                    // If we are master and all slaves have finished don't go to sleep
                    if ((sp_master != null) && (sp_master.slavesMask == 0))
                    {
                        ThreadHelper.lock_release(this.sleepLock);
                        break;
                    }

                    // Do sleep after retesting sleep conditions under lock protection, in
                    // particular we need to avoid a deadlock in case a master thread has,
                    // in the meanwhile, allocated us and sent the wake_up() call before we
                    // had the chance to grab the lock.
                    if (this.do_sleep || !this.is_searching)
                    {
                        ThreadHelper.cond_wait(this.sleepCond, this.sleepLock);
                    }

                    ThreadHelper.lock_release(this.sleepLock);
                }

                // If this thread has been assigned work, launch a search
                if (this.is_searching)
                {
                    Debug.Assert(!this.do_sleep && !this.do_exit);

                    ThreadHelper.lock_grab(Threads.splitLock);

                    Debug.Assert(this.is_searching);
                    var sp = this.curSplitPoint;

                    ThreadHelper.lock_release(Threads.splitLock);

                    var ls = LoopStackBroker.GetObject();
                    var ss = ls.ss;
                    var ssPos = 0;

                    var pos = PositionBroker.GetObject();
                    pos.copy(sp.pos, this);

                    Array.Copy(sp.ss, sp.ssPos - 1, ss, ssPos, 4);
                    ss[ssPos + 1].sp = sp;

                    ThreadHelper.lock_grab(sp.Lock);

                    Debug.Assert(sp.activePositions[idx] == null);
                    
                    sp.activePositions[idx] = pos;

                    if (sp.nodeType == NodeTypeC.Root)
                    {
                        Search.search(NodeTypeC.SplitPointRoot, pos, ss, ssPos + 1, sp.alpha, sp.beta, sp.depth);
                    }
                    else if (sp.nodeType == NodeTypeC.PV)
                    {
                        Search.search(NodeTypeC.SplitPointPV, pos, ss, ssPos + 1, sp.alpha, sp.beta, sp.depth);
                    }
                    else if (sp.nodeType == NodeTypeC.NonPV)
                    {
                        Search.search(NodeTypeC.SplitPointNonPV, pos, ss, ssPos + 1, sp.alpha, sp.beta, sp.depth);
                    }
                    else
                    {
                        Debug.Assert(false);
                    }

                    Debug.Assert(this.is_searching);

                    this.is_searching = false;
                    sp.activePositions[idx] = null;
#if ACTIVE_REPARENT
                    sp.allSlavesRunning = false;
#endif
                    sp.slavesMask &= ~(1UL << this.idx);
                    sp.nodes += pos.nodes;

                    // Wake up master thread so to allow it to return from the idle loop in
                    // case we are the last slave of the split point.
                    if (use_sleeping_threads && this != sp.master && !sp.master.is_searching && sp.slavesMask != 0)
                    {
                        Debug.Assert(!sp.master.is_searching);
                        sp.master.wake_up();
                    }

                    // After releasing the lock we cannot access anymore any SplitPoint
                    // related data in a safe way becuase it could have been released under
                    // our feet by the sp master. Also accessing other Thread objects is
                    // unsafe because if we are exiting there is a chance are already freed.
                    ThreadHelper.lock_release(sp.Lock);

#if ACTIVE_REPARENT
    // Try to reparent to the first split point, with still all slaves
    // running, where we are available as a possible slave.
                    for (int i = 0; i < Threads.size(); i++)
                    {
                        Thread th = Threads.threads[i];
                        int spCnt = th.splitPointsCnt;
                        SplitPoint latest = th.splitPoints[spCnt != 0 ? spCnt - 1 : 0];

                        if (this.is_available_to(th)
                            && spCnt > 0
                            && !th.cutoff_occurred()
                            && latest.allSlavesRunning
                            && Utils.more_than_one(latest.slavesMask))
                        {
                            ThreadHelper.lock_grab(latest.Lock);
                            ThreadHelper.lock_grab(Threads.splitLock);

                            // Retest all under lock protection, we are in the middle
                            // of a race storm here !
                            if (this.is_available_to(th)
                                && spCnt == th.splitPointsCnt
                                && !th.cutoff_occurred()
                                && latest.allSlavesRunning
                                && Utils.more_than_one(latest.slavesMask))
                            {
                                latest.slavesMask |= 1UL << idx;
                                curSplitPoint = latest;
                                is_searching = true;
                            }

                            ThreadHelper.lock_release(Threads.splitLock);
                            ThreadHelper.lock_release(latest.Lock);

                            break; // Exit anyhow, only one try (enough in 99% of cases)
                        }
                    }
#endif

                    pos.startState = null;
                    pos.st = null;
                    PositionBroker.Free();
                    LoopStackBroker.Free(ls);
                }
            }
        }

        #endregion
    };

    /// ThreadsManager class handles all the threads related stuff like init, starting,
    /// parking and, the most important, launching a slave thread at a split point.
    /// All the access to shared thread data is done through this class.
    internal static class Threads
    {
        /* As long as the single ThreadsManager object is defined as a global we don't
           need to explicitly initialize to zero its data members because variables with
           static storage duration are automatically set to zero before enter main()
        */

        internal static readonly List<Thread> threads = new List<Thread>();

        internal static Thread timer;

        internal static readonly object splitLock = new object();

        internal static readonly object sleepCond = new object();

        internal static int minimumSplitDepth;

        internal static int maxThreadsPerSplitPoint;

        internal static bool useSleepingThreads;

        //internal static bool use_sleeping_threads() { return useSleepingThreads; }
        internal static int min_split_depth()
        {
            return minimumSplitDepth;
        }

        internal static int size()
        {
            return threads.Count;
        }

        internal static Thread thread(int index)
        {
            return threads[index];
        }

        internal static Thread main_thread()
        {
            return threads[0];
        }

        // read_uci_options() updates internal threads parameters from the corresponding
        // UCI options and creates/destroys threads to match the requested number. Thread
        // objects are dynamically allocated to avoid creating in advance all possible
        // threads, with included pawns and material tables, if only few are used.
        internal static void read_uci_options(ManualResetEvent[] initEvents)
        {
            maxThreadsPerSplitPoint = int.Parse(OptionMap.Instance["Max Threads per Split Point"].v);
            minimumSplitDepth = int.Parse(OptionMap.Instance["Min Split Depth"].v) * DepthC.ONE_PLY;
            useSleepingThreads = bool.Parse(OptionMap.Instance["Use Sleeping Threads"].v);

            var requested = int.Parse(OptionMap.Instance["Threads"].v);
            var current = 0;

            Debug.Assert(requested > 0);

            while (size() < requested)
            {
                if (initEvents == null)
                {
                    threads.Add(new Thread(ThreadLoopType.Idle, null));
                }
                else
                {
                    threads.Add(new Thread(ThreadLoopType.Idle, initEvents[current + 2]));
                    current++;
                }
            }

            while (size() > requested)
            {
                var idx = size() - 1;
                threads[idx].exit();
                threads.RemoveAt(idx);
            }
        }

        // init() is called during startup. Initializes locks and condition variables
        // and launches all threads sending them immediately to sleep.
        internal static void init()
        {
            var requested = int.Parse(OptionMap.Instance["Threads"].v);
            var initEvents = new ManualResetEvent[requested + 1];
            for (var i = 0; i < (requested + 1); i++)
            {
                initEvents[i] = new ManualResetEvent(false);
            }

#if WINDOWS_RT
            Windows.Foundation.IAsyncAction action = Windows.System.Threading.ThreadPool.RunAsync(delegate { launch_threads(initEvents); }, WorkItemPriority.Normal);
#else
            ThreadPool.QueueUserWorkItem(launch_threads, initEvents);
#endif
            WaitHandle.WaitAll(initEvents);
        }

        private static void launch_threads(object state)
        {
            var initEvents = (ManualResetEvent[])state;
            timer = new Thread(ThreadLoopType.Timer, initEvents[0]);
            threads.Add(new Thread(ThreadLoopType.Main, initEvents[1]));
            read_uci_options(initEvents);
        }

        // exit() is called to cleanly terminate the threads when the program finishes
        internal static void exit()
        {
            for (var i = 0; i < size(); i++)
            {
                threads[i].exit();
            }

            timer.exit();
        }

        // available_slave_exists() tries to find an idle thread which is available as
        // a slave for the thread with threadID 'master'.
        internal static bool available_slave_exists(Thread master)
        {
            for (var i = 0; i < size(); i++)
            {
                if (threads[i].is_available_to(master))
                {
                    return true;
                }
            }

            return false;
        }

        // split() does the actual work of distributing the work at a node between
        // several available threads. If it does not succeed in splitting the node
        // (because no idle threads are available, or because we have no unused split
        // point objects), the function immediately returns. If splitting is possible, a
        // SplitPoint object is initialized with all the data that must be copied to the
        // helper threads and then helper threads are told that they have been assigned
        // work. This will cause them to instantly leave their idle loops and call
        // search(). When all threads have returned from search() then split() returns.
        internal static int split(
            bool Fake,
            Position pos,
            Stack[] ss,
            int ssPos,
            int alpha,
            int beta,
            int bestValue,
            ref int bestMove,
            int depth,
            int threatMove,
            int moveCount,
            MovePicker mp,
            int nodeType)
        {
            Debug.Assert(pos.pos_is_ok());
            Debug.Assert(bestValue > -ValueC.VALUE_INFINITE);
            Debug.Assert(bestValue <= alpha);
            Debug.Assert(alpha < beta);
            Debug.Assert(beta <= ValueC.VALUE_INFINITE);
            Debug.Assert(depth > DepthC.DEPTH_ZERO);

            var master = pos.this_thread();

            if (master.splitPointsCnt >= Constants.MAX_SPLITPOINTS_PER_THREAD)
            {
                return bestValue;
            }

            // Pick the next available split point from the split point stack
            var sp = master.splitPoints[master.splitPointsCnt];

            sp.parent = master.curSplitPoint;
            sp.master = master;
            sp.cutoff = false;
            sp.slavesMask = 1UL << master.idx;
#if ACTIVE_REPARENT
            sp.allSlavesRunning = true;
#endif

            sp.depth = depth;
            sp.bestMove = bestMove;
            sp.threatMove = threatMove;
            sp.alpha = alpha;
            sp.beta = beta;
            sp.nodeType = nodeType;
            sp.bestValue = bestValue;
            sp.mp = mp;
            sp.moveCount = moveCount;
            sp.pos = pos;
            sp.nodes = 0;
            sp.ss = ss;
            sp.ssPos = ssPos;

            Debug.Assert(master.is_searching);
            master.curSplitPoint = sp;

            var slavesCnt = 0;

            ThreadHelper.lock_grab(splitLock);
            ThreadHelper.lock_grab(sp.Lock);

            for (var i = 0; i < size() && !Fake; ++i)
            {
                if (threads[i].is_available_to(master))
                {
                    sp.slavesMask |= 1UL << i;
                    threads[i].curSplitPoint = sp;
                    threads[i].is_searching = true; // Slave leaves idle_loop()

                    if (useSleepingThreads)
                    {
                        threads[i].wake_up();
                    }

                    if (++slavesCnt + 1 >= maxThreadsPerSplitPoint) // Master is always included
                    {
                        break;
                    }
                }
            }

            master.splitPointsCnt++;

            ThreadHelper.lock_release(sp.Lock);
            ThreadHelper.lock_release(splitLock);
            
            // Everything is set up. The master thread enters the idle loop, from which
            // it will instantly launch a search, because its is_searching flag is set.
            // We pass the split point as a parameter to the idle loop, which means that
            // the thread will return from the idle loop when all slaves have finished
            // their work at this split point.
            if (slavesCnt != 0 || Fake)
            {
                master.idle_loop(null);
                // In helpful master concept a master can help only a sub-tree of its split
                // point, and because here is all finished is not possible master is booked.
                Debug.Assert(!master.is_searching);
            }

            // We have returned from the idle loop, which means that all threads are
            // finished. Note that setting is_searching and decreasing activeSplitPoints is
            // done under lock protection to avoid a race with Thread::is_available_to().
            ThreadHelper.lock_grab(splitLock);
            ThreadHelper.lock_grab(sp.Lock); // To protect sp->nodes

            master.is_searching = true;
            master.splitPointsCnt--;
            master.curSplitPoint = sp.parent;
            pos.nodes += sp.nodes;
            bestMove = sp.bestMove;

            ThreadHelper.lock_release(sp.Lock);
            ThreadHelper.lock_release(splitLock);
            
            return sp.bestValue;
        }

        // ThreadsManager::start_searching() wakes up the main thread sleeping in
        // main_loop() so to start a new search, then returns immediately.
        internal static void start_searching(Position pos, LimitsType limits, List<int> searchMoves)
        {
            wait_for_search_finished();

            Search.SearchTime.Reset();
            Search.SearchTime.Start(); // As early as possible

            Search.SignalsStopOnPonderhit = Search.SignalsFirstRootMove = false;
            Search.SignalsStop = Search.SignalsFailedLowAtRoot = false;

            Search.RootPos.copy(pos);
            Search.Limits = limits;
            Search.RootMoves.Clear();

            var mlist = MListBroker.GetObject();
            mlist.pos = 0;
            Movegen.generate_legal(pos, mlist.moves, ref mlist.pos);
            for (var i = 0; i < mlist.pos; i++)
            {
                var move = mlist.moves[i].move;
                if ((searchMoves.Count == 0) || Utils.existSearchMove(searchMoves, move))
                {
                    Search.RootMoves.Add(new RootMove(move));
                }
            }
            MListBroker.Free();

            main_thread().do_sleep = false;
            main_thread().wake_up();
        }

        // ThreadsManager::wait_for_search_finished() waits for main thread to go to
        // sleep, this means search is finished. Then returns.
        internal static void wait_for_search_finished()
        {
            var t = main_thread();
            ThreadHelper.lock_grab(t.sleepLock);
            ThreadHelper.cond_signal(t.sleepCond); // In case is waiting for stop or ponderhit
            while (!t.do_sleep)
            {
                ThreadHelper.cond_wait(sleepCond, t.sleepLock);
            }
            ThreadHelper.lock_release(t.sleepLock);
        }

        // ThreadsManager::set_timer() is used to set the timer to trigger after msec
        // milliseconds. If msec is 0 then timer is stopped.
        internal static void set_timer(int msec)
        {
            ThreadHelper.lock_grab(timer.sleepLock);
            timer.maxPly = msec;
            ThreadHelper.cond_signal(timer.sleepCond); // Wake up and restart the timer
            ThreadHelper.lock_release(timer.sleepLock);
        }

        // wake_up() is called before a new search to start the threads that are waiting
        // on the sleep condition and to reset maxPly. When useSleepingThreads is set
        // threads will be woken up at split time.
        internal static void wake_up()
        {
            for (var i = 0; i < size(); i++)
            {
                threads[i].maxPly = 0;
                threads[i].do_sleep = false;

                if (!useSleepingThreads)
                {
                    threads[i].wake_up();
                }
            }
        }

        // sleep() is called after the search finishes to ask all the threads but the
        // main one to go waiting on a sleep condition.
        internal static void sleep()
        {
            for (var i = 1; i < size(); i++) // Main thread will go to sleep by itself
            {
                threads[i].do_sleep = true; // to avoid a race with start_searching()
            }
        }
    };

    internal static class ThreadHelper
    {
        //#  define lock_grab(x) EnterCriticalSection(x)
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void lock_grab(object Lock)
        {
            Monitor.Enter(Lock);
        }

        //#  define lock_release(x) LeaveCriticalSection(x)
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void lock_release(object Lock)
        {
            Monitor.Exit(Lock);
        }

        //#  define cond_signal(x) SetEvent(*x)
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void cond_signal(object sleepCond)
        {
            lock (sleepCond)
            {
                Monitor.Pulse(sleepCond);
            }
        }

        //#  define cond_wait(x,y) { lock_release(y); WaitForSingleObject(*x, INFINITE); lock_grab(y); }
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void cond_wait(object sleepCond, object sleepLock)
        {
            lock_release(sleepLock);
            lock (sleepCond)
            {
                Monitor.Wait(sleepCond);
            }
            lock_grab(sleepLock);
        }

        //#  define cond_timedwait(x,y,z) { lock_release(y); WaitForSingleObject(*x,z); lock_grab(y); }
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void cond_timedwait(object sleepCond, object sleepLock, int msec)
        {
            lock_release(sleepLock);
            lock (sleepCond)
            {
                Monitor.Wait(sleepCond, msec);
            }
            lock_grab(sleepLock);
        }
    }
}