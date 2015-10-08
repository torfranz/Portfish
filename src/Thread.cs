using Key = System.UInt64;
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
        internal SplitPoint parentSplitPoint;

        internal Position pos;

        internal int depth;

        internal int beta;

        internal int nodeType;

        internal Thread master;

        internal int threatMove;

        // Const pointers to shared data
        internal MovePicker movePicker;

        internal Stack[] ss;

        internal int ssPos;

        // Shared data
        internal readonly object Lock = new object();

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

    /*
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
    */

    internal class Thread
    {
        internal readonly MaterialTable materialTable = new MaterialTable();

        internal readonly PawnTable pawnTable = new PawnTable();

        internal Position activePosition;

        internal readonly object sleepCond = new object();

        internal readonly object sleepLock = new object();

        internal readonly SplitPoint[] splitPoints = new SplitPoint[Constants.MAX_SPLITPOINTS_PER_THREAD];

        internal volatile SplitPoint activeSplitPoint;

        internal volatile bool do_exit;

        internal int idx;

        internal int maxPly;

        internal volatile bool searching;

        internal volatile int splitPointsSize;

        internal Thread(ManualResetEvent initEvent)
        {
            this.searching = this.do_exit = false;
            this.maxPly = this.splitPointsSize = 0;
            this.activeSplitPoint = null;
            this.activePosition = null;
            this.idx = Threads.size();

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
            this.idle_loop(initEvent);
        }

        internal void base_idle_loop(ManualResetEvent initEvent)
        {
            var this_sp = this.splitPointsSize > 0 ? this.activeSplitPoint : null;
            Debug.Assert(this_sp == null || (this_sp.master == this && this.searching));

            if (initEvent != null)
            {
                // Signal done
                initEvent.Set();
            }

            // If this thread is the master of a split point and all slaves have
            // finished their work at this split point, return from the idle loop.
            while ((this_sp == null) || (this_sp.slavesMask != 0))
            {
                // If we are not searching, wait for a condition to be signaled
                // instead of wasting CPU time polling for work.
                while (this.do_exit || (!this.searching && Threads.sleepWhileIdle))
                {
                    if (this.do_exit)
                    {
                        Debug.Assert(this_sp == null);
                        return;
                    }

                    // Grab the lock to avoid races with Thread::notify_one()
                    ThreadHelper.lock_grab(this.sleepLock);

                    // If we are master and all slaves have finished don't go to sleep
                    if ((this_sp != null) && (this_sp.slavesMask == 0))
                    {
                        ThreadHelper.lock_release(this.sleepLock);
                        break;
                    }

                    // Do sleep after retesting sleep conditions under lock protection, in
                    // particular we need to avoid a deadlock in case a master thread has,
                    // in the meanwhile, allocated us and sent the notify_one() call before we
                    // had the chance to grab the lock.
                    if (!this.searching && !this.do_exit)
                    {
                        ThreadHelper.cond_wait(this.sleepCond, this.sleepLock);
                    }

                    ThreadHelper.lock_release(this.sleepLock);
                }

                // If this thread has been assigned work, launch a search
                if (this.searching)
                {
                    Debug.Assert( /*!this.is_finished &&*/ !this.do_exit);

                    ThreadHelper.lock_grab(Threads.splitLock);

                    Debug.Assert(this.searching);
                    var sp = this.activeSplitPoint;

                    ThreadHelper.lock_release(Threads.splitLock);

                    var ls = LoopStackBroker.GetObject();
                    var ss = ls.ss;
                    var ssPos = 0;

                    var pos = PositionBroker.GetObject();
                    pos.copy(sp.pos, this);

                    Array.Copy(sp.ss, sp.ssPos - 1, ss, ssPos, 4);
                    ss[ssPos + 1].sp = sp;

                    ThreadHelper.lock_grab(sp.Lock);

                    Debug.Assert(activePosition == null);

                    activePosition = pos;

                    switch (sp.nodeType)
                    {
                        case NodeTypeC.Root:
                            {
                                Search.search(NodeTypeC.SplitPointRoot, pos, ss, ssPos + 1, sp.alpha, sp.beta, sp.depth);
                            }
                            break;
                        case NodeTypeC.PV:
                            {
                                Search.search(NodeTypeC.SplitPointPV, pos, ss, ssPos + 1, sp.alpha, sp.beta, sp.depth);
                            }
                            break;
                        case NodeTypeC.NonPV:
                            {
                                Search.search(
                                    NodeTypeC.SplitPointNonPV,
                                    pos,
                                    ss,
                                    ssPos + 1,
                                    sp.alpha,
                                    sp.beta,
                                    sp.depth);
                            }
                            break;
                        default:
                            {
                                Debug.Assert(false);
                            }
                            break;
                    }

                    Debug.Assert(this.searching);

                    this.searching = false;
                    activePosition = null;
#if ACTIVE_REPARENT
                    sp.allSlavesRunning = false;
#endif
                    sp.slavesMask &= ~(1UL << this.idx);
                    sp.nodes += pos.nodes;

                    // Wake up master thread so to allow it to return from the idle loop in
                    // case we are the last slave of the split point.
                    if (Threads.sleepWhileIdle && this != sp.master && !sp.master.searching && sp.slavesMask != 0)
                    {
                        Debug.Assert(!sp.master.searching);
                        sp.master.notify_one();
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
                        int spCnt = th.splitPointsSize;
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
                                && spCnt == th.splitPointsSize
                                && !th.cutoff_occurred()
                                && latest.allSlavesRunning
                                && Utils.more_than_one(latest.slavesMask))
                            {
                                latest.slavesMask |= 1UL << idx;
                                activeSplitPoint = latest;
                                searching = true;
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

        /// Thread::idle_loop() is where the thread is parked when it has no work to do
        internal virtual void idle_loop(ManualResetEvent initEvent)
        {
            this.base_idle_loop(initEvent);
        }

        internal void exit()
        {
            this.do_exit = true; // Search must be already finished
            this.notify_one();
        }

        // Thread::notify_one() wakes up the thread, normally at split time
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal void notify_one()
        {
            ThreadHelper.lock_grab(this.sleepLock);
            ThreadHelper.cond_signal(this.sleepCond);
            ThreadHelper.lock_release(this.sleepLock);
        }

        // Thread::cutoff_occurred() checks whether a beta cutoff has occurred in the
        // current active split point, or in some ancestor of the split point.
        internal bool cutoff_occurred()
        {
            for (var sp = this.activeSplitPoint; sp != null; sp = sp.parentSplitPoint)
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
            if (this.searching)
            {
                return false;
            }

            // Make a local copy to be sure doesn't become zero under our feet while
            // testing next condition and so leading to an out of bound access.
            var size = this.splitPointsSize;

            // No active split points means that the thread is available as a slave for any
            // other thread otherwise apply the "helpful master" concept if possible.
            return (size == 0) || ((this.splitPoints[size - 1].slavesMask & (1UL << master.idx)) != 0);
        }

        #region Loops

        #endregion
    }

    // MainThread and TimerThread are sublassed from Thread to charaterize the two
    // special threads: the main one and the recurring timer.

    internal sealed class TimerThread : Thread
    {
        internal int msec;

        internal TimerThread(ManualResetEvent initEvent)
            : base(initEvent)
        {
        }

        // Thread::timer_loop() is where the timer thread waits maxPly milliseconds and
        // then calls do_timer_event(). If maxPly is 0 thread sleeps until is woken up.
        internal override void idle_loop(ManualResetEvent initEvent)
        {
            // Signal done
            initEvent.Set();

            while (!this.do_exit)
            {
                ThreadHelper.lock_grab(this.sleepLock);
                if (!this.do_exit)
                {
                    ThreadHelper.cond_timedwait(
                        this.sleepCond,
                        this.sleepLock,
                        this.msec != 0 ? this.msec : Constants.INT_MAX);
                }

                ThreadHelper.lock_release(this.sleepLock);

                if (this.msec != 0)
                {
                    Search.check_time();
                }
            }
        }
    }

    internal sealed class MainThread : Thread
    {
        internal volatile bool thinking;

        internal MainThread(ManualResetEvent initEvent)
            : base(initEvent)
        {
            this.thinking = true; // Avoid a race with start_thinking()
        }

        // Thread::main_loop() is where the main thread is parked waiting to be started
        // when there is a new search. Main thread will launch all the slave threads.
        internal override void idle_loop(ManualResetEvent initEvent)
        {
            // Initialize the TT here
            var ttSize = uint.Parse(OptionMap.Instance["Hash"].v);
            if (TT.hashMask != ttSize)
            {
                TT.set_size(ttSize);
            }

            // Signal done
            initEvent.Set();

            while (true)
            {
                ThreadHelper.lock_grab(this.sleepLock);

                this.thinking = false;
                this.searching = false;

                while (!this.thinking && !this.do_exit)
                {
                    ThreadHelper.cond_signal(Threads.sleepCond); // Wake up UI thread if needed
                    ThreadHelper.cond_wait(this.sleepCond, this.sleepLock);
                }

                ThreadHelper.lock_release(this.sleepLock);

                if (this.do_exit)
                {
                    return;
                }

                this.searching = true;

                Search.think(); // This is the search entry point

                Debug.Assert(this.searching);
                this.searching = false;
            }
        }
    }

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

        internal static TimerThread timer;

        internal static readonly object splitLock = new object();

        internal static readonly object sleepCond = new object();

        internal static int minimumSplitDepth;

        internal static int maxThreadsPerSplitPoint;

        internal static bool sleepWhileIdle;

        //internal static bool use_sleeping_threads() { return useSleepingThreads; }
        internal static int size()
        {
            return threads.Count;
        }

        internal static Thread thread(int index)
        {
            return threads[index];
        }

        internal static MainThread main_thread()
        {
            return (MainThread)threads[0];
        }

        internal static TimerThread timer_thread()
        {
            return timer;
        }

        // read_uci_options() updates internal threads parameters from the corresponding
        // UCI options and creates/destroys threads to match the requested number. Thread
        // objects are dynamically allocated to avoid creating in advance all possible
        // threads, with included pawns and material tables, if only few are used.
        internal static void read_uci_options(ManualResetEvent[] initEvents)
        {
            maxThreadsPerSplitPoint = int.Parse(OptionMap.Instance["Max Threads per Split Point"].v);
            minimumSplitDepth = int.Parse(OptionMap.Instance["Min Split Depth"].v) * DepthC.ONE_PLY;

            var requested = int.Parse(OptionMap.Instance["Threads"].v);
            var current = 0;

            Debug.Assert(requested > 0);

            while (size() < requested)
            {
                if (initEvents == null)
                {
                    threads.Add(new Thread(null));
                }
                else
                {
                    threads.Add(new Thread(initEvents[current + 2]));
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

        // init() is called at startup to create and launch requested threads, that will
        // go immediately to sleep due to 'sleepWhileIdle' set to true. We cannot use
        // engine at this point due to allocation of Endgames in Thread c'tor.
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
            timer = new TimerThread(initEvents[0]);
            threads.Add(new MainThread(initEvents[1]));
            sleepWhileIdle = true;
            read_uci_options(initEvents);
        }

        // exit() cleanly terminates the threads before the program exits
        internal static void exit()
        {
            for (var i = 0; i < size(); i++)
            {
                threads[i].exit();
            }

            timer.exit();
        }

        // available_slave() tries to find an idle thread which is available as
        // a slave for the thread with threadID 'master'.
        internal static Thread available_slave(Thread master)
        {
            for (var i = 0; i < size(); i++)
            {
                if (threads[i].is_available_to(master))
                {
                    return threads[i];
                }
            }

            return null;
        }

        // split() does the actual work of distributing the work at a node between
        // several available threads. If it does not succeed in splitting the node
        // (because no idle threads are available), the function immediately returns.
        // If splitting is possible, a SplitPoint object is initialized with all the
        // data that must be copied to the helper threads and then helper threads are
        // told that they have been assigned work. This will cause them to instantly
        // leave their idle loops and call search(). When all threads have returned from
        // search() then split() returns.
        internal static void split(
            bool Fake,
            Position pos,
            Stack[] ss,
            int ssPos,
            int alpha,
            int beta,
            ref int bestValue,
            ref int bestMove,
            int depth,
            int threatMove,
            int moveCount,
            MovePicker movePicker,
            int nodeType)
        {
            Debug.Assert(bestValue <= alpha && alpha < beta && beta <= ValueC.VALUE_INFINITE);
            Debug.Assert(pos.pos_is_ok());
            Debug.Assert(bestValue > -ValueC.VALUE_INFINITE);
            Debug.Assert(depth > DepthC.DEPTH_ZERO);

            var thisThread = pos.this_thread();

            Debug.Assert(thisThread.searching);
            Debug.Assert(thisThread.splitPointsSize < Constants.MAX_SPLITPOINTS_PER_THREAD);

            // Pick the next available split point from the split point stack
            var sp = thisThread.splitPoints[thisThread.splitPointsSize];

            sp.parentSplitPoint = thisThread.activeSplitPoint;
            sp.master = thisThread;
            sp.cutoff = false;
            sp.slavesMask = 1UL << thisThread.idx;
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
            sp.movePicker = movePicker;
            sp.moveCount = moveCount;
            sp.pos = pos;
            sp.nodes = 0;
            sp.ss = ss;
            sp.ssPos = ssPos;

            // Try to allocate available threads and ask them to start searching setting
            // 'searching' flag. This must be done under lock protection to avoid concurrent
            // allocation of the same slave by another master.
            ThreadHelper.lock_grab(splitLock);
            ThreadHelper.lock_grab(sp.Lock);

            thisThread.splitPointsSize++;
            thisThread.activeSplitPoint = sp;
            thisThread.activePosition = null;

            var slavesCnt = 1; // Master is always included
            Thread slave;
            while ((slave = Threads.available_slave(thisThread)) != null
                && ++slavesCnt <= Threads.maxThreadsPerSplitPoint && !Fake)
            {
                sp.slavesMask |= 1UL << slave.idx;
                slave.activeSplitPoint = sp;
                slave.searching = true; // Slave leaves idle_loop()

                slave.notify_one(); // Could be sleeping
            }

            ThreadHelper.lock_release(sp.Lock);
            ThreadHelper.lock_release(splitLock);

            // Everything is set up. The master thread enters the idle loop, from which
            // it will instantly launch a search, because its searching flag is set.
            // We pass the split point as a parameter to the idle loop, which means that
            // the thread will return from the idle loop when all slaves have finished
            // their work at this split point.
            if (slavesCnt > 1 || Fake)
            {
                thisThread.base_idle_loop(null); // Force a call to base class idle_loop()

                // In helpful master concept a master can help only a sub-tree of its split
                // point, and because here is all finished is not possible master is booked.
                Debug.Assert(!thisThread.searching);
                Debug.Assert(thisThread.activePosition == null);
            }

            // We have returned from the idle loop, which means that all threads are
            // finished. Note that setting searching and decreasing activeSplitPoints is
            // done under lock protection to avoid a race with Thread::is_available_to().
            ThreadHelper.lock_grab(splitLock);
            ThreadHelper.lock_grab(sp.Lock); // To protect sp->nodes

            thisThread.searching = true;
            thisThread.splitPointsSize--;
            thisThread.activeSplitPoint = sp.parentSplitPoint;
            thisThread.activePosition = pos;
            pos.nodes += sp.nodes;
            bestMove = sp.bestMove;
            bestValue = sp.bestValue;

            ThreadHelper.lock_release(sp.Lock);
            ThreadHelper.lock_release(splitLock);
        }

        // start_thinking() wakes up the main thread sleeping in MainThread::idle_loop()
        // so to start a new search, then returns immediately.
        internal static void start_thinking(Position pos, LimitsType limits, List<int> searchMoves)
        {
            wait_for_think_finished();

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

            main_thread().thinking = true;
            main_thread().notify_one();
        }

        // ThreadsManager::wait_for_think_finished() waits for main thread to go to
        // sleep, this means search is finished. Then returns.
        internal static void wait_for_think_finished()
        {
            var t = main_thread();
            ThreadHelper.lock_grab(t.sleepLock);
            ThreadHelper.cond_signal(t.sleepCond); // In case is waiting for stop or ponderhit
            while (t.thinking)
            {
                ThreadHelper.cond_wait(sleepCond, t.sleepLock);
            }
            ThreadHelper.lock_release(t.sleepLock);
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