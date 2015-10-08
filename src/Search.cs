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

namespace Portfish
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Runtime.CompilerServices;
    using System.Text;

    /// The Stack struct keeps track of the information we need to remember from
    /// nodes shallower and deeper in the tree during the search. Each search thread
    /// has its own array of Stack objects, indexed by the current ply.Start with a small aspiration window and, in case of fail high/low,
             // research with bigger wind
    internal struct Stack
    {
        internal SplitPoint sp;

        internal int ply;

        internal int currentMove;

        internal int excludedMove;

        internal int killers0;

        internal int killers1;

        internal int reduction;

        internal int staticEval;

        internal int evalMargin;

        internal int skipNullMove;
    };

    /// The LimitsType struct stores information sent by GUI about available time
    /// to search the current move, maximum depth/time, if we are in analysis mode
    /// or if we have to ponder while is our opponent's side to move.
    internal sealed class LimitsType
    {
        internal readonly int[] inc = new int[ColorC.COLOR_NB];

        internal readonly int[] time = new int[ColorC.COLOR_NB];

        internal int movesToGo, movetime, depth, nodes, infinite, mate;

        internal bool ponder;

        internal LimitsType()
        {
            this.time[0] = 0;
            this.time[1] = 1;
            this.inc[0] = 0;
            this.inc[1] = 0;
            this.movesToGo = 0;
            this.movetime = 0;
            this.depth = 0;
            this.nodes = 0;
            this.mate = 0;
            this.infinite = 0;
            this.ponder = false;
        }

        internal bool use_time_management()
        {
            return (this.mate + this.movetime + this.depth + this.nodes + this.infinite) == 0;
        }
    };

    // RootMove struct is used for moves at the root of the tree. For each root
    // move we store a score, a node count, and a PV (really a refutation in the
    // case of moves which fail low). Score is normally set at -VALUE_INFINITE for
    // all non-pv moves.
    internal sealed class RootMove
    {
        internal readonly List<int> pv = new List<int>();

        internal int prevScore;

        internal int score;

        internal RootMove(int m)
        {
            this.score = this.prevScore = -ValueC.VALUE_INFINITE;
            this.pv.Add(m);
            this.pv.Add(MoveC.MOVE_NONE);
        }

        /// RootMove::extract_pv_from_tt() builds a PV by adding moves from the TT table.
        /// We consider also failing high nodes and not only BOUND_EXACT nodes so to
        /// allow to always have a ponder move even when we fail high at root, and a
        /// long PV to print that is important for position analysis.
        internal void extract_pv_from_tt(Position pos)
        {
            var sia = StateInfoArrayBroker.GetObject();

            var stPos = 0;
            TTEntry tte;
            bool tteHasValue;
            var ply = 0;
            var m = this.pv[0];

            this.pv.Clear();
            
            uint ttePos = 0;
            do
            {
                this.pv.Add(m);

                Debug.Assert(pos.move_is_legal(pv[ply]));
                pos.do_move(pv[ply++], sia.state[stPos++]);
                tteHasValue = TT.probe(pos.key(), ref ttePos, out tte);
            } while (tteHasValue
                    && pos.is_pseudo_legal(m = tte.move()) // Local copy, TT could change
                    && pos.pl_move_is_legal(m, pos.pinned_pieces())
                    && ply < Constants.MAX_PLY
                    && (!pos.is_draw(false) || ply < 2));
            ;
            
            this.pv.Add(MoveC.MOVE_NONE); // Must be zero-terminating

            while (ply != 0)
            {
                pos.undo_move(this.pv[--ply]);
            }

            StateInfoArrayBroker.Free();
        }

        // insert_pv_in_tt() is called at the end of a search iteration, and inserts
        // the PV back into the TT. This makes sure the old PV moves are searched
        // first, even if the old TT entries have been overwritten.
        internal void insert_pv_in_tt(Position pos)
        {
            var sia = StateInfoArrayBroker.GetObject();

            var stPos = 0;
            TTEntry tte;
            bool tteHasValue;
            
            int v, m = 0;
            var ply = 0;
            uint ttePos = 0;
            
            do
            {
                tteHasValue = TT.probe(pos.key(), ref ttePos, out tte);

                if ((!tteHasValue) || tte.move() != this.pv[ply]) // Don't overwrite existing correct entries
                {
                    if (pos.in_check())
                    {
                        v = m = ValueC.VALUE_NONE;
                    }
                    else
                    {
                        v = Evaluate.do_evaluate(false, pos, ref m);
                    }

                    TT.store(pos.key(), ValueC.VALUE_NONE, Bound.BOUND_NONE, DepthC.DEPTH_NONE, this.pv[ply], v, m);
                }

                Debug.Assert(pos.move_is_legal(pv[ply]));
                pos.do_move(this.pv[ply++], sia.state[stPos++]);
            }
            while (this.pv[ply] != MoveC.MOVE_NONE);

            while (ply != 0)
            {
                pos.undo_move(this.pv[--ply]);
            }

            StateInfoArrayBroker.Free();
        }
    };

    // When playing with strength handicap choose best move among the MultiPV set
    // using a statistical rule dependent on 'level'. Idea by Heinz van Saanen.
    internal class Skill : IDisposable
    {
        internal Skill(int l)
        {
            level = l;
        }

        public void Dispose()
        {
            if (enabled()) // Swap best PV line with the sub-optimal one
            {
                var bestpos = Search.find(Search.RootMoves, 0, Search.RootMoves.Count, best != 0 ? best : pick_move());
                var temp = Search.RootMoves[0];
                Search.RootMoves[0] = Search.RootMoves[bestpos];
                Search.RootMoves[bestpos] = temp;
            }
        }

        internal bool enabled()
        {
            return level < 20;
        }

        internal bool time_to_pick(int depth)
        {
            return depth == 1 + level;
        }

        internal Move pick_move()
        {
            // PRNG sequence should be not deterministic
            for (var i = Math.Abs(DateTime.Now.Millisecond % 50); i > 0; i--)
            {
                Search.rk.rand();
            }

            // RootMoves are already sorted by score in descending order
            var variance = Math.Min(Search.RootMoves[0].score - Search.RootMoves[Search.PVSize - 1].score, Constants.PawnValueMidgame);
            var weakness = 120 - 2 * level;
            var max_s = -ValueC.VALUE_INFINITE;
            best = MoveC.MOVE_NONE;

            // Choose best move. For each move score we add two terms both dependent on
            // weakness, one deterministic and bigger for weaker moves, and one random,
            // then we choose the move with the resulting highest score.
            for (var i = 0; i < Search.PVSize; i++)
            {
                var s = Search.RootMoves[i].score;

                // Don't allow crazy blunders even at very low skills
                if (i > 0 && Search.RootMoves[i - 1].score > s + 2 * Constants.PawnValueMidgame)
                {
                    break;
                }

                // This is our magic formula
                s += (weakness * (Search.RootMoves[0].score - s) + variance * (int)(Search.rk.rand() % (ulong)weakness)) / 128;

                if (s > max_s)
                {
                    max_s = s;
                    best = Search.RootMoves[i].pv[0];
                }
            }
            return best;
        }

        int level;
        Move best = MoveC.MOVE_NONE;
    }

    internal static class Search
    {
        #region SignalsType

        /// The SignalsType struct stores volatile flags updated during the search
        /// typically in an async fashion, for instance to stop the search by the GUI.
        internal static volatile bool SignalsStopOnPonderhit, SignalsFirstRootMove, SignalsStop, SignalsFailedLowAtRoot;

        #endregion

        internal static LimitsType Limits = new LimitsType();

        internal static readonly List<RootMove> RootMoves = new List<RootMove>();

        internal static Stopwatch SearchTime = new Stopwatch();

        internal static Stopwatch lastInfoTime = new Stopwatch();

        internal static readonly Position RootPos = new Position();

        internal static readonly RKISS rk = new RKISS();

        // Dynamic razoring margin based on depth
        internal static int razor_margin(int d)
        {
            return (0x200 + 0x10 * d);
        }

        // Futility lookup tables (initialized at startup) and their access functions
        internal static readonly int[][] FutilityMargins = new int[16][]; // [depth][moveNumber] 16, 64

        internal static readonly int[] FutilityMoveCounts = new int[32]; // [depth]

        // Reduction lookup tables (initialized at startup) and their access function
        private static readonly sbyte[][][] Reductions = new sbyte[2][][]; // [pv][depth][moveNumber] 2, 64, 64

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int futility_margin(int d, int mn)
        {
            return d < 7 * DepthC.ONE_PLY
                       ? FutilityMargins[Math.Max(d, 1)][Math.Min(mn, 63)]
                       : 2 * ValueC.VALUE_INFINITE;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int reduction(bool PvNode, int d, int mn)
        {
            return Reductions[PvNode ? 1 : 0][Math.Min((d) / DepthC.ONE_PLY, 63)][Math.Min(mn, 63)];
        }

        // This is the minimum interval in msec between two check_time() calls
        private const int TimerResolution = 5;

        /// Namespace variables
        internal static int PVSize, PVIdx; // was UInt64

        private static int BestMoveChanges;

        private static readonly History H = new History();

        private static int[] DrawValue = new int[ColorC.COLOR_NB];

        internal static int RootColor;

        internal static void init()
        {
            SearchTime.Start();
            lastInfoTime.Start();

            int d; // depth (ONE_PLY == 2)
            int hd; // half depth (ONE_PLY == 1)
            int mc; // moveCount

            // Init reductions array
            Reductions[0] = new sbyte[64][];
            Reductions[1] = new sbyte[64][];
            for (hd = 0; hd < 64; hd++)
            {
                Reductions[0][hd] = new sbyte[64];
                Reductions[1][hd] = new sbyte[64];
                if (hd != 0)
                {
                    for (mc = 1; mc < 64; mc++)
                    {
                        var pvRed = Math.Log(hd) * Math.Log(mc) / 3.0;
                        var nonPVRed = 0.33 + Math.Log(hd) * Math.Log(mc) / 2.25;
                        Reductions[1][hd][mc] = (sbyte)(pvRed >= 1.0 ? Math.Floor(pvRed * DepthC.ONE_PLY) : 0);
                        Reductions[0][hd][mc] = (sbyte)(nonPVRed >= 1.0 ? Math.Floor(nonPVRed * DepthC.ONE_PLY) : 0);
                    }
                }
            }

            // Init futility margins array
            for (d = 0; d < 16; d++)
            {
                FutilityMargins[d] = new int[64];
                if (d != 0)
                {
                    for (mc = 0; mc < 64; mc++)
                    {
                        FutilityMargins[d][mc] = (112 * (int)(Math.Log((double)(d * d) / 2) / Math.Log(2.0) + 1.001)
                                                  - 8 * mc + 45);
                    }
                }
            }

            // Init futility move count array
            for (d = 0; d < 32; d++)
            {
                FutilityMoveCounts[d] = (int)(3.001 + 0.25 * Math.Pow(d, 2.0));
            }
        }

        /// Search::perft() is our utility to verify move generation. All the leaf nodes
        /// up to the given depth are generated and counted and the sum returned.
        internal static long perft(Position pos, int depth)
        {
            var st = new StateInfo();
            long cnt = 0;

            var mlist = MListBroker.GetObject();
            mlist.pos = 0;
            Movegen.generate_legal(pos, mlist.moves, ref mlist.pos);

            // At the last ply just return the number of moves (leaf nodes)
            if (depth == DepthC.ONE_PLY)
            {
                var retval = mlist.pos;
                MListBroker.Free();
                return retval;
            }

            var ci = CheckInfoBroker.GetObject();
            ci.CreateCheckInfo(pos);
            for (var i = 0; i < mlist.pos; ++i)
            {
                var ms = mlist.moves[i];
                pos.do_move(ms.move, st, ci, pos.move_gives_check(ms.move, ci));
                cnt += perft(pos, depth - DepthC.ONE_PLY);
                pos.undo_move(ms.move);
            }
            CheckInfoBroker.Free();
            MListBroker.Free();
            return cnt;
        }

        // Search::validmoves() will return the list of all valid moves for a 'square' in UCI notation - all valid moves for the piece occupying that square
        // The list will be empty if no square is given or there is no piece on that square or the piece have no possible moves
        internal static void validmoves(Position pos, Stack<string> stack)
        {
            if (stack.Count > 0)
            {
                var squareFromString = stack.Pop();

                var st = new StateInfo();
                var mlist = MListBroker.GetObject();
                mlist.pos = 0;
                Movegen.generate_legal(pos, mlist.moves, ref mlist.pos);

                var firstOne = true;
                for (var i = 0; i < mlist.pos; ++i)
                {
                    var ms = mlist.moves[i];
                    var m = ms.move;
                    var from = ((m >> 6) & 0x3F);
                    if (Utils.square_to_string(from) == squareFromString)
                    {
                        if (!firstOne)
                        {
                            Plug.Write(" ");
                        }
                        Plug.Write(Utils.move_to_uci(m, false));
                        firstOne = false;
                    }
                }
                MListBroker.Free();
            }
            Plug.Write(Constants.endl);
        }

        /// Search::think() is the external interface to Stockfish's search, and is
        /// called by the main thread when the program receives the UCI 'go' command. It
        /// searches from RootPos and at the end prints the "bestmove" to output.
        internal static void think()
        {
            //SearchTime.Restart();
            RootColor = RootPos.sideToMove;
            TimeMgr.init(Limits, RootPos.startpos_ply_counter(), RootColor);

            MaterialEntry e;
            RootPos.this_thread().materialTable.probe(RootPos, out e);
            
            if (RootMoves.Count == 0)
            {
                RootMoves.Add(new RootMove(MoveC.MOVE_NONE));

                Plug.Write("info depth 0 score ");
                Plug.Write(score_to_uci(RootPos.in_check() ? -ValueC.VALUE_MATE : ValueC.VALUE_DRAW));
                Plug.Write(Constants.endl);
                
                goto finalize;
            }

            if ((bool.Parse(OptionMap.Instance["OwnBook"].v)) && (Limits.infinite == 0) && (Limits.mate == 0))
            {
                var bookMove = PolyglotBook.probe(
                    RootPos,
                    OptionMap.Instance["Book File"].v,
                    bool.Parse(OptionMap.Instance["Best Book Move"].v));
                if ((bookMove != 0) && Utils.existRootMove(RootMoves, bookMove))
                {
                    var bestpos = find(RootMoves, 0, RootMoves.Count, bookMove);
                    var temp = RootMoves[0];
                    RootMoves[0] = RootMoves[bestpos];
                    RootMoves[bestpos] = temp;
                    goto finalize;
                }
            }

            if (int.Parse(OptionMap.Instance["Contempt Factor"].v) != 0 && !bool.Parse(OptionMap.Instance["UCI_AnalyseMode"].v))
            {
                int cf = int.Parse(OptionMap.Instance["Contempt Factor"].v) * Constants.PawnValueMidgame / 100; // In centipawns
                cf = cf * e.game_phase() / PhaseC.PHASE_MIDGAME; // Scale down with phase
                DrawValue[RootColor] = ValueC.VALUE_DRAW - cf;
                DrawValue[1 - RootColor] = ValueC.VALUE_DRAW + cf;
            }
            else
            {
                DrawValue[ColorC.WHITE] = DrawValue[ColorC.BLACK] = ValueC.VALUE_DRAW;
            }

            var ttSize = uint.Parse(OptionMap.Instance["Hash"].v);
            if (TT.hashMask != ttSize)
            {
                TT.set_size(ttSize);
            }

            // Reset and wake up the threads
            for (var i = 0; i < Threads.size(); i++)
            {
                Threads.threads[i].maxPly = 0;
            }

            //TODO: using commented code engine does not take input
            Threads.sleepWhileIdle = false;//bool.Parse(OptionMap.Instance["Use Sleeping Threads"].v);

            // Set best timer interval to avoid lagging under time pressure. Timer is
            // used to check for remaining available thinking time.
            Threads.timer_thread().msec = /* Hack: we use maxPly to set timer interval */
                    Limits.use_time_management() ? Math.Min(100, Math.Max(TimeMgr.available_time() / 16, TimerResolution)) :
                             (Limits.nodes != 0) ? 2 * TimerResolution : 100;

            Threads.timer_thread().notify_one(); // Wake up the recurring timer

            // We're ready to start searching. Call the iterative deepening loop function
            id_loop(RootPos);

            // Stop timer and send all the slaves to sleep, if not already sleeping
            Threads.timer_thread().msec = 0; // Stop the timer

            Threads.sleepWhileIdle = true; // Send idle threads to sleep

            finalize:

            // When we reach max depth we arrive here even without Signals.stop is raised,
            // but if we are pondering or in infinite search, we shouldn't print the best
            // move before we are told to do so.
            if (!SignalsStop && (Limits.ponder || (Limits.infinite != 0)))
            {
                // Thread::wait_for() set the thread to sleep until condition 'b' turns true
                SignalsStopOnPonderhit = true;
                ThreadHelper.lock_grab(RootPos.this_thread().sleepLock);

                while (!SignalsStop)
                {
                    ThreadHelper.cond_wait(RootPos.this_thread().sleepCond, RootPos.this_thread().sleepLock);
                }

                ThreadHelper.lock_release(RootPos.this_thread().sleepLock);
            }

            // Best move could be MOVE_NONE when searching on a stalemate position
            Plug.Write("bestmove ");
            Plug.Write(Utils.move_to_uci(RootMoves[0].pv[0], RootPos.chess960));
            Plug.Write(" ponder ");
            Plug.Write(Utils.move_to_uci(RootMoves[0].pv[1], RootPos.chess960));
            Plug.Write(Constants.endl);
        }

        // id_loop() is the main iterative deepening loop. It calls search() repeatedly
        // with increasing depth until the allocated thinking time has been consumed,
        // user stops the search, or the maximum search depth is reached.
        private static void id_loop(Position pos)
        {
            var ls = LoopStackBroker.GetObject();
            var ss = ls.ss;

            var ssPos = 0;
            int depth, prevBestMoveChanges;
            int bestValue, alpha, beta, delta;
            var bestMoveNeverChanged = true;
            
            depth = BestMoveChanges = 0;
            bestValue = delta = -ValueC.VALUE_INFINITE;
            ss[ssPos].currentMove = MoveC.MOVE_NULL;
            TT.new_search();
            H.clear();

            PVSize = int.Parse(OptionMap.Instance["MultiPV"].v);
            using (var skill = new Skill(int.Parse(OptionMap.Instance["Skill Level"].v)))
            {

                // Do we have to play with skill handicap? In this case enable MultiPV search
                // that we will use behind the scenes to retrieve a set of possible moves.
                if (skill.enabled() && PVSize < 4)
                {
                    PVSize = 4;
                }
                
                PVSize = Math.Min(PVSize, RootMoves.Count);

                // Iterative deepening loop until requested to stop or target depth reached
                while (++depth <= Constants.MAX_PLY && !SignalsStop && ((Limits.depth == 0) || depth <= Limits.depth))
                {
                    // Save last iteration's scores before first PV line is searched and all
                    // the move scores but the (new) PV are set to -VALUE_INFINITE.
                    for (var i = 0; i < RootMoves.Count; i++)
                    {
                        RootMoves[i].prevScore = RootMoves[i].score;
                    }

                    prevBestMoveChanges = BestMoveChanges; // Only sensible when PVSize == 1
                    BestMoveChanges = 0;

                    // MultiPV loop. We perform a full root search for each PV line
                    for (PVIdx = 0; PVIdx < PVSize; PVIdx++)
                    {
                        // Set aspiration window default width
                        if (depth >= 5 && Math.Abs(RootMoves[PVIdx].prevScore) < ValueC.VALUE_KNOWN_WIN)
                        {
                            delta = 16;
                            alpha = RootMoves[PVIdx].prevScore - delta;
                            beta = RootMoves[PVIdx].prevScore + delta;
                        }
                        else
                        {
                            alpha = -ValueC.VALUE_INFINITE;
                            beta = ValueC.VALUE_INFINITE;
                        }

                        // Start with a small aspiration window and, in case of fail high/low,
                        // research with bigger window until not failing high/low anymore.
                        while (true)
                        {
                            // Search starts from ss+1 to allow referencing (ss-1). This is
                            // needed by update gains and ss copy when splitting at Root.
                            bestValue = search(NodeTypeC.Root, pos, ss, ssPos + 1, alpha, beta, depth * DepthC.ONE_PLY);

                            // Bring to front the best move. It is critical that sorting is
                            // done with a stable algorithm because all the values but the first
                            // and eventually the new best one are set to -VALUE_INFINITE and
                            // we want to keep the same order for all the moves but the new
                            // PV that goes to the front. Note that in case of MultiPV search
                            // the already searched PV lines are preserved.
                            Utils.sort(RootMoves, PVIdx, RootMoves.Count);
                            //sort<RootMove>(RootMoves.begin() + PVIdx, RootMoves.end());

                            // Write PV back to transposition table in case the relevant
                            // entries have been overwritten during the search.
                            for (var i = 0; i <= PVIdx; i++)
                            {
                                RootMoves[i].insert_pv_in_tt(pos);
                            }

                            // If search has been stopped return immediately. Sorting and
                            // writing PV back to TT is safe becuase RootMoves is still
                            // valid, although refers to previous iteration.
                            if (SignalsStop)
                            {
                                LoopStackBroker.Free(ls);
                                return;
                            }

                            // In case of failing high/low increase aspiration window and
                            // research, otherwise exit the loop.
                            if (bestValue > alpha && bestValue < beta)
                            {
                                break;
                            }

                            // Give some update (without cluttering the UI) before to research
                            if (SearchTime.ElapsedMilliseconds > 3000)
                            {
                                pv_info_to_uci(pos, depth, alpha, beta);
                            }

                            if (Math.Abs(bestValue) >= ValueC.VALUE_KNOWN_WIN)
                            {
                                alpha = -ValueC.VALUE_INFINITE;
                                beta = ValueC.VALUE_INFINITE;
                            }
                            else if (bestValue >= beta)
                            {
                                beta += delta;
                                delta += delta / 2;
                            }
                            else
                            {
                                SignalsFailedLowAtRoot = true;
                                SignalsStopOnPonderhit = false;

                                alpha -= delta;
                                delta += delta / 2;
                            }

                            Debug.Assert(alpha >= -ValueC.VALUE_INFINITE && beta <= ValueC.VALUE_INFINITE);
                        }

                        // Sort the PV lines searched so far and update the GUI
                        Utils.sort(RootMoves, 0, PVIdx + 1);

                        if (PVIdx + 1 == PVSize || SearchTime.ElapsedMilliseconds > 3000)
                        {
                            pv_info_to_uci(pos, depth, alpha, beta);
                        }
                    }

                    // Do we need to pick now the sub-optimal best move ?
                    if (skill.enabled() && skill.time_to_pick(depth))
                    {
                        skill.pick_move();
                    }

                    // Filter out startup noise when monitoring best move stability
                    if (depth > 2 && (BestMoveChanges != 0))
                    {
                        bestMoveNeverChanged = false;
                    }

                    // Do we have found a "mate in x"?
                    if (Limits.mate != 0
                            && bestValue >= ValueC.VALUE_MATE_IN_MAX_PLY
                            && ValueC.VALUE_MATE - bestValue <= 2 * Limits.mate)
                    {
                        SignalsStop = true;
                    }

                    // Do we have time for the next iteration? Can we stop searching now?
                    if (Limits.use_time_management() && !SignalsStopOnPonderhit)
                    {
                        var stop = false; // Local variable, not the volatile Signals.stop

                        // Take in account some extra time if the best move has changed
                        if (depth > 4 && depth < 50 && PVSize == 1)
                        {
                            TimeMgr.pv_instability(BestMoveChanges, prevBestMoveChanges);
                        }

                        // Stop search if most of available time is already consumed. We
                        // probably don't have enough time to search the first move at the
                        // next iteration anyway.
                        if (SearchTime.ElapsedMilliseconds > (TimeMgr.available_time() * 62) / 100)
                        {
                            stop = true;
                        }

                        // Stop search early if one move seems to be much better than others
                        if (depth >= 12 
                            && !stop
                            && PVSize == 1
                            && ((bestMoveNeverChanged && (pos.captured_piece_type() != 0))
                                || SearchTime.ElapsedMilliseconds > (TimeMgr.available_time() * 40) / 100))
                        {
                            var rBeta = bestValue - 2 * Constants.PawnValueMidgame;
                            ss[ssPos + 1].excludedMove = RootMoves[0].pv[0];
                            ss[ssPos + 1].skipNullMove = 1;
                            var v = search(
                                NodeTypeC.NonPV,
                                pos,
                                ss,
                                ssPos + 1,
                                rBeta - 1,
                                rBeta,
                                (depth - 3) * DepthC.ONE_PLY);
                            ss[ssPos + 1].skipNullMove = 0;
                            ss[ssPos + 1].excludedMove = MoveC.MOVE_NONE;

                            if (v < rBeta)
                            {
                                stop = true;
                            }
                        }

                        if (stop)
                        {
                            // If we are allowed to ponder do not stop the search now but
                            // keep pondering until GUI sends "ponderhit" or "stop".
                            if (Limits.ponder)
                            {
                                SignalsStopOnPonderhit = true;
                            }
                            else
                            {
                                SignalsStop = true;
                            }
                        }
                    }
                }
            }

            LoopStackBroker.Free(ls);
        }

        // search<>() is the main search function for both PV and non-PV nodes and for
        // normal and SplitPoint nodes. When called just after a split point the search
        // is simpler because we have already probed the hash table, done a null move
        // search, and searched the first move before splitting, we don't have to repeat
        // all this work again. We also don't need to store anything to the hash table
        // here: This is taken care of after we return from the split point.
        internal static int search(int NT, Position pos, Stack[] ss, int ssPos, int alpha, int beta, int depth)
        {
            var PvNode = (NT == NodeTypeC.PV || NT == NodeTypeC.Root || NT == NodeTypeC.SplitPointPV
                          || NT == NodeTypeC.SplitPointRoot);
            var SpNode = (NT == NodeTypeC.SplitPointPV || NT == NodeTypeC.SplitPointNonPV
                          || NT == NodeTypeC.SplitPointRoot);
            var RootNode = (NT == NodeTypeC.Root || NT == NodeTypeC.SplitPointRoot);

            Debug.Assert(alpha >= -ValueC.VALUE_INFINITE && alpha < beta && beta <= ValueC.VALUE_INFINITE);
            Debug.Assert((PvNode || alpha == beta - 1));
            Debug.Assert(depth > DepthC.DEPTH_ZERO);

            var ms = MovesSearchedBroker.GetObject();
            var movesSearched = ms.movesSearched;

            StateInfo st = null;
            var tte = TT.StaticEntry;
            var tteHasValue = false;
            uint ttePos = 0;
            ulong posKey = 0;
            int ttMove, move, excludedMove, bestMove, threatMove;
            int ext, newDepth;
            int bestValue, value, ttValue;
            int eval = 0, nullValue, futilityValue;
            bool inCheck, givesCheck, pvMove, singularExtensionNode;
            bool captureOrPromotion, dangerous, doFullDepthSearch;
            int moveCount = 0, playedMoveCount = 0;
            SplitPoint sp = null;

            // Step 1. Initialize node
            var thisThread = pos.this_thread();
            //var threatExtension = false;
            inCheck = pos.in_check();
            
            if (SpNode)
            {
                sp = ss[ssPos].sp;
                bestMove = sp.bestMove;
                threatMove = sp.threatMove;
                bestValue = sp.bestValue;
                ttMove = excludedMove = MoveC.MOVE_NONE;
                ttValue = ValueC.VALUE_NONE;

                Debug.Assert(sp.bestValue > -ValueC.VALUE_INFINITE && sp.moveCount > 0);

                goto split_point_start;
            }

            bestValue = -ValueC.VALUE_INFINITE;
            ss[ssPos].currentMove = threatMove = ss[ssPos + 1].excludedMove = bestMove = MoveC.MOVE_NONE;
            ss[ssPos].ply = ss[ssPos - 1].ply + 1;
            ss[ssPos + 1].skipNullMove = 0;
            ss[ssPos + 1].reduction = DepthC.DEPTH_ZERO;
            ss[ssPos + 2].killers0 = ss[ssPos + 2].killers1 = MoveC.MOVE_NONE;

            // Used to send selDepth info to GUI
            if (PvNode && thisThread.maxPly < ss[ssPos].ply)
            {
                thisThread.maxPly = ss[ssPos].ply;
            }
            
            if (!RootNode)
            {
                // Step 2. Check for aborted search and immediate draw
                if ((SignalsStop || pos.is_draw(false) || ss[ssPos].ply > Constants.MAX_PLY))
                {
                    MovesSearchedBroker.Free();
                    return DrawValue[pos.sideToMove];
                }

                // Step 3. Mate distance pruning. Even if we mate at the next move our score
                // would be at best mate_in(ss->ply+1), but if alpha is already bigger because
                // a shorter mate was found upward in the tree then there is no need to search
                // further, we will never beat current alpha. Same logic but with reversed signs
                // applies also in the opposite condition of being mated instead of giving mate,
                // in this case return a fail-high score.
                alpha = Math.Max(Utils.mated_in(ss[ssPos].ply), alpha);
                beta = Math.Min(Utils.mate_in(ss[ssPos].ply + 1), beta);
                if (alpha >= beta)
                {
                    MovesSearchedBroker.Free();
                    return alpha;
                }
            }

            // Step 4. Transposition table lookup
            // We don't want the score of a partial search to overwrite a previous full search
            // TT value, so we use a different position key in case of an excluded move.
            excludedMove = ss[ssPos].excludedMove;
            posKey = (excludedMove != 0) ? pos.exclusion_key() : pos.key();
            tteHasValue = TT.probe(posKey, ref ttePos, out tte);
            ttMove = RootNode ? RootMoves[PVIdx].pv[0] : tteHasValue ? tte.move() : MoveC.MOVE_NONE;
            ttValue = tteHasValue ? value_from_tt(tte.value(), ss[ssPos].ply) : ValueC.VALUE_NONE;

            // At PV nodes we check for exact scores, while at non-PV nodes we check for
            // a fail high/low. Biggest advantage at probing at PV nodes is to have a
            // smooth experience in analysis mode. We don't probe at Root nodes otherwise
            // we should also update RootMoveList to avoid bogus output.
            if (!RootNode 
                && tteHasValue 
                && tte.depth() >= depth
                && ttValue != ValueC.VALUE_NONE // Only in case of TT access race
                && (PvNode ? tte.type() == Bound.BOUND_EXACT 
                            : ttValue >= beta ? ((tte.type() & Bound.BOUND_LOWER) != 0 )
                                              : ((tte.type() & Bound.BOUND_UPPER) != 0)))
            {
                Debug.Assert(ttValue != ValueC.VALUE_NONE); // Due to depth > DEPTH_NONE

                TT.table[ttePos].set_generation(TT.generation);
                ss[ssPos].currentMove = ttMove; // Can be MOVE_NONE

                if (ttValue >= beta && (ttMove != 0) && !pos.is_capture_or_promotion(ttMove)
                    && ttMove != ss[ssPos].killers0)
                {
                    ss[ssPos].killers1 = ss[ssPos].killers0;
                    ss[ssPos].killers0 = ttMove;
                }

                MovesSearchedBroker.Free();
                return ttValue;
            }

            // Step 5. Evaluate the position statically and update parentSplitPoint's gain statistics
            if (inCheck)
            {
                ss[ssPos].staticEval = ss[ssPos].evalMargin = eval = ValueC.VALUE_NONE;
            }
            else if (tteHasValue)
            {
                // Never assume anything on values stored in TT
                if ((ss[ssPos].staticEval = eval = tte.eval_value()) == ValueC.VALUE_NONE
                    || (ss[ssPos].evalMargin = tte.eval_margin()) == ValueC.VALUE_NONE)
                {
                    eval = ss[ssPos].staticEval = Evaluate.do_evaluate(false, pos, ref ss[ssPos].evalMargin);
                }

                // Can ttValue be used as a better position evaluation?
                if (ttValue != ValueC.VALUE_NONE)
                {
                    if ((((tte.type() & Bound.BOUND_LOWER) != 0) && ttValue > eval)
                        || (((tte.type() & Bound.BOUND_UPPER) != 0) && ttValue < eval))
                    {
                        eval = ttValue;
                    }
                }
            }
            else
            {
                eval = ss[ssPos].staticEval = Evaluate.do_evaluate(false, pos, ref ss[ssPos].evalMargin);
                TT.store(
                    posKey,
                    ValueC.VALUE_NONE,
                    Bound.BOUND_NONE,
                    DepthC.DEPTH_NONE,
                    MoveC.MOVE_NONE,
                    ss[ssPos].staticEval,
                    ss[ssPos].evalMargin);
            }

            // Update gain for the parentSplitPoint non-capture move given the static position
            // evaluation before and after the move.
            if ((move = ss[ssPos - 1].currentMove) != MoveC.MOVE_NULL && ss[ssPos - 1].staticEval != ValueC.VALUE_NONE
                && ss[ssPos].staticEval != ValueC.VALUE_NONE && (pos.captured_piece_type() == 0) && Utils.type_of_move(move) == MoveTypeC.NORMAL)
            {
                var to = Utils.to_sq(move);
                H.update_gain(pos.piece_on(to), to, -ss[ssPos - 1].staticEval - ss[ssPos].staticEval);
            }

            // Step 6. Razoring (is omitted in PV nodes)
            if (!PvNode && !inCheck && depth < 4 * DepthC.ONE_PLY && eval + razor_margin(depth) < beta
                && ttMove == MoveC.MOVE_NONE && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY
                && !pos.pawn_on_7th(pos.sideToMove))
            {
                var rbeta = beta - razor_margin(depth);
                var v = qsearch(NodeTypeC.NonPV, false, pos, ss, ssPos, rbeta - 1, rbeta, DepthC.DEPTH_ZERO);
                if (v < rbeta)
                {
                    // Logically we should return (v + razor_margin(depth)), but
                    // surprisingly this did slightly weaker in tests.
                    MovesSearchedBroker.Free();
                    return v;
                }
            }

            // Step 7. Static null move pruning (is omitted in PV nodes)
            // We're betting that the opponent doesn't have a move that will reduce
            // the score by more than futility_margin(depth) if we do a null move.
            if (!PvNode && !inCheck && (ss[ssPos].skipNullMove == 0) && depth < 4 * DepthC.ONE_PLY
                && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY && eval - FutilityMargins[depth][0] >= beta
                && (pos.non_pawn_material(pos.sideToMove) != 0))
            {
                MovesSearchedBroker.Free();
                return eval - FutilityMargins[depth][0];
            }

            // Step 8. Null move search with verification search (is omitted in PV nodes)
            if (!PvNode && !inCheck && (ss[ssPos].skipNullMove == 0) && depth > DepthC.ONE_PLY && eval >= beta
                && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY && (pos.non_pawn_material(pos.sideToMove) != 0))
            {
                ss[ssPos].currentMove = MoveC.MOVE_NULL;

                // Null move dynamic reduction based on depth
                Depth R = 3 * DepthC.ONE_PLY + depth / 4;

                // Null move dynamic reduction based on value
                if (eval - Constants.PawnValueMidgame > beta)
                {
                    R += DepthC.ONE_PLY;
                }

                if (st == null)
                {
                    st = StateInfoBroker.GetObject();
                }
                pos.do_null_move(st);
                ss[ssPos + 1].skipNullMove = 1;
                
                nullValue = depth - R < DepthC.ONE_PLY ? -qsearch(NodeTypeC.NonPV, false, pos, ss, ssPos + 1, -beta, -alpha, DepthC.DEPTH_ZERO)
                                      : -search(NodeTypeC.NonPV, pos, ss, ssPos + 1, -beta, -alpha, depth - R);

                ss[ssPos + 1].skipNullMove = 0;
                pos.undo_null_move(st);

                if (nullValue >= beta)
                {
                    // Do not return unproven mate scores
                    if (nullValue >= ValueC.VALUE_MATE_IN_MAX_PLY)
                    {
                        nullValue = beta;
                    }

                    if (depth < 6 * DepthC.ONE_PLY)
                    {
                        if (st != null)
                        {
                            st.previous = null;
                            StateInfoBroker.Free();
                        }
                        MovesSearchedBroker.Free();
                        return nullValue;
                    }

                    // Do verification search at high depths
                    ss[ssPos].skipNullMove = 1;
                    var v = search(NodeTypeC.NonPV, pos, ss, ssPos, alpha, beta, depth - R);
                    ss[ssPos].skipNullMove = 0;

                    if (v >= beta)
                    {
                        if (st != null)
                        {
                            st.previous = null;
                            StateInfoBroker.Free();
                        }
                        MovesSearchedBroker.Free();
                        return nullValue;
                    }
                }
                else
                {
                    // The null move failed low, which means that we may be faced with
                    // some kind of threat. If the previous move was reduced, check if
                    // the move that refuted the null move was somehow connected to the
                    // the move that refuted the null move was somehow connected to the
                    // move which was reduced. If a connection is found extend moves that
                    // defend against threat.
                    threatMove = ss[ssPos + 1].currentMove;

                    if (depth < 5 * DepthC.ONE_PLY && (ss[ssPos - 1].reduction != 0) && threatMove != MoveC.MOVE_NONE
                        && allows(pos, ss[ssPos - 1].currentMove, threatMove))
                    {
                        //threatExtension = true;
                        
                        if (st != null)
                        {
                            st.previous = null;
                            StateInfoBroker.Free();
                        }
                        MovesSearchedBroker.Free();
                        return beta - 1;
                    }
                }
            }

            // Step 9. ProbCut (is omitted in PV nodes)
            // If we have a very good capture (i.e. SEE > seeValues[captured_piece_type])
            // and a reduced search returns a value much above beta, we can (almost) safely
            // prune the previous move.
            if (!PvNode && !inCheck && excludedMove == MoveC.MOVE_NONE && depth >= 4 * DepthC.ONE_PLY + DepthC.ONE_PLY
                && (ss[ssPos].skipNullMove == 0) && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY)
            {
                var rbeta = beta + 200;
                var rdepth = depth - DepthC.ONE_PLY - 3 * DepthC.ONE_PLY;

                Debug.Assert(rdepth >= DepthC.ONE_PLY);
                Debug.Assert(ss[ssPos - 1].currentMove != MoveC.MOVE_NONE);
                Debug.Assert(ss[ssPos - 1].currentMove != MoveC.MOVE_NULL);

                var mp2 = MovePickerBroker.GetObject();
                mp2.MovePickerC(pos, ttMove, H, pos.captured_piece_type());
                var ci2 = CheckInfoBroker.GetObject();
                ci2.CreateCheckInfo(pos);

                while ((move = mp2.next_move()) != MoveC.MOVE_NONE)
                {
                    if (pos.pl_move_is_legal(move, ci2.pinned))
                    {
                        ss[ssPos].currentMove = move;
                        if (st == null)
                        {
                            st = StateInfoBroker.GetObject();
                        }
                        pos.do_move(move, st, ci2, pos.move_gives_check(move, ci2));
                        value = -search(NodeTypeC.NonPV, pos, ss, ssPos + 1, -rbeta, -rbeta + 1, rdepth);
                        pos.undo_move(move);
                        if (value >= rbeta)
                        {
                            if (st != null)
                            {
                                st.previous = null;
                                StateInfoBroker.Free();
                            }
                            CheckInfoBroker.Free();
                            MovePickerBroker.Free(mp2);
                            MovesSearchedBroker.Free();
                            return value;
                        }
                    }
                }

                CheckInfoBroker.Free();
                MovePickerBroker.Free(mp2);
            }

            // Step 10. Internal iterative deepening
            if (ttMove == MoveC.MOVE_NONE && depth >= (PvNode ? 5 * DepthC.ONE_PLY : 8 * DepthC.ONE_PLY)
                && (PvNode || (!inCheck && ss[ssPos].staticEval + 256 >= beta)))
            {
                var d = (PvNode ? depth - 2 * DepthC.ONE_PLY : depth / 2);

                ss[ssPos].skipNullMove = 1;
                search(PvNode ? NodeTypeC.PV : NodeTypeC.NonPV, pos, ss, ssPos, alpha, beta, d);
                ss[ssPos].skipNullMove = 0;

                tteHasValue = TT.probe(posKey, ref ttePos, out tte);
                ttMove = (tteHasValue) ? tte.move() : MoveC.MOVE_NONE;
            }
            else
            {
                // Re-read (needed as TTEntry is a struct in the port)
                if ((tteHasValue) && (TT.table[ttePos].key == tte.key))
                {
                    tte = TT.table[ttePos];
                }
            }

            split_point_start: // At split points actual search starts from here

            var mp = MovePickerBroker.GetObject();
            mp.MovePickerC(
                pos,
                ttMove,
                depth,
                H,
                ss[ssPos],
                PvNode ? -ValueC.VALUE_INFINITE : beta,
                SpNode ? ss[ssPos].sp.movePicker : null);
            var ci = CheckInfoBroker.GetObject();
            ci.CreateCheckInfo(pos);
            
            value = bestValue; // Workaround a bogus 'uninitialized' warning under gcc
            singularExtensionNode = !RootNode && !SpNode && depth >= (PvNode ? 6 * DepthC.ONE_PLY : 8 * DepthC.ONE_PLY)
                                    && ttMove != MoveC.MOVE_NONE && (excludedMove == 0)
                                    // Recursive singular search is not allowed
                                    && ((tte.type() & Bound.BOUND_LOWER) != 0) // FIXME: uninitialized!
                                    && tte.depth() >= depth - 3 * DepthC.ONE_PLY;

            // Step 11. Loop through moves
            // Loop through all pseudo-legal moves until no moves remain or a beta cutoff occurs
            while ((move = mp.next_move()) != MoveC.MOVE_NONE && !thisThread.cutoff_occurred()
                   && !SignalsStop)
            {
                Debug.Assert(Utils.is_ok_M(move));

                if (move == excludedMove)
                {
                    continue;
                }

                // At root obey the "searchmoves" option and skip moves not listed in Root
                // Move List, as a consequence any illegal move is also skipped. In MultiPV
                // mode we also skip PV moves which have been already searched.

                // If we find none, it means !count
                if (RootNode && (find(RootMoves, PVIdx, RootMoves.Count, move) == -1))
                {
                    continue;
                }

                if (SpNode)
                {
                    // Shared counter cannot be decremented later if move turns out to be illegal
                    if (!pos.pl_move_is_legal(move, ci.pinned))
                    {
                        continue;
                    }

                    moveCount = ++sp.moveCount;
                    ThreadHelper.lock_release(sp.Lock);
                }
                else
                {
                    moveCount++;
                }

                if (RootNode)
                {
                    SignalsFirstRootMove = (moveCount == 1);

                    if (thisThread == Threads.main_thread() && SearchTime.ElapsedMilliseconds > 3000)
                    {
                        Plug.Write("info depth ");
                        Plug.Write((depth / DepthC.ONE_PLY).ToString());
                        Plug.Write(" currmove ");
                        Plug.Write(Utils.move_to_uci(move, pos.chess960));
                        Plug.Write(" nodes ");
                        Plug.Write(pos.nodes.ToString());
                        Plug.Write(" currmovenumber ");
                        Plug.Write((moveCount + PVIdx).ToString());
                        Plug.Write(Constants.endl);
                    }
                }

                ext = DepthC.DEPTH_ZERO;
                captureOrPromotion = pos.is_capture_or_promotion(move);
                givesCheck = pos.move_gives_check(move, ci);
                dangerous = givesCheck 
                            || pos.is_passed_pawn_push(move)
                            || Utils.type_of_move(move) == MoveTypeC.CASTLING
                            || (captureOrPromotion // Entering a pawn endgame?
                                    && Utils.type_of(pos.piece_on(Utils.to_sq(move))) != PieceTypeC.PAWN
                                    && Utils.type_of_move(move) == MoveTypeC.NORMAL
                                    && (pos.non_pawn_material(ColorC.WHITE) + pos.non_pawn_material(ColorC.BLACK) 
                                        - Position.PieceValue[PhaseC.MG][pos.piece_on(Utils.to_sq(move))] == ValueC.VALUE_ZERO));

                // Step 12. Extend checks and, in PV nodes, also dangerous moves
                if (PvNode && dangerous)
                {
                    ext = DepthC.ONE_PLY;
                }
                // else if (threatExtension && refutes(pos, move, threatMove))
                // {
                // ext = DepthC.ONE_PLY;
                // }
                else if (givesCheck && pos.see(move, true) >= 0)
                {
                    ext = DepthC.ONE_PLY / 2;
                }

                // Singular extension search. If all moves but one fail low on a search of
                // (alpha-s, beta-s), and just one fails high on (alpha, beta), then that move
                // is singular and should be extended. To verify this we do a reduced search
                // on all the other moves but the ttMove, if result is lower than ttValue minus
                // a margin then we extend ttreMove.
                if (singularExtensionNode && move == ttMove && (ext == 0) && pos.pl_move_is_legal(move, ci.pinned))
                {
                    Debug.Assert(ttValue != ValueC.VALUE_NONE);

                    var rBeta = ttValue - depth;
                    ss[ssPos].excludedMove = move;
                    ss[ssPos].skipNullMove = 1;
                    value = search(NodeTypeC.NonPV, pos, ss, ssPos, rBeta - 1, rBeta, depth / 2);
                    ss[ssPos].skipNullMove = 0;
                    ss[ssPos].excludedMove = MoveC.MOVE_NONE;
                    if (value < rBeta)
                    {
                        ext = DepthC.ONE_PLY;
                    }
                }

                // Update current move (this must be done after singular extension search)
                newDepth = depth - DepthC.ONE_PLY + ext;

                // Step 13. Futility pruning (is omitted in PV nodes)
                if (!PvNode
                    && !captureOrPromotion
                    && !inCheck 
                    && !dangerous 
                    && move != ttMove
                    && (bestValue > ValueC.VALUE_MATED_IN_MAX_PLY || (bestValue == -ValueC.VALUE_INFINITE && alpha > ValueC.VALUE_MATED_IN_MAX_PLY)))
                {
                    // Move count based pruning
                    if (depth < 16 * DepthC.ONE_PLY
                        && moveCount >= FutilityMoveCounts[depth]
                        && ((threatMove == 0) || !refutes(pos, move, threatMove)))
                    {
                        if (SpNode)
                        {
                            ThreadHelper.lock_grab(sp.Lock);
                        }

                        continue;
                    }

                    // Value based pruning
                    // We illogically ignore reduction condition depth >= 3*ONE_PLY for predicted depth,
                    // but fixing this made program slightly weaker.
                    var predictedDepth = newDepth - reduction(PvNode, depth, moveCount);
                    futilityValue = ss[ssPos].staticEval + ss[ssPos].evalMargin + futility_margin(predictedDepth, moveCount)
                                    + H.gain(pos.piece_moved(move), Utils.to_sq(move));

                    if (futilityValue < beta)
                    {
                        if (SpNode)
                        {
                            ThreadHelper.lock_grab(sp.Lock);
                        }

                        continue;
                    }

                    // Prune moves with negative SEE at low depths
                    if (predictedDepth < 2 * DepthC.ONE_PLY && pos.see(move, true) < 0)
                    {
                        if (SpNode)
                        {
                            ThreadHelper.lock_grab(sp.Lock);
                        }

                        continue;
                    }
                }

                // Check for legality only before to do the move
                if (!RootNode && !SpNode && !pos.pl_move_is_legal(move, ci.pinned))
                {
                    moveCount--;
                    continue;
                }

                pvMove = (PvNode && moveCount == 1);
                ss[ssPos].currentMove = move;
                if (!SpNode && !captureOrPromotion && playedMoveCount < 64)
                {
                    movesSearched[playedMoveCount++] = move;
                }

                // Step 14. Make the move
                if (st == null)
                {
                    st = StateInfoBroker.GetObject();
                }
                pos.do_move(move, st, ci, givesCheck);

                // Step 15. Reduced depth search (LMR). If the move fails high will be
                // re-searched at full depth.
                if (depth > 3 * DepthC.ONE_PLY
                    && !pvMove 
                    && !captureOrPromotion 
                    && !dangerous
                    && move != ttMove
                    && move != ss[ssPos].killers0
                    && move != ss[ssPos].killers1)
                {
                    ss[ssPos].reduction = reduction(PvNode, depth, moveCount);
                    var d = Math.Max(newDepth - ss[ssPos].reduction, DepthC.ONE_PLY);
                    alpha = SpNode ? sp.alpha : alpha;

                    value = -search(NodeTypeC.NonPV, pos, ss, ssPos + 1, -(alpha + 1), -alpha, d);

                    doFullDepthSearch = (value > alpha && ss[ssPos].reduction != DepthC.DEPTH_ZERO);
                    ss[ssPos].reduction = DepthC.DEPTH_ZERO;
                }
                else
                {
                    doFullDepthSearch = !pvMove;
                }

                // Step 16. Full depth search, when LMR is skipped or fails high
                if (doFullDepthSearch)
                {
                    alpha = SpNode ? sp.alpha : alpha;
                    value = newDepth < DepthC.ONE_PLY 
                            ? -qsearch(NodeTypeC.NonPV, givesCheck, pos, ss, ssPos + 1, -(alpha + 1), -alpha, DepthC.DEPTH_ZERO)
                            : -search(NodeTypeC.NonPV, pos, ss, ssPos + 1, -(alpha + 1), -alpha, newDepth);
                }

                // Only for PV nodes do a full PV search on the first move or after a fail
                // high, in the latter case search only if value < beta, otherwise let the
                // parentSplitPoint node to fail low with value <= alpha and to try another move.
                if (PvNode && (pvMove || (value > alpha && (RootNode || value < beta))))
                {
                    value = newDepth < DepthC.ONE_PLY
                                ? -qsearch(NodeTypeC.PV, givesCheck, pos, ss, ssPos + 1, -beta, -alpha, DepthC.DEPTH_ZERO)
                                : -search(NodeTypeC.PV, pos, ss, ssPos + 1, -beta, -alpha, newDepth);
                }

                // Step 17. Undo move
                pos.undo_move(move);

                Debug.Assert(value > -ValueC.VALUE_INFINITE && value < ValueC.VALUE_INFINITE);

                // Step 18. Check for new best move
                if (SpNode)
                {
                    ThreadHelper.lock_grab(sp.Lock);
                    bestValue = sp.bestValue;
                    alpha = sp.alpha;
                }

                // Finished searching the move. If Signals.stop is true, the search
                // was aborted because the user interrupted the search or because we
                // ran out of time. In this case, the return value of the search cannot
                // be trusted, and we don't update the best move and/or PV.
                if (SignalsStop || thisThread.cutoff_occurred())
                {
                    if (st != null)
                    {
                        st.previous = null;
                        StateInfoBroker.Free();
                    }
                    CheckInfoBroker.Free();
                    MovePickerBroker.Free(mp);
                    MovesSearchedBroker.Free();

                    return value; // To avoid returning VALUE_INFINITE
                }

                // Finished searching the move. If Signals.stop is true, the search
                // was aborted because the user interrupted the search or because we
                // ran out of time. In this case, the return value of the search cannot
                // be trusted, and we don't update the best move and/or PV.
                if (RootNode)
                {
                    var rmPos = find(RootMoves, 0, RootMoves.Count, move);

                    // PV move or new best move ?
                    if (pvMove || value > alpha)
                    {
                        RootMoves[rmPos].score = value;
                        RootMoves[rmPos].extract_pv_from_tt(pos);

                        // We record how often the best move has been changed in each
                        // iteration. This information is used for time management: When
                        // the best move changes frequently, we allocate some more time.
                        if (!pvMove)
                        {
                            BestMoveChanges++;
                        }
                    }
                    else
                    {
                        // All other moves but the PV are set to the lowest value, this
                        // is not a problem when sorting becuase sort is stable and move
                        // position in the list is preserved, just the PV is pushed up.
                        RootMoves[rmPos].score = -ValueC.VALUE_INFINITE;
                    }
                }

                if (value > bestValue)
                {
                    bestValue = value;
                    if (SpNode) sp.bestValue = value;

                    if (value > alpha)
                    {
                        bestMove = move;
                        if (SpNode) sp.bestMove = move;

                        if (PvNode && value < beta)
                        {
                            alpha = value; // Update alpha here! Always alpha < beta
                            if (SpNode) sp.alpha = value;
                        }
                        else 
                        {
                            Debug.Assert(value >= beta); // Fail high

                            if (SpNode) sp.cutoff = true;
                            break;
                        }
                    }
                }

                // Step 19. Check for split
                if (!SpNode 
                    && depth >= Threads.minimumSplitDepth 
                    && Threads.available_slave(thisThread) != null
                    && thisThread.splitPointsSize < Constants.MAX_SPLITPOINTS_PER_THREAD)
                {
                    Debug.Assert(bestValue < beta);

                    Threads.split(
                        Constants.FakeSplit,
                        pos,
                        ss,
                        ssPos,
                        alpha,
                        beta,
                        ref bestValue,
                        ref bestMove,
                        depth,
                        threatMove,
                        moveCount,
                        mp,
                        NT);

                    if (bestValue >= beta)
                    {
                        break;
                    }
                }
            }

            // Step 20. Check for mate and stalemate
            // All legal moves have been searched and if there are no legal moves, it
            // must be mate or stalemate. Note that we can have a false positive in
            // case of Signals.stop or thread.cutoff_occurred() are set, but this is
            // harmless because return value is discarded anyhow in the parentSplitPoint nodes.
            // If we are in a singular extension search then return a fail low score.
            // A split node has at least one move, the one tried before to be splitted.
            if (!SpNode && moveCount == 0)
            {
                if (st != null)
                {
                    st.previous = null;
                    StateInfoBroker.Free();
                }
                CheckInfoBroker.Free();
                MovePickerBroker.Free(mp);
                MovesSearchedBroker.Free();
                return (excludedMove != 0) ? alpha : inCheck ? Utils.mated_in(ss[ssPos].ply) : DrawValue[pos.sideToMove];
            }

            // If we have pruned all the moves without searching return a fail-low score
            if (bestValue == -ValueC.VALUE_INFINITE)
            {
                Debug.Assert(playedMoveCount == 0);
                bestValue = alpha;
            }

            if (bestValue >= beta) // Failed high
            {
                TT.store(
                    posKey,
                    value_to_tt(bestValue, ss[ssPos].ply),
                    Bound.BOUND_LOWER,
                    depth,
                    bestMove,
                    ss[ssPos].staticEval,
                    ss[ssPos].evalMargin);

                if (!pos.is_capture_or_promotion(bestMove) && !inCheck)
                {
                    if (bestMove != ss[ssPos].killers0)
                    {
                        ss[ssPos].killers1 = ss[ssPos].killers0;
                        ss[ssPos].killers0 = bestMove;
                    }

                    // Increase history value of the cut-off move
                    var bonus = (depth * depth);
                    H.add(pos.piece_moved(bestMove), Utils.to_sq(bestMove), bonus);

                    // Decrease history of all the other played non-capture moves
                    for (var i = 0; i < playedMoveCount - 1; i++)
                    {
                        var m = movesSearched[i];
                        H.add(pos.piece_moved(m), Utils.to_sq(m), -bonus);
                    }
                }
            }
            else // Failed low or PV search
            {
                TT.store(posKey, value_to_tt(bestValue, ss[ssPos].ply), PvNode && bestMove != MoveC.MOVE_NONE ? Bound.BOUND_EXACT : Bound.BOUND_UPPER, depth, bestMove, ss[ssPos].staticEval, ss[ssPos].evalMargin);
            }

            Debug.Assert(bestValue > -ValueC.VALUE_INFINITE && bestValue < ValueC.VALUE_INFINITE);

            if (st != null)
            {
                st.previous = null;
                StateInfoBroker.Free();
            }
            CheckInfoBroker.Free();
            MovePickerBroker.Free(mp);
            MovesSearchedBroker.Free();

            return bestValue;
        }

        // qsearch() is the quiescence search function, which is called by the main
        // search function when the remaining depth is zero (or, to be more precise,
        // less than ONE_PLY).
        private static int qsearch(int NT, bool InCheck, Position pos, Stack[] ss, int ssPos, int alpha, int beta, int depth)
        {
            var PvNode = (NT == NodeTypeC.PV);

            Debug.Assert(NT == NodeTypeC.PV || NT == NodeTypeC.NonPV);
            Debug.Assert(InCheck == pos.in_check());
            Debug.Assert(alpha >= -ValueC.VALUE_INFINITE && alpha < beta && beta <= ValueC.VALUE_INFINITE);
            Debug.Assert(PvNode || (alpha == beta - 1));
            Debug.Assert(depth <= DepthC.DEPTH_ZERO);

            StateInfo st = null;
            int ttMove, move, bestMove;
            int ttValue, bestValue, value, futilityValue, futilityBase, oldAlpha = 0;

            bool givesCheck, enoughMaterial, evasionPrunable, fromNull;
            var tteHasValue = false;
            TTEntry tte;
            uint ttePos = 0;
            int ttDepth;
            Key posKey;

            // To flag BOUND_EXACT a node with eval above alpha and no available moves
            if (PvNode)
            {
                oldAlpha = alpha;
            }

            ss[ssPos].currentMove = bestMove = MoveC.MOVE_NONE;
            ss[ssPos].ply = ss[ssPos - 1].ply + 1;
            fromNull = ss[ssPos - 1].currentMove == MoveC.MOVE_NULL;

            // Check for an instant draw or maximum ply reached
            if (pos.is_draw(true) || ss[ssPos].ply > Constants.MAX_PLY)
            {
                return DrawValue[pos.sideToMove];
            }

            // Transposition table lookup. At PV nodes, we don't use the TT for
            // pruning, but only for move ordering.
            posKey = pos.key();
            tteHasValue = TT.probe(posKey, ref ttePos, out tte);
            ttMove = (tteHasValue ? tte.move() : MoveC.MOVE_NONE);
            ttValue = tteHasValue ? value_from_tt(tte.value(), ss[ssPos].ply) : ValueC.VALUE_NONE;

            // Decide whether or not to include checks, this fixes also the type of
            // TT entry depth that we are going to use. Note that in qsearch we use
            // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
            ttDepth = (InCheck || depth >= DepthC.DEPTH_QS_CHECKS ? DepthC.DEPTH_QS_CHECKS : DepthC.DEPTH_QS_NO_CHECKS);
            
            if (tteHasValue 
                && tte.depth() >= depth
                && ttValue != ValueC.VALUE_NONE // Only in case of TT access race
                && (PvNode ? tte.type() == Bound.BOUND_EXACT
                            : ttValue >= beta ? ((tte.type() & Bound.BOUND_LOWER) != 0)
                                              : ((tte.type() & Bound.BOUND_UPPER) != 0)))
            {
                ss[ssPos].currentMove = ttMove; // Can be MOVE_NONE
                return ttValue;
            }

            // Evaluate the position statically
            if (InCheck)
            {
                ss[ssPos].staticEval = ss[ssPos].evalMargin = ValueC.VALUE_NONE;
                bestValue = futilityBase = -ValueC.VALUE_INFINITE;
                enoughMaterial = false;
            }
            else
            {
                if (fromNull)
                {
                    // Approximated score. Real one is slightly higher due to tempo
                    ss[ssPos].staticEval = bestValue = -ss[ssPos - 1].staticEval;
                    ss[ssPos].evalMargin = ValueC.VALUE_ZERO;
                }
                else if (tteHasValue)
                {
                    // Never assume anything on values stored in TT
                    if ((ss[ssPos].staticEval = bestValue = tte.eval_value()) == ValueC.VALUE_NONE
                        || (ss[ssPos].evalMargin = tte.eval_margin()) == ValueC.VALUE_NONE)
                    {
                        ss[ssPos].staticEval = bestValue = Evaluate.do_evaluate(false, pos, ref ss[ssPos].evalMargin);
                    }
                }
                else
                {
                    ss[ssPos].staticEval = bestValue = Evaluate.do_evaluate(false, pos, ref ss[ssPos].evalMargin);
                }

                // Stand pat. Return immediately if static value is at least beta
                if (bestValue >= beta)
                {
                    if (!tteHasValue)
                    {
                        TT.store(
                            pos.key(),
                            value_to_tt(bestValue, ss[ssPos].ply),
                            Bound.BOUND_LOWER,
                            DepthC.DEPTH_NONE,
                            MoveC.MOVE_NONE,
                            ss[ssPos].staticEval,
                            ss[ssPos].evalMargin);
                    }

                    return bestValue;
                }

                if (PvNode && bestValue > alpha)
                {
                    alpha = bestValue;
                }

                futilityBase = ss[ssPos].staticEval + ss[ssPos].evalMargin + 128;
                enoughMaterial = (pos.sideToMove == 0 ? pos.st.npMaterialWHITE : pos.st.npMaterialBLACK)
                                 > Constants.RookValueMidgame;
            }

            // Initialize a MovePicker object for the current position, and prepare
            // to search the moves. Because the depth is <= 0 here, only captures,
            // queen promotions and checks (only if depth >= DEPTH_QS_CHECKS) will
            // be generated.
            var mp = MovePickerBroker.GetObject();
            mp.MovePickerC(pos, ttMove, depth, H, (ss[ssPos - 1].currentMove) & 0x3F);
            var ci = CheckInfoBroker.GetObject();
            ci.CreateCheckInfo(pos);

            // Loop through the moves until no moves remain or a beta cutoff occurs
            while ((move = mp.next_move()) != MoveC.MOVE_NONE)
            {
                Debug.Assert(Utils.is_ok_M(move));

                givesCheck = pos.move_gives_check(move, ci);

                // Futility pruning
                if (!PvNode 
                    && !InCheck 
                    && !givesCheck
                    && !fromNull
                    && move != ttMove 
                    && enoughMaterial
                    && Utils.type_of_move(move) != MoveTypeC.PROMOTION && !pos.is_passed_pawn_push(move))
                {
                    futilityValue = futilityBase + Position.PieceValue[PhaseC.EG][pos.board[move & 0x3F]]
                                    + (Utils.type_of_move(move) == MoveTypeC.ENPASSANT
                                           ? Constants.PawnValueEndgame
                                           : ValueC.VALUE_ZERO);

                    if (futilityValue < beta)
                    {
                        bestValue = Math.Max(bestValue, futilityValue);
                        continue;
                    }

                    // Prune moves with negative or equal SEE
                    if (futilityBase < beta 
                        && depth < DepthC.DEPTH_ZERO 
                        && pos.see(move, false) <= 0)
                    {
                        bestValue = Math.Max(bestValue, futilityBase);
                        continue;
                    }
                }

                // Detect non-capture evasions that are candidate to be pruned
                evasionPrunable = !PvNode 
                                    && InCheck 
                                    && bestValue > ValueC.VALUE_MATED_IN_MAX_PLY
                                    && !pos.is_capture(move)
                                    && (pos.can_castle_C(pos.sideToMove) == 0);
                
                // Don't search moves with negative SEE values
                if (!PvNode 
                    && move != ttMove 
                    && (!InCheck || evasionPrunable) 
                    && Utils.type_of_move(move) != MoveTypeC.PROMOTION
                    && pos.see(move, true) < 0)
                {
                    continue;
                }

                // Don't search useless checks
                if (!PvNode 
                    && !InCheck 
                    && givesCheck 
                    && move != ttMove
                    && !pos.is_capture_or_promotion(move)
                    && ss[ssPos].staticEval + Constants.PawnValueMidgame / 4 < beta
                    && !check_is_dangerous(pos, move, futilityBase, beta))
                {
                    continue;
                }

                // Check for legality only before to do the move
                if (!pos.pl_move_is_legal(move, ci.pinned))
                {
                    continue;
                }

                ss[ssPos].currentMove = move;

                // Make and search the move
                if (st == null)
                {
                    st = StateInfoBroker.GetObject();
                }
                pos.do_move(move, st, ci, givesCheck);
                value = -qsearch(NT, givesCheck, pos, ss, ssPos + 1, -beta, -alpha, depth - DepthC.ONE_PLY);
                pos.undo_move(move);

                Debug.Assert(value > -ValueC.VALUE_INFINITE && value < ValueC.VALUE_INFINITE);

                // Check for new best move
                if (value > bestValue)
                {
                    bestValue = value;
                    
                    if (value > alpha)
                    {
                        if (PvNode && value < beta) // Update alpha here! Always alpha < beta
                        {
                            alpha = value;
                            bestMove = move;
                        }
                        else // Fail high
                        {
                            TT.store(posKey, value_to_tt(value, ss[ssPos].ply), Bound.BOUND_LOWER, 
                                ttDepth, move, ss[ssPos].staticEval, ss[ssPos].evalMargin);

                            if (st != null)
                            {
                                st.previous = null;
                                StateInfoBroker.Free();
                            }
                            CheckInfoBroker.Free();
                            MovePickerBroker.Free(mp);
                            return value;
                        }
                    }
                }
            }

            // All legal moves have been searched. A special case: If we're in check
            // and no legal moves were found, it is checkmate.
            if (InCheck && bestValue == -ValueC.VALUE_INFINITE)
            {
                if (st != null)
                {
                    st.previous = null;
                    StateInfoBroker.Free();
                }
                CheckInfoBroker.Free();
                MovePickerBroker.Free(mp);
                return Utils.mated_in(ss[ssPos].ply); // Plies to mate from the root
            }

            TT.store(posKey, value_to_tt(bestValue, ss[ssPos].ply),
                    //PvNode && bestMove != MoveC.MOVE_NONE ? Bound.BOUND_EXACT : Bound.BOUND_UPPER,
                    PvNode && bestMove > oldAlpha ? Bound.BOUND_EXACT : Bound.BOUND_UPPER, // TODO: this line asserts in bench
                    ttDepth, bestMove, ss[ssPos].staticEval, ss[ssPos].evalMargin);

            Debug.Assert(bestValue > -ValueC.VALUE_INFINITE && bestValue < ValueC.VALUE_INFINITE);

            if (st != null)
            {
                st.previous = null;
                StateInfoBroker.Free();
            }
            CheckInfoBroker.Free();
            MovePickerBroker.Free(mp);

            return bestValue;
        }

        // check_is_dangerous() tests if a checking move can be pruned in qsearch().
        // bestValue is updated only when returning false because in that case move
        // will be pruned.
        private static bool check_is_dangerous(Position pos, int move, int futilityBase, int beta)
        {
            //ulong b, occ, oldAtt, newAtt, kingAtt;
            //int from, to, ksq;
            //int pc;
            //int them;

            //from = Utils.from_sq(move);
            //to = Utils.to_sq(move);
            //them = Utils.flip_C(pos.sideToMove);
            //ksq = pos.king_square(them);
            //kingAtt = Position.attacks_from_KING(ksq);
            //pc = pos.piece_moved(move);

            //occ = pos.occupied_squares ^ Utils.SquareBB[from] ^ Utils.SquareBB[ksq];
            //oldAtt = Position.attacks_from(pc, from, occ);
            //newAtt = Position.attacks_from(pc, to, occ);

            //// Rule 1. Checks which give opponent's king at most one escape square are dangerous
            //b = kingAtt & ~pos.pieces_C(them) & ~newAtt & ~(1UL << to);

            //if ((b & (b - 1)) == 0) // Catches also !b
            Piece pc = pos.piece_moved(move);
            Square from = Utils.from_sq(move);
            Square to = Utils.to_sq(move);
            Color them = pos.sideToMove ^ 1;
            Square ksq = pos.king_square(them);
            Bitboard enemies = pos.pieces_C(them);
            Bitboard kingAtt = Position.attacks_from_KING(ksq);
            Bitboard occ = pos.occupied_squares ^ Utils.SquareBB[from] ^ Utils.SquareBB[ksq];
            Bitboard oldAtt = Position.attacks_from(pc, from, occ);
            Bitboard newAtt = Position.attacks_from(pc, to, occ);

            // Checks which give opponent's king at most one escape square are dangerous
            if (!Utils.more_than_one(kingAtt & ~(enemies | newAtt | (ulong)to)))
            {
                return true;
            }

            // Queen contact check is very dangerous
            if (Utils.type_of(pc) == PieceTypeC.QUEEN && (Utils.bit_is_set(kingAtt, to) != 0))
            {
                return true;
            }

            // Creating new double threats with checks is dangerous
            Bitboard b = (enemies ^ (ulong)ksq) & newAtt & ~oldAtt;
            while (b != 0)
            {
                // Note that here we generate illegal "double move"!
                if (futilityBase + Position.PieceValue[PhaseC.EG][pos.piece_on(Utils.pop_lsb(ref b))] >= beta)
                {
                    return true;
                }
            }
            return false;
        }

        // allows() tests whether the 'first' move at previous ply somehow makes the
        // 'second' move possible, for instance if the moving piece is the same in
        // both moves. Normally the second move is the threat (the best move returned
        // from a null search that fails low).
        internal static bool allows(Position pos, int first, int second)
        {
            Debug.Assert(Utils.is_ok_M(first));
            Debug.Assert(Utils.is_ok_M(second));
            Debug.Assert(Utils.color_of(pos.piece_on(Utils.from_sq(second))) == 1 - pos.sideToMove);

            Square m1to = Utils.to_sq(first);
            Square m1from = Utils.from_sq(first);
            Square m2to = Utils.to_sq(second);
            Square m2from = Utils.from_sq(second);


            // The piece is the same or second's destination was vacated by the first move
            if (m1to == m2from || m2to == m1from)
            {
                return true;
            }

            // Second one moves through the square vacated by first one
            if (Utils.bit_is_set(Utils.between_bb(m2from, m2to), m1from) != 0)
            {
                return true;
            }

            // Second's destination is defended by the first move's piece
            Bitboard m1att = Position.attacks_from(pos.piece_on(m1to), m1to, pos.occupied_squares ^ (ulong)m2from);
            if (Utils.bit_is_set(m1att, m2to) != 0)
            {
                return true;
            }

            // Second move gives a discovered check through the first's checking piece
            if (Utils.bit_is_set(m1att, pos.king_square(pos.sideToMove)) != 0 &&
                Utils.bit_is_set(Utils.between_bb(m1to, pos.king_square(pos.sideToMove)), m2from) != 0) // TODO: removing condition asserts below
            {
                Debug.Assert(Utils.bit_is_set(Utils.between_bb(m1to, pos.king_square(pos.sideToMove)), m2from) != 0);
                return true;
            }

            return false;
        }

        // value_to_tt() adjusts a mate score from "plies to mate from the root" to
        // "plies to mate from the current position". Non-mate scores are unchanged.
        // The function is called before storing a value to the transposition table.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private static int value_to_tt(int v, int ply)
        {
            Debug.Assert(v != ValueC.VALUE_NONE); 

            return v >= ValueC.VALUE_MATE_IN_MAX_PLY ? v + ply
                 : v <= ValueC.VALUE_MATED_IN_MAX_PLY ? v - ply : v;
        }

        // value_from_tt() is the inverse of value_to_tt(): It adjusts a mate score
        // from the transposition table (where refers to the plies to mate/be mated
        // from current position) to "plies to mate/be mated from the root".
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private static int value_from_tt(int v, int ply)
        {
            return v == ValueC.VALUE_NONE ? ValueC.VALUE_NONE
                 : v >= ValueC.VALUE_MATE_IN_MAX_PLY ? v - ply
                 : v <= ValueC.VALUE_MATED_IN_MAX_PLY ? v + ply : v;
        }

        // refutes() tests whether a 'first' move is able to defend against a 'second'
        // opponent's move. In this case will not be pruned. Normally the second move
        // is the threat (the best move returned from a null search that fails low).
        private static bool refutes(Position pos, int move, int threat)
        {
            Debug.Assert(Utils.is_ok_M(move));
            Debug.Assert(Utils.is_ok_M(threat));
            
            Square mfrom = Utils.from_sq(move);
            Square mto = Utils.to_sq(move);
            Square tfrom = Utils.from_sq(threat);
            Square tto = Utils.to_sq(threat);

            // Don't prune moves of the threatened piece
            if (mfrom == tto)
            {
                return true;
            }

            // If the threatened piece has value less than or equal to the value of the
            // threat piece, don't prune moves which defend it.
            if (pos.is_capture(threat)
                && (Position.PieceValue[PhaseC.MG][pos.piece_on(tfrom)] >= Position.PieceValue[PhaseC.MG][pos.piece_on(tto)]
                    || Utils.type_of(pos.piece_on(tfrom)) == PieceTypeC.KING))
            {
                // Update occupancy as if the piece and the threat are moving
                var occ = Utils.xor_bit(Utils.xor_bit(Utils.xor_bit(pos.occupied_squares, mfrom), mto), tfrom);
                Piece piece = pos.piece_on(mfrom);

                // The piece moved in 'to' attacks the square 's' ?
                if (Utils.bit_is_set(Position.attacks_from(piece, mto, occ), tto) != 0)
                {
                    return true;
                }

                // Scan for possible X-ray attackers behind the moved piece
                var xray = (Utils.rook_attacks_bb(tto, occ)
                        & pos.pieces(PieceTypeC.ROOK, PieceTypeC.QUEEN, Utils.color_of(piece)))
                        | (Utils.bishop_attacks_bb(tto, occ)
                            & pos.pieces(PieceTypeC.BISHOP, PieceTypeC.QUEEN, Utils.color_of(piece)));

                // Verify attackers are triggered by our move and not already existing
                if ((xray != 0) && ((xray ^ (xray & pos.attacks_from_QUEEN(tto))) != 0))
                {
                    return true;
                }
            }

            // Don't prune safe moves which block the threat path
            if ((Utils.bit_is_set(Utils.between_bb(tfrom, tto), mto) != 0) && pos.see(move, true) >= 0)
            {
                return true;
            }

            return false;
        }

        // pv_info_to_uci() sends search info to GUI. UCI protocol requires to send all
        // the PV lines also if are still to be searched and so refer to the previous
        // search score.
        private static void pv_info_to_uci(Position pos, int depth, int alpha, int beta)
        {
            var t = SearchTime.ElapsedMilliseconds;
            var uciPVSize = Math.Min(int.Parse(OptionMap.Instance["MultiPV"].v), RootMoves.Count);
             var selDepth = 0;

            for (var i = 0; i < Threads.size(); i++)
            {
                if (Threads.threads[i].maxPly > selDepth)
                {
                    selDepth = Threads.threads[i].maxPly;
                }
            }

            for (var i = 0; i < uciPVSize; i++)
            {
                var updated = (i <= PVIdx);

                if (depth == 1 && !updated)
                {
                    continue;
                }

                var d = (updated ? depth : depth - 1);
                var v = (updated ? RootMoves[i].score : RootMoves[i].prevScore);
                var s = new StringBuilder();

                for (var j = 0; RootMoves[i].pv[j] != MoveC.MOVE_NONE; j++)
                {
                    s.Append(" ").Append(Utils.move_to_uci(RootMoves[i].pv[j], pos.chess960));
                }

                Plug.Write("info depth ");
                Plug.Write(d.ToString());
                Plug.Write(" seldepth ");
                Plug.Write(selDepth.ToString());
                Plug.Write(" score ");
                Plug.Write((i == PVIdx ? score_to_uci(v, alpha, beta) : score_to_uci(v)));
                Plug.Write(" nodes ");
                Plug.Write(pos.nodes.ToString());
                Plug.Write(" nps ");
                Plug.Write(((t > 0 ? pos.nodes * 1000 / t : 0)).ToString());
                Plug.Write(" time ");
                Plug.Write(t.ToString());
                Plug.Write(" multipv ");
                Plug.Write((i + 1).ToString());
                Plug.Write(" pv");
                Plug.Write(s.ToString());
                Plug.Write(Constants.endl);
            }
        }

        // score_to_uci() converts a value to a string suitable for use with the UCI
        // protocol specifications:
        //
        // cp <x>     The score from the engine's point of view in centipawns.
        // mate <y>   Mate in y moves, not plies. If the engine is getting mated
        //            use negative values for y.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static string score_to_uci(int v)
        {
            return score_to_uci(v, -ValueC.VALUE_INFINITE, ValueC.VALUE_INFINITE);
        }

        internal static string score_to_uci(int v, int alpha, int beta)
        {
            var s = new StringBuilder();

            if (Math.Abs(v) < ValueC.VALUE_MATE_IN_MAX_PLY)
            {
                s.Append("cp ").Append(v * 100 / Constants.PawnValueMidgame);
            }
            else
            {
                s.Append("mate ").Append((v > 0 ? ValueC.VALUE_MATE - v + 1 : -ValueC.VALUE_MATE - v) / 2);
            }

            s.Append((v >= beta ? " lowerbound" : v <= alpha ? " upperbound" : ""));

            return s.ToString();
        }

        /// do_timer_event() is called by the timer thread when the timer triggers. It
        /// is used to print debug info and, more important, to detect when we are out of
        /// available time and so stop the search.
        //static int lastInfoTime;
        internal static void check_time()
        {
            if (lastInfoTime.ElapsedMilliseconds >= 1000)
            {
                lastInfoTime.Reset();
                lastInfoTime.Start();
                Utils.dbg_print();
            }

            if (Limits.ponder)
            {
                return;
            }

            long nodes = 0;
            if (Limits.nodes != 0)
            {
                ThreadHelper.lock_grab(Threads.splitLock);

                nodes = RootPos.nodes;

                // Loop across all split points and sum accumulated SplitPoint nodes plus
                // all the currently active positions nodes.
                for (var i = 0; i < Threads.size(); i++)
                {
                    for (int j = 0; j < Threads.thread(i).splitPointsSize; j++)
                    {
                        SplitPoint sp = Threads.thread(i).splitPoints[j];

                        ThreadHelper.lock_grab(sp.Lock);

                        nodes += sp.nodes;
                        Bitboard sm = sp.slavesMask;
                        while (sm != 0)
                        {
                            Position pos = Threads.threads[Utils.pop_lsb(ref sm)].activePosition;
                            nodes += pos?.nodes ?? 0;
                        }

                        ThreadHelper.lock_release(sp.Lock);
                    }
                }

                ThreadHelper.lock_release(Threads.splitLock);
            }

            var elapsed = SearchTime.ElapsedMilliseconds;

            var stillAtFirstMove = SignalsFirstRootMove && !SignalsFailedLowAtRoot && elapsed > TimeMgr.available_time();

            var noMoreTime = elapsed > TimeMgr.maximum_time() - 2 * TimerResolution || stillAtFirstMove;

            if ((Limits.use_time_management() && noMoreTime)
                || (Limits.movetime != 0 && elapsed >= Limits.movetime)
                || (Limits.nodes != 0 && nodes >= Limits.nodes))
            {
                SignalsStop = true;
            }
        }

        // Returns the position of the first found item
        internal static int find(List<RootMove> RootMoves, int firstPos, int lastPos, int moveToFind)
        {
            for (var i = firstPos; i < lastPos; i++)
            {
                if (RootMoves[i].pv[0] == moveToFind)
                {
                    return i;
                }
            }
            return -1;
        }
    }
}