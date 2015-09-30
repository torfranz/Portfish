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

        internal int eval;

        internal int evalMargin;

        internal int skipNullMove;
    };

    /// The LimitsType struct stores information sent by GUI about available time
    /// to search the current move, maximum depth/time, if we are in analysis mode
    /// or if we have to ponder while is our opponent's side to move.
    internal sealed class LimitsType
    {
        internal readonly int[] inc = new int[2];

        internal readonly int[] time = new int[2];

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
            return (this.movetime + this.depth + this.nodes + this.infinite) == 0;
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
            var ply = 1;
            var m = this.pv[0];

            Debug.Assert(m != MoveC.MOVE_NONE && pos.is_pseudo_legal(m));

            this.pv.Clear();
            this.pv.Add(m);
            pos.do_move(m, sia.state[stPos++]);

            uint ttePos = 0;

            while (TT.probe(pos.key(), ref ttePos, out tte) && (m = tte.move()) != MoveC.MOVE_NONE
                   // Local copy, TT entry could change
                   && pos.is_pseudo_legal(m) && pos.pl_move_is_legal(m, pos.pinned_pieces()) && ply < Constants.MAX_PLY
                   && (!pos.is_draw(false) || ply < 2))
            {
                this.pv.Add(m);
                pos.do_move(m, sia.state[stPos++]);
                ply++;
            }
            this.pv.Add(MoveC.MOVE_NONE);

            do pos.undo_move(this.pv[--ply]);
            while (ply != 0);

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
            ulong k;
            int v, m = ValueC.VALUE_NONE;
            var ply = 0;
            uint ttePos = 0;

            Debug.Assert(this.pv[ply] != MoveC.MOVE_NONE && pos.is_pseudo_legal(this.pv[ply]));

            do
            {
                k = pos.key();
                tteHasValue = TT.probe(k, ref ttePos, out tte);

                // Don't overwrite existing correct entries
                if ((!tteHasValue) || tte.move() != this.pv[ply])
                {
                    v = (pos.in_check() ? ValueC.VALUE_NONE : Evaluate.do_evaluate(false, pos, ref m));
                    TT.store(k, ValueC.VALUE_NONE, Bound.BOUND_NONE, DepthC.DEPTH_NONE, this.pv[ply], v, m);
                }
                pos.do_move(this.pv[ply], sia.state[stPos++]);
            }
            while (this.pv[++ply] != MoveC.MOVE_NONE);

            do pos.undo_move(this.pv[--ply]);
            while (ply != 0);

            StateInfoArrayBroker.Free();
        }
    };

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

        internal static readonly Position RootPosition = new Position();

        internal static readonly RKISS rk = new RKISS();

        /// Constants

        // Lookup table to check if a Piece is a slider and its access function
        internal static readonly bool[] Slidings = new bool[18]
                                                       {
                                                           false, false, false, true, true, true, false, false, false,
                                                           false, false, true, true, true, false, false, false, false
                                                       };

        internal static bool piece_is_slider(int p)
        {
            return Slidings[p];
        }

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
        private static int MultiPV, UCIMultiPV, PVIdx; // was UInt64

        private static int BestMoveChanges;

        private static int SkillLevel;

        private static bool SkillLevelEnabled, Chess960;

        private static readonly History H = new History();

        // is_dangerous() checks whether a move belongs to some classes of known
        // 'dangerous' moves so that we avoid to prune it.
        private static bool is_dangerous(Position pos, int m, bool captureOrPromotion)
        {
            // Test for a pawn pushed to 7th or a passed pawn move
            if ((pos.board[((m >> 6) & 0x3F)] & 7) == PieceTypeC.PAWN)
            {
                var c = pos.sideToMove;
                if (Utils.relative_rank_CS(c, (m & 0x3F)) == RankC.RANK_7
                    || (((pos.byTypeBB[PieceTypeC.PAWN] & pos.byColorBB[c ^ 1]) & Utils.PassedPawnMask[c][(m & 0x3F)])
                        == 0))
                {
                    return true;
                }
            }

            // Castle move?
            if (Utils.type_of_move(m) == MoveTypeC.CASTLING)
            {
                return true;
            }
            
            // Passed pawn move?
            if (Utils.type_of(pos.piece_moved(m)) == PieceTypeC.PAWN && pos.pawn_is_passed(pos.sideToMove, Utils.to_sq(m)))
            {
                return true;
            }

            // Entering a pawn endgame?
            if (captureOrPromotion && (pos.board[m & 0x3F] & 7) != PieceTypeC.PAWN && Utils.type_of_move(m) == MoveTypeC.NORMAL
                && (pos.st.npMaterialWHITE + pos.st.npMaterialBLACK - Position.PieceValue[Constants.Midgame][pos.board[m & 0x3F]]
                    == ValueC.VALUE_ZERO))
            {
                return true;
            }

            return false;
        }

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
        /// searches from RootPosition and at the end prints the "bestmove" to output.
        internal static void think()
        {
            var pos = RootPosition;
            Chess960 = pos.chess960;
            //SearchTime.Restart();
            Evaluate.RootColor = pos.sideToMove;
            Evaluate.ValueDraw[Evaluate.RootColor] = ValueC.VALUE_DRAW - Evaluate.ContemptFactor;
            Evaluate.ValueDraw[~Evaluate.RootColor] = ValueC.VALUE_DRAW + Evaluate.ContemptFactor;
            TimeMgr.init(Limits, pos.startpos_ply_counter(), pos.sideToMove);
            TT.new_search();
            H.clear();

            if (RootMoves.Count == 0)
            {
                Plug.Write("info depth 0 score ");
                Plug.Write(score_to_uci(pos.in_check() ? -ValueC.VALUE_MATE : ValueC.VALUE_DRAW));
                Plug.Write(Constants.endl);

                RootMoves.Add(new RootMove(MoveC.MOVE_NONE));
                goto finalize;
            }

            if ((bool.Parse(OptionMap.Instance["OwnBook"].v)) && (Limits.infinite == 0))
            {
                var bookMove = PolyglotBook.probe(
                    pos,
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

            UCIMultiPV = int.Parse(OptionMap.Instance["MultiPV"].v);
            SkillLevel = int.Parse(OptionMap.Instance["Skill Level"].v);

            // Do we have to play with skill handicap? In this case enable MultiPV that
            // we will use behind the scenes to retrieve a set of possible moves.
            SkillLevelEnabled = (SkillLevel < 20);
            MultiPV = (SkillLevelEnabled ? Math.Max(UCIMultiPV, 4) : UCIMultiPV);

            var ttSize = uint.Parse(OptionMap.Instance["Hash"].v);
            if (TT.size != ttSize)
            {
                TT.set_size(ttSize);
            }

            Threads.wake_up();

            // Set best timer interval to avoid lagging under time pressure. Timer is
            // used to check for remaining available thinking time.
            if (Limits.use_time_management())
            {
                Threads.set_timer(Math.Min(100, Math.Max(TimeMgr.available_time() / 16, TimerResolution)));
            }
            else if (Limits.nodes != 0)
            {
                Threads.set_timer(2 * TimerResolution);
            }
            else
            {
                Threads.set_timer(100);
            }

            // We're ready to start searching. Call the iterative deepening loop function
            id_loop(pos);

            // Stop timer and send all the slaves to sleep, if not already sleeping
            Threads.set_timer(0); // Stop timer
            Threads.sleep();

            finalize:

            // When we reach max depth we arrive here even without Signals.stop is raised,
            // but if we are pondering or in infinite search, we shouldn't print the best
            // move before we are told to do so.
            if (!SignalsStop && (Limits.ponder || (Limits.infinite != 0)))
            {
                pos.this_thread().wait_for_stop_or_ponderhit();
            }

            // Best move could be MOVE_NONE when searching on a stalemate position
            Plug.Write("bestmove ");
            Plug.Write(Utils.move_to_uci(RootMoves[0].pv[0], Chess960));
            Plug.Write(" ponder ");
            Plug.Write(Utils.move_to_uci(RootMoves[0].pv[1], Chess960));
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
            var skillBest = MoveC.MOVE_NONE;

            depth = BestMoveChanges = 0;
            bestValue = delta = -ValueC.VALUE_INFINITE;
            ss[ssPos].currentMove = MoveC.MOVE_NULL;

            // Iterative deepening loop until requested to stop or target depth reached
            while (!SignalsStop && ++depth <= Constants.MAX_PLY && ((Limits.depth == 0) || depth <= Limits.depth))
            {
                // Save last iteration's scores before first PV line is searched and all
                // the move scores but the (new) PV are set to -VALUE_INFINITE.
                for (var i = 0; i < RootMoves.Count; i++)
                {
                    RootMoves[i].prevScore = RootMoves[i].score;
                }

                prevBestMoveChanges = BestMoveChanges;
                BestMoveChanges = 0;

                // MultiPV loop. We perform a full root search for each PV line
                for (PVIdx = 0; PVIdx < Math.Min(MultiPV, RootMoves.Count); PVIdx++)
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
                    while(true)
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

                        // In case we have found an exact score and we are going to leave
                        // the fail high/low loop then reorder the PV moves, otherwise
                        // leave the last PV move in its position so to be searched again.
                        // Of course this is needed only in MultiPV search.
                        if ((PVIdx != 0) && bestValue > alpha && bestValue < beta)
                        {
                            Utils.sort(RootMoves, 0, PVIdx);
                        }

                        // Write PV back to transposition table in case the relevant
                        // entries have been overwritten during the search.
                        for (var i = 0; i <= PVIdx; i++)
                        {
                            RootMoves[i].insert_pv_in_tt(pos);
                        }

                        // If search has been stopped exit the aspiration window loop.
                        // Sorting and writing PV back to TT is safe becuase RootMoves
                        // is still valid, although refers to previous iteration.
                        if (SignalsStop)
                        {
                            break;
                        }

                        // Send full PV info to GUI if we are going to leave the loop or
                        // if we have a fail high/low and we are deep in the search.
                        if ((bestValue > alpha && bestValue < beta) || SearchTime.ElapsedMilliseconds > 2000)
                        {
                            pv_info_to_uci(pos, depth, alpha, beta);
                        }

                        // In case of failing high/low increase aspiration window and
                        // research, otherwise exit the fail high/low loop.
                        if (bestValue >= beta)
                        {
                            beta += delta;
                            delta += delta / 2;
                        }
                        else if (bestValue <= alpha)
                        {
                            SignalsFailedLowAtRoot = true;
                            SignalsStopOnPonderhit = false;

                            alpha -= delta;
                            delta += delta / 2;
                        }
                        else
                        {
                            break;
                        }

                        // Search with full window in case we have a win/mate score
                        if (Math.Abs(bestValue) >= ValueC.VALUE_KNOWN_WIN)
                        {
                            alpha = -ValueC.VALUE_INFINITE;
                            beta = ValueC.VALUE_INFINITE;
                        }

                        Debug.Assert(alpha >= -ValueC.VALUE_INFINITE && beta <= ValueC.VALUE_INFINITE);
                    }
                }

                // Skills: Do we need to pick now the best move ?
                if (SkillLevelEnabled && depth == 1 + SkillLevel)
                {
                    skillBest = do_skill_level();
                }

                // Filter out startup noise when monitoring best move stability
                if (depth > 2 && (BestMoveChanges != 0))
                {
                    bestMoveNeverChanged = false;
                }

                // Do we have time for the next iteration? Can we stop searching now?
                if (!SignalsStop && !SignalsStopOnPonderhit && Limits.use_time_management())
                {
                    var stop = false; // Local variable, not the volatile Signals.stop

                    // Take in account some extra time if the best move has changed
                    if (depth > 4 && depth < 50)
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
                    if (depth >= 12 && !stop
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

            // When using skills swap best PV line with the sub-optimal one
            if (SkillLevelEnabled)
            {
                if (skillBest == MoveC.MOVE_NONE) // Still unassigned ?
                {
                    skillBest = do_skill_level();
                }

                var bestpos = find(RootMoves, 0, RootMoves.Count, skillBest);
                var temp = RootMoves[0];
                RootMoves[0] = RootMoves[bestpos];
                RootMoves[bestpos] = temp;
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
            int refinedValue, nullValue, futilityValue;
            bool inCheck, givesCheck, pvMove, singularExtensionNode;
            bool captureOrPromotion, dangerous, doFullDepthSearch;
            int moveCount = 0, playedMoveCount = 0;
            SplitPoint sp = null;

            // Step 1. Initialize node
            var thisThread = pos.this_thread();
            
            inCheck = pos.in_check();
            
            if (SpNode)
            {
                sp = ss[ssPos].sp;
                bestMove = sp.bestMove;
                threatMove = sp.threatMove;
                bestValue = sp.bestValue;
                ttMove = excludedMove = MoveC.MOVE_NONE;
                ttValue = ValueC.VALUE_NONE;

                ////moveCount = sp.moveCount; // Lock must be held here

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
                    return Evaluate.ValueDraw[pos.sideToMove];
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
            if (!RootNode && tteHasValue
                && (PvNode
                        ? tte.depth() >= depth && tte.type() == Bound.BOUND_EXACT
                        : can_return_tt(tte, depth, ttValue, beta)))
            {
                TT.entries[ttePos].set_generation(TT.generation);
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

            // Step 5. Evaluate the position statically and update parent's gain statistics
            if (inCheck)
            {
                ss[ssPos].eval = ss[ssPos].evalMargin = refinedValue = ValueC.VALUE_NONE;
            }
            else if (tteHasValue)
            {
                Debug.Assert(tte.static_value() != ValueC.VALUE_NONE);
                ss[ssPos].eval = tte.static_value();
                ss[ssPos].evalMargin = tte.static_value_margin();
                refinedValue = refine_eval(tte, ttValue, ss[ssPos].eval);
            }
            else
            {
                refinedValue = ss[ssPos].eval = Evaluate.do_evaluate(false, pos, ref ss[ssPos].evalMargin);
                TT.store(
                    posKey,
                    ValueC.VALUE_NONE,
                    Bound.BOUND_NONE,
                    DepthC.DEPTH_NONE,
                    MoveC.MOVE_NONE,
                    ss[ssPos].eval,
                    ss[ssPos].evalMargin);
            }

            // Update gain for the parent non-capture move given the static position
            // evaluation before and after the move.
            if ((move = ss[ssPos - 1].currentMove) != MoveC.MOVE_NULL && ss[ssPos - 1].eval != ValueC.VALUE_NONE
                && ss[ssPos].eval != ValueC.VALUE_NONE && (pos.captured_piece_type() == 0) && Utils.type_of_move(move) == MoveTypeC.NORMAL)
            {
                var to = Utils.to_sq(move);
                H.update_gain(pos.piece_on(to), to, -ss[ssPos - 1].eval - ss[ssPos].eval);
            }

            // Step 6. Razoring (is omitted in PV nodes)
            if (!PvNode && !inCheck && depth < 4 * DepthC.ONE_PLY && refinedValue + razor_margin(depth) < beta
                && ttMove == MoveC.MOVE_NONE && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY
                && !pos.pawn_on_7th(pos.sideToMove))
            {
                var rbeta = beta - razor_margin(depth);
                var v = qsearch(NodeTypeC.NonPV, pos, ss, ssPos, rbeta - 1, rbeta, DepthC.DEPTH_ZERO);
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
                && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY && refinedValue - FutilityMargins[depth][0] >= beta
                && (pos.non_pawn_material(pos.sideToMove) != 0))
            {
                MovesSearchedBroker.Free();
                return refinedValue - FutilityMargins[depth][0];
            }

            // Step 8. Null move search with verification search (is omitted in PV nodes)
            if (!PvNode && !inCheck && (ss[ssPos].skipNullMove == 0) && depth > DepthC.ONE_PLY && refinedValue >= beta
                && Math.Abs(beta) < ValueC.VALUE_MATE_IN_MAX_PLY && (pos.non_pawn_material(pos.sideToMove) != 0))
            {
                ss[ssPos].currentMove = MoveC.MOVE_NULL;

                // Null move dynamic reduction based on depth
                Depth R = 3 * DepthC.ONE_PLY + depth / 4;

                // Null move dynamic reduction based on value
                if (refinedValue - Constants.PawnValueMidgame > beta)
                {
                    R += DepthC.ONE_PLY;
                }

                if (st == null)
                {
                    st = StateInfoBroker.GetObject();
                }
                pos.do_null_move(true, st);
                ss[ssPos + 1].skipNullMove = 1;
                
                nullValue = depth - R < DepthC.ONE_PLY ? -qsearch(NodeTypeC.NonPV, pos, ss, ssPos + 1, -beta, -alpha, DepthC.DEPTH_ZERO)
                                      : -search(NodeTypeC.NonPV, pos, ss, ssPos + 1, -beta, -alpha, depth - R);

                ss[ssPos + 1].skipNullMove = 0;
                pos.do_null_move(false, st);

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
                    // move which was reduced. If a connection is found, return a fail
                    // low score (which will cause the reduced move to fail high in the
                    // parent node, which will trigger a re-search with full depth).
                    threatMove = ss[ssPos + 1].currentMove;

                    if (depth < 5 * DepthC.ONE_PLY && (ss[ssPos - 1].reduction != 0) && threatMove != MoveC.MOVE_NONE
                        && connected_moves(pos, ss[ssPos - 1].currentMove, threatMove))
                    {
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
                && (PvNode || (!inCheck && ss[ssPos].eval + 256 >= beta)))
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
                if ((tteHasValue) && (TT.entries[ttePos].key == tte.key))
                {
                    tte = TT.entries[ttePos];
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
                SpNode ? ss[ssPos].sp.mp : null);
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

                    if (thisThread == Threads.main_thread() && SearchTime.ElapsedMilliseconds > 2000)
                    {
                        Plug.Write("info depth ");
                        Plug.Write((depth / DepthC.ONE_PLY).ToString());
                        Plug.Write(" currmove ");
                        Plug.Write(Utils.move_to_uci(move, Chess960));
                        Plug.Write(" nodes ");
                        Plug.Write(pos.nodes.ToString());
                        Plug.Write(" currmovenumber ");
                        Plug.Write((moveCount + PVIdx).ToString());
                        Plug.Write(Constants.endl);
                    }
                }

                captureOrPromotion = pos.is_capture_or_promotion(move);
                givesCheck = pos.move_gives_check(move, ci);
                dangerous = givesCheck || is_dangerous(pos, move, captureOrPromotion);
                ext = DepthC.DEPTH_ZERO;

                // Step 12. Extend checks and, in PV nodes, also dangerous moves
                if (PvNode && dangerous)
                {
                    ext = DepthC.ONE_PLY;
                }

                else if (givesCheck && pos.see(move, true) >= 0)
                {
                    ext = DepthC.ONE_PLY / 2;
                }

                // Singular extension search. If all moves but one fail low on a search of
                // (alpha-s, beta-s), and just one fails high on (alpha, beta), then that move
                // is singular and should be extended. To verify this we do a reduced search
                // on all the other moves but the ttMove, if result is lower than ttValue minus
                // a margin then we extend ttMove.
                if (singularExtensionNode && (ext == 0) && move == ttMove && pos.pl_move_is_legal(move, ci.pinned))
                {
                    var rBeta = ttValue - depth;
                    ss[ssPos].excludedMove = move;
                    ss[ssPos].skipNullMove = 1;
                    value = search(NodeTypeC.NonPV, pos, ss, ssPos, rBeta - 1, rBeta, depth / 2);
                    ss[ssPos].skipNullMove = 0;
                    ss[ssPos].excludedMove = MoveC.MOVE_NONE;
                    if (value < rBeta)
                    {
                        ext = rBeta >= beta ? DepthC.ONE_PLY + DepthC.ONE_PLY / 2 : DepthC.ONE_PLY;
                    }
                }

                // Update current move (this must be done after singular extension search)
                newDepth = depth - DepthC.ONE_PLY + ext;

                // Step 13. Futility pruning (is omitted in PV nodes)
                if (!PvNode && !inCheck && !captureOrPromotion && !dangerous && move != ttMove
                    && (bestValue > ValueC.VALUE_MATED_IN_MAX_PLY || bestValue == -ValueC.VALUE_INFINITE))
                {
                    // Move count based pruning
                    if (depth < 16 * DepthC.ONE_PLY
                        && moveCount >= FutilityMoveCounts[depth]
                        && ((threatMove == 0) || !connected_threat(pos, move, threatMove)))
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
                    futilityValue = ss[ssPos].eval + ss[ssPos].evalMargin + futility_margin(predictedDepth, moveCount)
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
                if (!pos.pl_move_is_legal(move, ci.pinned))
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
                if (!pvMove && !captureOrPromotion && !dangerous && ss[ssPos].killers0 != move
                    && ss[ssPos].killers1 != move && depth > 3 * DepthC.ONE_PLY)
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
                                ? -qsearch(NodeTypeC.NonPV, pos, ss, ssPos + 1, -(alpha + 1), -alpha, DepthC.DEPTH_ZERO)
                                : -search(NodeTypeC.NonPV, pos, ss, ssPos + 1, -(alpha + 1), -alpha, newDepth);
                }

                // Only for PV nodes do a full PV search on the first move or after a fail
                // high, in the latter case search only if value < beta, otherwise let the
                // parent node to fail low with value <= alpha and to try another move.
                if (PvNode && (pvMove || (value > alpha && (RootNode || value < beta))))
                {
                    value = newDepth < DepthC.ONE_PLY
                                ? -qsearch(NodeTypeC.PV, pos, ss, ssPos + 1, -beta, -alpha, DepthC.DEPTH_ZERO)
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
                if (RootNode && !SignalsStop)
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
                        if (!pvMove && MultiPV == 1)
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
                        else // Fail high
                        {
                            if (SpNode) sp.cutoff = true;
                            break;
                        }
                    }
                }

                // Step 19. Check for split
                if (!SpNode && depth >= Threads.min_split_depth() && bestValue < beta
                    && Threads.available_slave_exists(thisThread))
                {
                    bestValue = Threads.split(
                        Constants.FakeSplit,
                        pos,
                        ss,
                        ssPos,
                        alpha,
                        beta,
                        bestValue,
                        ref bestMove,
                        depth,
                        threatMove,
                        moveCount,
                        mp,
                        NT);
                    break;
                }
            }

            // Step 20. Check for mate and stalemate
            // All legal moves have been searched and if there are no legal moves, it
            // must be mate or stalemate. Note that we can have a false positive in
            // case of Signals.stop or thread.cutoff_occurred() are set, but this is
            // harmless because return value is discarded anyhow in the parent nodes.
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
                return (excludedMove != 0) ? alpha : inCheck ? Utils.mated_in(ss[ssPos].ply) : ValueC.VALUE_DRAW;
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
                    ss[ssPos].eval,
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
                TT.store(posKey, value_to_tt(bestValue, ss[ssPos].ply), PvNode && bestMove != MoveC.MOVE_NONE ? Bound.BOUND_EXACT : Bound.BOUND_UPPER, depth, bestMove, ss[ssPos].eval, ss[ssPos].evalMargin);
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
        private static int qsearch(int NT, Position pos, Stack[] ss, int ssPos, int alpha, int beta, int depth)
        {
            var PvNode = (NT == NodeTypeC.PV);

            Debug.Assert(NT == NodeTypeC.PV || NT == NodeTypeC.NonPV);
            Debug.Assert(alpha >= -ValueC.VALUE_INFINITE && alpha < beta && beta <= ValueC.VALUE_INFINITE);
            Debug.Assert(PvNode || (alpha == beta - 1));
            Debug.Assert(depth <= DepthC.DEPTH_ZERO);

            StateInfo st = null;
            int ttMove, move, bestMove;
            int ttValue, bestValue, value, futilityValue, futilityBase;

            bool inCheck, enoughMaterial, givesCheck, evasionPrunable;
            var tteHasValue = false;
            TTEntry tte;
            uint ttePos = 0;
            int ttDepth;

            inCheck = pos.in_check();

            ss[ssPos].currentMove = bestMove = MoveC.MOVE_NONE;
            ss[ssPos].ply = ss[ssPos - 1].ply + 1;

            // Check for an instant draw or maximum ply reached
            if (pos.is_draw(true) || ss[ssPos].ply > Constants.MAX_PLY)
            {
                return Evaluate.ValueDraw[pos.sideToMove];
            }

            // Transposition table lookup. At PV nodes, we don't use the TT for
            // pruning, but only for move ordering.
            tteHasValue = TT.probe(pos.key(), ref ttePos, out tte);
            ttMove = (tteHasValue ? tte.move() : MoveC.MOVE_NONE);
            ttValue = tteHasValue ? value_from_tt(tte.value(), ss[ssPos].ply) : ValueC.VALUE_ZERO;

            // Decide whether or not to include checks, this fixes also the type of
            // TT entry depth that we are going to use. Note that in qsearch we use
            // only two types of depth in TT: DEPTH_QS_CHECKS or DEPTH_QS_NO_CHECKS.
            ttDepth = (inCheck || depth >= DepthC.DEPTH_QS_CHECKS ? DepthC.DEPTH_QS_CHECKS : DepthC.DEPTH_QS_NO_CHECKS);
            
            if (!PvNode && tteHasValue && can_return_tt(tte, ttDepth, ttValue, beta))
            {
                ss[ssPos].currentMove = ttMove; // Can be MOVE_NONE
                return ttValue;
            }

            // Evaluate the position statically
            if (inCheck)
            {
                ss[ssPos].eval = ss[ssPos].evalMargin = ValueC.VALUE_NONE;
                bestValue = futilityBase = -ValueC.VALUE_INFINITE;
                enoughMaterial = false;
            }
            else
            {
                if (tteHasValue)
                {
                    Debug.Assert(tte.static_value() != ValueC.VALUE_NONE);
                    ss[ssPos].eval = bestValue = tte.static_value();
                    ss[ssPos].evalMargin = tte.static_value_margin();
                }
                else
                {
                    ss[ssPos].eval = bestValue = Evaluate.do_evaluate(false, pos, ref ss[ssPos].evalMargin);
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
                            ss[ssPos].eval,
                            ss[ssPos].evalMargin);
                    }

                    return bestValue;
                }

                if (PvNode && bestValue > alpha)
                {
                    alpha = bestValue;
                }

                futilityBase = ss[ssPos].eval + ss[ssPos].evalMargin + 128;
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
                if (!PvNode && !inCheck && !givesCheck && move != ttMove && enoughMaterial
                    && Utils.type_of_move(move) != MoveTypeC.PROMOTION && !pos.is_passed_pawn_push(move))
                {
                    futilityValue = futilityBase + Position.PieceValue[Constants.Endgame][pos.board[move & 0x3F]]
                                    + (Utils.type_of_move(move) == MoveTypeC.ENPASSANT
                                           ? Constants.PawnValueEndgame
                                           : ValueC.VALUE_ZERO);

                    if (futilityValue < beta)
                    {
                        if (futilityValue > bestValue)
                        {
                            bestValue = futilityValue;
                        }

                        continue;
                    }

                    // Prune moves with negative or equal SEE
                    if (futilityBase < beta && depth < DepthC.DEPTH_ZERO && pos.see(move, false) <= 0)
                    {
                        continue;
                    }
                }

                // Detect non-capture evasions that are candidate to be pruned
                evasionPrunable = !PvNode && inCheck && bestValue > ValueC.VALUE_MATED_IN_MAX_PLY
                                  && !(((pos.board[move & 0x3F] != PieceC.NO_PIECE)
                                        && !((move & (3 << 14)) == (3 << 14))) || ((move & (3 << 14)) == (2 << 14)))
                                  && ((pos.st.castleRights & (CastleRightC.WHITE_ANY << (pos.sideToMove << 1))) == 0);

                // Don't search moves with negative SEE values
                if (!PvNode && move != ttMove && (!inCheck || evasionPrunable) && Utils.type_of_move(move) != MoveTypeC.PROMOTION
                    && pos.see(move, true) < 0)
                {
                    continue;
                }

                // Don't search useless checks
                if (!PvNode && !inCheck && givesCheck && move != ttMove
                    && !(((move & (3 << 14)) != 0)
                             ? ((move & (3 << 14)) != (3 << 14))
                             : (pos.board[move & 0x3F] != PieceC.NO_PIECE))
                    && ss[ssPos].eval + Constants.PawnValueMidgame / 4 < beta
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
                value = -qsearch(NT, pos, ss, ssPos + 1, -beta, -alpha, depth - DepthC.ONE_PLY);
                pos.undo_move(move);

                Debug.Assert(value > -ValueC.VALUE_INFINITE && value < ValueC.VALUE_INFINITE);

                // Check for new best move
                if (value > bestValue)
                {
                    bestValue = value;
                    bestMove = move;

                    if (value > alpha)
                    {
                        if (PvNode && value < beta) // Update alpha here! Always alpha < beta
                        {
                            alpha = value;
                            bestMove = move;
                        }
                        else // Fail high
                        {
                            TT.store(pos.key(), value_to_tt(value, ss[ssPos].ply), Bound.BOUND_LOWER, ttDepth, move, ss[ssPos].eval, ss[ssPos].evalMargin);
                            return value;
                        }
                    }
                }
            }

            // All legal moves have been searched. A special case: If we're in check
            // and no legal moves were found, it is checkmate.
            if (inCheck && bestValue == -ValueC.VALUE_INFINITE)
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

            TT.store(pos.key(), value_to_tt(bestValue, ss[ssPos].ply), PvNode && bestMove != MoveC.MOVE_NONE ? Bound.BOUND_EXACT : Bound.BOUND_UPPER, ttDepth, bestMove, ss[ssPos].eval, ss[ssPos].evalMargin);

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
            ulong b, occ, oldAtt, newAtt, kingAtt;
            int from, to, ksq;
            int pc;
            int them;

            from = Utils.from_sq(move);
            to = Utils.to_sq(move);
            them = Utils.flip_C(pos.sideToMove);
            ksq = pos.king_square(them);
            kingAtt = Position.attacks_from_KING(ksq);
            pc = pos.piece_moved(move);

            occ = pos.occupied_squares ^ Utils.SquareBB[from] ^ Utils.SquareBB[ksq];
            oldAtt = Position.attacks_from(pc, from, occ);
            newAtt = Position.attacks_from(pc, to, occ);

            // Rule 1. Checks which give opponent's king at most one escape square are dangerous
            b = kingAtt & ~pos.pieces_C(them) & ~newAtt & ~(1UL << to);

            if ((b & (b - 1)) == 0) // Catches also !b
            {
                return true;
            }

            // Rule 2. Queen contact check is very dangerous
            if (Utils.type_of(pc) == PieceTypeC.QUEEN && (Utils.bit_is_set(kingAtt, to) != 0))
            {
                return true;
            }

            // Rule 3. Creating new double threats with checks
            b = pos.pieces_C(them) & newAtt & ~oldAtt & ~(1UL << ksq);

            while (b != 0)
            {
                // Note that here we generate illegal "double move"!
                if (futilityBase + Position.PieceValue[Constants.Endgame][pos.piece_on(Utils.pop_lsb(ref b))] >= beta)
                {
                    return true;
                }
            }
            return false;
        }

        // connected_moves() tests whether two moves are 'connected' in the sense
        // that the first move somehow made the second move possible (for instance
        // if the moving piece is the same in both moves). The first move is assumed
        // to be the move that was made to reach the current position, while the
        // second move is assumed to be a move from the current position.
        internal static bool connected_moves(Position pos, int m1, int m2)
        {
            int f1, t1, f2, t2;
            int p1, p2;
            int ksq;

            Debug.Assert(Utils.is_ok_M(m1));
            Debug.Assert(Utils.is_ok_M(m2));

            // Case 1: The moving piece is the same in both moves
            f2 = Utils.from_sq(m2);
            t1 = Utils.to_sq(m1);
            if (f2 == t1)
            {
                return true;
            }

            // Case 2: The destination square for m2 was vacated by m1
            t2 = Utils.to_sq(m2);
            f1 = Utils.from_sq(m1);
            if (t2 == f1)
            {
                return true;
            }

            // Case 3: Moving through the vacated square
            p2 = pos.piece_on(f2);
            if (piece_is_slider(p2) && (Utils.bit_is_set(Utils.between_bb(f2, t2), f1) != 0))
            {
                return true;
            }

            // Case 4: The destination square for m2 is defended by the moving piece in m1
            p1 = pos.piece_on(t1);
            if ((Utils.bit_is_set(pos.attacks_from_PS(p1, t1), t2)) != 0)
            {
                return true;
            }

            // Case 5: Discovered check, checking piece is the piece moved in m1
            ksq = pos.king_square(pos.sideToMove);
            if (piece_is_slider(p1) && (Utils.bit_is_set(Utils.between_bb(t1, ksq), f2) != 0)
                && (Utils.bit_is_set(Position.attacks_from(p1, t1, Utils.xor_bit(pos.occupied_squares, f2)), ksq) != 0))
            {
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
            if (v >= ValueC.VALUE_MATE_IN_MAX_PLY)
            {
                return v + ply;
            }

            if (v <= ValueC.VALUE_MATED_IN_MAX_PLY)
            {
                return v - ply;
            }

            return v;
        }

        // value_from_tt() is the inverse of value_to_tt(): It adjusts a mate score
        // from the transposition table (where refers to the plies to mate/be mated
        // from current position) to "plies to mate/be mated from the root".
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private static int value_from_tt(int v, int ply)
        {
            if (v >= ValueC.VALUE_MATE_IN_MAX_PLY)
            {
                return v - ply;
            }

            if (v <= ValueC.VALUE_MATED_IN_MAX_PLY)
            {
                return v + ply;
            }

            return v;
        }

        // connected_threat() tests whether it is safe to forward prune a move or if
        // is somehow connected to the threat move returned by null search.
        private static bool connected_threat(Position pos, int m, int threat)
        {
            Debug.Assert(Utils.is_ok_M(m));
            Debug.Assert(Utils.is_ok_M(threat));
            Debug.Assert(!pos.is_capture_or_promotion(m));
            Debug.Assert(!pos.is_passed_pawn_push(m));

            int mfrom, mto, tfrom, tto;

            mfrom = Utils.from_sq(m);
            mto = Utils.to_sq(m);
            tfrom = Utils.from_sq(threat);
            tto = Utils.to_sq(threat);

            // Case 1: Don't prune moves which move the threatened piece
            if (mfrom == tto)
            {
                return true;
            }

            // Case 2: If the threatened piece has value less than or equal to the
            // value of the threatening piece, don't prune moves which defend it.
            if (pos.is_capture(threat)
                && (Position.PieceValue[Constants.Midgame][pos.piece_on(tfrom)] >= Position.PieceValue[Constants.Midgame][pos.piece_on(tto)]
                    || Utils.type_of(pos.piece_on(tfrom)) == PieceTypeC.KING) && pos.move_attacks_square(m, tto))
            {
                return true;
            }

            // Case 3: If the moving piece in the threatened move is a slider, don't
            // prune safe moves which block its ray.
            if (piece_is_slider(pos.piece_on(tfrom)) && (Utils.bit_is_set(Utils.between_bb(tfrom, tto), mto) != 0)
                && pos.see(m, true) >= 0)
            {
                return true;
            }

            return false;
        }

        // can_return_tt() returns true if a transposition table score can be used to
        // cut-off at a given point in search.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private static bool can_return_tt(TTEntry tte, int depth, int v, int beta)
        {
            return (tte.depth() >= depth || v >= Math.Max(ValueC.VALUE_MATE_IN_MAX_PLY, beta)
                    || v < Math.Min(ValueC.VALUE_MATED_IN_MAX_PLY, beta))
                   && ((((tte.type() & Bound.BOUND_LOWER) != 0) && v >= beta)
                       || (((tte.type() & Bound.BOUND_UPPER) != 0) && v < beta));
        }

        // refine_eval() returns the transposition table score if possible, otherwise
        // falls back on static position evaluation.
        private static int refine_eval(TTEntry tte, int v, int defaultEval)
        {
            if ((((tte.type() & Bound.BOUND_LOWER) != 0) && v >= defaultEval)
                || (((tte.type() & Bound.BOUND_UPPER) != 0) && v < defaultEval))
            {
                return v;
            }

            return defaultEval;
        }

        // pv_info_to_uci() sends search info to GUI. UCI protocol requires to send all
        // the PV lines also if are still to be searched and so refer to the previous
        // search score.
        private static void pv_info_to_uci(Position pos, int depth, int alpha, int beta)
        {
            var t = SearchTime.ElapsedMilliseconds;
            var selDepth = 0;

            for (var i = 0; i < Threads.size(); i++)
            {
                if (Threads.threads[i].maxPly > selDepth)
                {
                    selDepth = Threads.threads[i].maxPly;
                }
            }

            for (var i = 0; i < Math.Min(UCIMultiPV, RootMoves.Count); i++)
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
                    s.Append(" ").Append(Utils.move_to_uci(RootMoves[i].pv[j], Chess960));
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

        // When playing with strength handicap choose best move among the MultiPV set
        // using a statistical rule dependent on SkillLevel. Idea by Heinz van Saanen.
        internal static int do_skill_level()
        {
            Debug.Assert(MultiPV > 1);

            // PRNG sequence should be not deterministic
            for (var i = Math.Abs(DateTime.Now.Millisecond % 50); i > 0; i--)
            {
                rk.rand();
            }

            // RootMoves are already sorted by score in descending order
            var size = Math.Min(MultiPV, RootMoves.Count);
            var variance = Math.Min(RootMoves[0].score - RootMoves[size - 1].score, Constants.PawnValueMidgame);
            var weakness = 120 - 2 * SkillLevel;
            var max_s = -ValueC.VALUE_INFINITE;
            var best = MoveC.MOVE_NONE;

            // Choose best move. For each move score we add two terms both dependent on
            // weakness, one deterministic and bigger for weaker moves, and one random,
            // then we choose the move with the resulting highest score.
            for (var i = 0; i < size; i++)
            {
                var s = RootMoves[i].score;

                // Don't allow crazy blunders even at very low skills
                if (i > 0 && RootMoves[i - 1].score > s + 2 * Constants.PawnValueMidgame)
                {
                    break;
                }

                // This is our magic formula
                s += (weakness * (RootMoves[0].score - s) + variance * (int)(rk.rand() % (ulong)weakness)) / 128;

                if (s > max_s)
                {
                    max_s = s;
                    best = RootMoves[i].pv[0];
                }
            }
            return best;
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

                nodes = RootPosition.nodes;
                
                // Loop across all split points and sum accumulated SplitPoint nodes plus
                // all the currently active slaves positions.
                for (var i = 0; i < Threads.size(); i++)
                {
                    for (int j = 0; j < Threads.thread(i).splitPointsCnt; j++)
                    {
                        SplitPoint sp = Threads.thread(i).splitPoints[j];

                        ThreadHelper.lock_grab(sp.Lock);

                        nodes += sp.nodes;
                        Bitboard sm = sp.slavesMask;
                        while (sm != 0)
                        {
                            Position pos = sp.activePositions[Utils.pop_lsb(ref sm)];
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
        private static int find(List<RootMove> RootMoves, int firstPos, int lastPos, int moveToFind)
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