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

namespace Portfish
{
    using System.Diagnostics;
    using System.Runtime.CompilerServices;

    /// MovePicker class is used to pick one pseudo legal move at a time from the
    /// current position. The most important method is next_move(), which returns a
    /// new pseudo legal move each time it is called, until there are no moves left,
    /// when MOVE_NONE is returned. In order to improve the efficiency of the alpha
    /// beta algorithm, MovePicker attempts to return the moves which are most likely
    /// to get a cut-off first.
    internal sealed class MovePicker
    {
        private Position pos;

        private History H;

        private int depth;

        private int ttMove;

        private int recaptureSquare;

        private int captureThreshold, phase;

        private int curMovePos, lastMovePos, lastQuietPos, lastBadCapturePos;

        private MovePicker mpExternal;

        private readonly MoveStack[] ms = new MoveStack[Constants.MAX_MOVES + 2];
                                     // 2 additional for the killers at the end

        private int mpos;

        public void Recycle()
        {
            this.pos = null;
            this.H = null;
            this.mpExternal = null;
        }

        #region Helpers

        // Unary predicate used by std::partition to split positive scores from remaining
        // ones so to sort separately the two sets, and with the second sort delayed.
        //internal static bool has_positive_score(ref MoveStack move) { return move.score > 0; }

        // Picks and pushes to the front the best move in range [firstMove, lastMove),
        // it is faster than sorting all the moves in advance when moves are few, as
        // normally are the possible captures.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private int pick_best()
        {
            var first = this.curMovePos;
            var max = this.curMovePos;
            if (first != this.lastMovePos)
            {
                for (; ++first != this.lastMovePos;)
                {
                    if (this.ms[max].score < this.ms[first].score)
                    {
                        max = first;
                    }
                }
            }
            var temp = this.ms[this.curMovePos];
            this.ms[this.curMovePos] = this.ms[max];
            this.ms[max] = temp;
            return this.curMovePos;
        }

        //Rearranges the elements in the range [first,last), in such a way that all the elements for which pred returns true precede all those for which it returns false. 
        // The iterator returned points to the first element of the second group.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private int partition(int first, int last)
        {
            // move elements satisfying _Pred to beginning of sequence
            for (;; ++first)
            {
                // find any out-of-order pair
                for (; first != last && (this.ms[first].score > 0); ++first)
                {
                    ; // skip in-place elements at beginning
                }
                if (first == last)
                {
                    break; // done
                }

                for (; first != --last && (this.ms[last].score <= 0);)
                {
                    ; // skip in-place elements at end
                }
                if (first == last)
                {
                    break; // done
                }

                var temp = this.ms[last];
                this.ms[last] = this.ms[first];
                this.ms[first] = temp;
            }
            return first;
        }

        #endregion

        internal static class SequencerC
        {
            internal const int MAIN_SEARCH = 0,
                               CAPTURES_S1 = 1,
                               KILLERS_S1 = 2,
                               QUIETS_1_S1 = 3,
                               QUIETS_2_S1 = 4,
                               BAD_CAPTURES_S1 = 5,
                               EVASION = 6,
                               EVASIONS_S2 = 7,
                               QSEARCH_0 = 8,
                               CAPTURES_S3 = 9,
                               QUIET_CHECKS_S3 = 10,
                               QSEARCH_1 = 11,
                               CAPTURES_S4 = 12,
                               PROBCUT = 13,
                               CAPTURES_S5 = 14,
                               RECAPTURE = 15,
                               CAPTURES_S6 = 16,
                               STOP = 17;
        };

        /// Constructors of the MovePicker class. As arguments we pass information
        /// to help it to return the presumably good moves first, to decide which
        /// moves to return (in the quiescence search, for instance, we only want to
        /// search captures, promotions and some checks) and about how important good
        /// move ordering is at the current node.
        internal void MovePickerC(Position p, int ttm, int d, History h, Stack ss, int beta, MovePicker mpExt)
        {
            this.pos = p;
            this.H = h;
            this.depth = d;
            this.mpExternal = mpExt;

            Debug.Assert(d > DepthC.DEPTH_ZERO);

            this.captureThreshold = 0;
            this.curMovePos = this.lastMovePos = 0;
            this.lastBadCapturePos = Constants.MAX_MOVES - 1;

            this.recaptureSquare = 0;
            this.lastQuietPos = 0;
            this.mpos = 0;

            if (p.in_check())
            {
                this.phase = SequencerC.EVASION;
            }
            else
            {
                this.phase = SequencerC.MAIN_SEARCH;

                this.ms[Constants.MAX_MOVES].move = ss.killers0;
                this.ms[Constants.MAX_MOVES + 1].move = ss.killers1;

                // Consider sligtly negative captures as good if at low depth and far from beta
                if (ss.staticEval < beta - Constants.PawnValueMidgame && d < 3 * DepthC.ONE_PLY)
                {
                    this.captureThreshold = -Constants.PawnValueMidgame;
                }

                // Consider negative captures as good if still enough to reach beta
                else if (ss.staticEval > beta)
                {
                    this.captureThreshold = beta - ss.staticEval;
                }
            }

            this.ttMove = (ttm != 0 && this.pos.is_pseudo_legal(ttm) ? ttm : MoveC.MOVE_NONE);
            this.lastMovePos += ((this.ttMove != MoveC.MOVE_NONE) ? 1 : 0);
        }

        internal void MovePickerC(Position p, int ttm, int d, History h, int sq)
        {
            this.pos = p;
            this.H = h;
            this.curMovePos = 0;
            this.lastMovePos = 0;

            Debug.Assert(d <= DepthC.DEPTH_ZERO);

            this.depth = 0;
            this.recaptureSquare = 0;
            this.captureThreshold = 0;
            this.lastQuietPos = 0;
            this.lastBadCapturePos = 0;
            this.mpos = 0;

            if (p.in_check())
            {
                this.phase = SequencerC.EVASION;
            }

            else if (d > DepthC.DEPTH_QS_NO_CHECKS)
            {
                this.phase = SequencerC.QSEARCH_0;
            }

            else if (d > DepthC.DEPTH_QS_RECAPTURES)
            {
                this.phase = SequencerC.QSEARCH_1;

                // Skip TT move if is not a capture or a promotion, this avoids qsearch
                // tree explosion due to a possible perpetual check or similar rare cases
                // when TT table is full.
                if ((ttm != 0) && !this.pos.is_capture_or_promotion(ttm))
                {
                    ttm = MoveC.MOVE_NONE;
                }
            }
            else
            {
                this.phase = SequencerC.RECAPTURE;
                this.recaptureSquare = sq;
                ttm = MoveC.MOVE_NONE;
            }

            this.ttMove = ((ttm != 0) && this.pos.is_pseudo_legal(ttm) ? ttm : MoveC.MOVE_NONE);
            this.lastMovePos += ((this.ttMove != MoveC.MOVE_NONE) ? 1 : 0);
        }

        internal void MovePickerC(Position p, int ttm, History h, int pt)
        {
            this.pos = p;
            this.H = h;
            this.curMovePos = 0;
            this.lastMovePos = 0;

            Debug.Assert(!this.pos.in_check());

            this.depth = 0;
            this.ttMove = 0;
            this.lastQuietPos = 0;
            this.lastBadCapturePos = 0;
            this.mpos = 0;

            this.phase = SequencerC.PROBCUT;

            // In ProbCut we generate only captures better than parentSplitPoint's captured piece
            this.captureThreshold = Position.PieceValue[PhaseC.MG][pt];
            this.ttMove = ((ttm != 0) && this.pos.is_pseudo_legal(ttm) ? ttm : MoveC.MOVE_NONE);

            if ((this.ttMove != 0)
                && (!this.pos.is_capture(this.ttMove) || this.pos.see(this.ttMove, false) <= this.captureThreshold))
            {
                this.ttMove = MoveC.MOVE_NONE;
            }

            this.lastMovePos += ((this.ttMove != MoveC.MOVE_NONE) ? 1 : 0);
        }

        /// MovePicker::score_captures(), MovePicker::score_noncaptures() and
        /// MovePicker::score_evasions() assign a numerical move ordering score
        /// to each move in a move list.  The moves with highest scores will be
        /// picked first by next_move().
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        private void score_captures()
        {
            // Winning and equal captures in the main search are ordered by MVV/LVA.
            // Suprisingly, this appears to perform slightly better than SEE based
            // move ordering. The reason is probably that in a position with a winning
            // capture, capturing a more valuable (but sufficiently defended) piece
            // first usually doesn't hurt. The opponent will have to recapture, and
            // the hanging piece will still be hanging (except in the unusual cases
            // where it is possible to recapture with the hanging piece). Exchanging
            // big pieces before capturing a hanging piece probably helps to reduce
            // the subtree size.
            // In main search we want to push captures with negative SEE values to
            // badCaptures[] array, but instead of doing it now we delay till when
            // the move has been picked up in pick_move_from_list(), this way we save
            // some SEE calls in case we get a cutoff (idea from Pablo Vazquez).
            for (var idx = 0; idx < this.lastMovePos; idx++)
            {
                var m = this.ms[idx].move;
                this.ms[idx].score = Position.PieceValue[PhaseC.MG][this.pos.board[m & 0x3F]]
                                     - (this.pos.board[((m >> 6) & 0x3F)] & 7);
                
                if (Utils.type_of_move(m) == MoveTypeC.PROMOTION)
                {
                    this.ms[idx].score += Position.PieceValue[PhaseC.MG][Utils.promotion_type(m)] - Position.PieceValue[PhaseC.MG][PieceTypeC.PAWN];
                }
                else if (Utils.type_of_move(m) == MoveTypeC.ENPASSANT)
                {
                    this.ms[idx].score += Position.PieceValue[PhaseC.MG][PieceTypeC.PAWN];
                }
            }
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private void score_noncaptures()
        {
            for (var idx = 0; idx < this.lastMovePos; idx++)
            {
                this.ms[idx].score =
                    this.H.history[this.pos.board[((this.ms[idx].move >> 6) & 0x3F)]][this.ms[idx].move & 0x3F];
            }
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private void score_evasions()
        {
            // Try good captures ordered by MVV/LVA, then non-captures if destination square
            // is not under attack, ordered by history value, then bad-captures and quiet
            // moves with a negative SEE. This last group is ordered by the SEE score.

            // Skip if we don't have at least two moves to order
            if (this.lastMovePos < 2)
            {
                return;
            }

            int seeScore;
            for (var idx = 0; idx < this.lastMovePos; idx++)
            {
                var m = this.ms[idx].move;
                if (
                    (seeScore =
                     ((Position.PieceValue[PhaseC.MG][this.pos.board[m & 0x3F]]
                       >= Position.PieceValue[PhaseC.MG][this.pos.board[((m >> 6) & 0x3F)]])
                          ? 1
                          : this.pos.see(m, false))) < 0)
                {
                    this.ms[idx].score = seeScore - History.MaxValue; // Be sure we are at the bottom
                }
                else if (((this.pos.board[m & 0x3F] != PieceC.NO_PIECE) && !((m & (3 << 14)) == (3 << 14)))
                         || ((m & (3 << 14)) == (2 << 14)))
                {
                    this.ms[idx].score = Position.PieceValue[PhaseC.MG][this.pos.board[(m & 0x3F)]]
                                         - (this.pos.board[((m >> 6) & 0x3F)] & 7) + History.MaxValue;
                }
                else
                {
                    this.ms[idx].score = this.H.value(this.pos.board[((m >> 6) & 0x3F)], (m & 0x3F));
                }
            }
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private void sort()
        {
            MoveStack tmp;
            int p, q;

            for (p = this.curMovePos + 1; p < this.lastMovePos; p++)
            {
                tmp = this.ms[p];
                for (q = p; q != this.curMovePos && this.ms[q - 1].score < tmp.score; --q)
                {
                    this.ms[q] = this.ms[q - 1];
                }
                this.ms[q] = tmp;
            }
        }

        /// MovePicker::generate_next() generates, scores and sorts the next bunch of moves,
        /// when there are no more moves to try for the current phase.
        private void generate_next()
        {
            this.curMovePos = 0;

            switch (++this.phase)
            {
                case SequencerC.CAPTURES_S1:
                case SequencerC.CAPTURES_S3:
                case SequencerC.CAPTURES_S4:
                case SequencerC.CAPTURES_S5:
                case SequencerC.CAPTURES_S6:
                    this.mpos = 0;
                    Movegen.generate_capture(this.pos, this.ms, ref this.mpos);
                    this.lastMovePos = this.mpos;
                    this.score_captures();
                    return;

                case SequencerC.KILLERS_S1:
                    this.curMovePos = Constants.MAX_MOVES; //killers[0];
                    this.lastMovePos = this.curMovePos + 2;
                    return;

                case SequencerC.QUIETS_1_S1:
                    this.mpos = 0;
                    Movegen.generate_quiet(this.pos, this.ms, ref this.mpos);
                    this.lastQuietPos = this.lastMovePos = this.mpos;
                    this.score_noncaptures();
                    this.lastMovePos = this.partition(this.curMovePos, this.lastMovePos);
                    this.sort();
                    return;

                case SequencerC.QUIETS_2_S1:
                    this.curMovePos = this.lastMovePos;
                    this.lastMovePos = this.lastQuietPos;
                    if (this.depth >= 3 * DepthC.ONE_PLY)
                    {
                        this.sort();
                    }
                    return;

                case SequencerC.BAD_CAPTURES_S1:
                    // Just pick them in reverse order to get MVV/LVA ordering
                    this.curMovePos = Constants.MAX_MOVES - 1;
                    this.lastMovePos = this.lastBadCapturePos;
                    return;

                case SequencerC.EVASIONS_S2:
                    this.mpos = 0;
                    Movegen.generate_evasion(this.pos, this.ms, ref this.mpos);
                    this.lastMovePos = this.mpos;
                    this.score_evasions();
                    return;

                case SequencerC.QUIET_CHECKS_S3:
                    this.mpos = 0;
                    Movegen.generate_quiet_check(this.pos, this.ms, ref this.mpos);
                    this.lastMovePos = this.mpos;
                    return;

                case SequencerC.EVASION:
                case SequencerC.QSEARCH_0:
                case SequencerC.QSEARCH_1:
                case SequencerC.PROBCUT:
                case SequencerC.RECAPTURE:
                    this.phase = SequencerC.STOP;
                    this.lastMovePos = this.curMovePos + 1; // Avoid another next_phase() call
                    break;

                case SequencerC.STOP:
                    this.lastMovePos = this.curMovePos + 1; // Avoid another next_phase() call
                    return;

                default:
                    Debug.Assert(false);
                    break;
            }
        }

        /// MovePicker::next_move() is the most important method of the MovePicker class.
        /// It returns a new pseudo legal move every time it is called, until there
        /// are no more moves left. It picks the move with the biggest score from a list
        /// of generated moves taking care not to return the tt move if has already been
        /// searched previously. Note that this function is not thread safe so should be
        /// lock protected by caller when accessed through a shared MovePicker object.
        internal int next_move()
        {
            if (this.mpExternal != null)
            {
                return this.mpExternal.next_move();
            }

            int move;
            var bestpos = 0;

            while (true)
            {
                while (this.curMovePos == this.lastMovePos)
                {
                    this.generate_next();
                }

                switch (this.phase)
                {
                    case SequencerC.MAIN_SEARCH:
                    case SequencerC.EVASION:
                    case SequencerC.QSEARCH_0:
                    case SequencerC.QSEARCH_1:
                    case SequencerC.PROBCUT:
                        this.curMovePos++;
                        return this.ttMove;

                    case SequencerC.CAPTURES_S1:
                        bestpos = this.pick_best();
                        this.curMovePos++;
                        move = this.ms[bestpos].move;
                        if (move != this.ttMove)
                        {
                            Debug.Assert(this.captureThreshold <= 0); // Otherwise we cannot use see_sign()

                            if (this.pos.see(move, true) >= this.captureThreshold)
                            {
                                return move;
                            }

                            // Losing capture, move it to the tail of the array
                            this.ms[this.lastBadCapturePos--].move = move;
                        }
                        break;

                    case SequencerC.KILLERS_S1:
                        move = this.ms[this.curMovePos++].move;
                        if (move != MoveC.MOVE_NONE && this.pos.is_pseudo_legal(move) && move != this.ttMove
                            && !this.pos.is_capture(move))
                        {
                            return move;
                        }
                        break;

                    case SequencerC.QUIETS_1_S1:
                    case SequencerC.QUIETS_2_S1:
                        move = this.ms[this.curMovePos++].move;
                        if (move != this.ttMove && move != this.ms[Constants.MAX_MOVES].move
                            && move != this.ms[Constants.MAX_MOVES + 1].move)
                        {
                            return move;
                        }
                        break;

                    case SequencerC.BAD_CAPTURES_S1:
                        return this.ms[this.curMovePos--].move;

                    case SequencerC.EVASIONS_S2:
                    case SequencerC.CAPTURES_S3:
                    case SequencerC.CAPTURES_S4:
                        bestpos = this.pick_best();
                        this.curMovePos++;
                        move = this.ms[bestpos].move;
                        if (move != this.ttMove)
                        {
                            return move;
                        }
                        break;

                    case SequencerC.CAPTURES_S5:
                        bestpos = this.pick_best();
                        this.curMovePos++;
                        move = this.ms[bestpos].move;
                        if (move != this.ttMove && this.pos.see(move, false) > this.captureThreshold)
                        {
                            return move;
                        }
                        break;

                    case SequencerC.CAPTURES_S6:
                        bestpos = this.pick_best();
                        this.curMovePos++;
                        move = this.ms[bestpos].move;
                        if (Utils.to_sq(move) == this.recaptureSquare)
                        {
                            return move;
                        }
                        break;

                    case SequencerC.QUIET_CHECKS_S3:
                        move = this.ms[this.curMovePos++].move;
                        if (move != this.ttMove)
                        {
                            return move;
                        }
                        break;

                    case SequencerC.STOP:
                        return MoveC.MOVE_NONE;

                    default:
                        Debug.Assert(false);
                        break;
                }
            }
        }
    }
}