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

    internal static class ResultC
    {
        internal const int INVALID = 0, UNKNOWN = 1, DRAW = 2, WIN = 4;
    };

    internal struct KPKPosition
    {
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static bool probe_kpk(Square wksq, Square wpsq, Square bksq, Color stm)
        {
            Debug.Assert(Utils.file_of(wpsq) <= FileC.FILE_D);
            var idx = index(stm, bksq, wksq, wpsq);
            return (KPKBitbase[idx / 32] & (1 << (idx & 31))) != 0;
        }

        internal static void init_kpk()
        {
            var db = new KPKPosition[IndexMax];
            int idx, repeat = 1;

            // Initialize db with known win / draw positions
            for (idx = 0; idx < IndexMax; idx++)
            {
                db[idx].classify_leaf(idx);
            }

            // Iterate until all positions are classified (30 cycles needed)
            while (repeat != 0)
            {
                for (repeat = idx = 0; idx < IndexMax; idx++)
                {
                    if (db[idx].res == ResultC.UNKNOWN && (db[idx].classify(db) != ResultC.UNKNOWN))
                    {
                        repeat = 1;
                    }
                }
            }

            // Map 32 position results into one KPKBitbase[] entry
            for (idx = 0; idx < IndexMax; idx++)
            {
                if (db[idx].res == ResultC.WIN)
                {
                    KPKBitbase[idx / 32] |= (uint)(1 << (idx & 31));
                }
            }
        }

        private int wksq, bksq, psq;
        private Result res;
        private int stm;

        // The possible pawns squares are 24, the first 4 files and ranks from 2 to 7
        private const int IndexMax = 2 * 24 * 64 * 64; // stm * wp_sq * wk_sq * bk_sq = 196608

        // Each uint32_t stores results of 32 positions, one per bit
        private static readonly uint[] KPKBitbase = new uint[IndexMax / 32];

        private ulong k_attacks(int Us)
        {
            return Us == ColorC.WHITE
                       ? Utils.StepAttacksBB[PieceC.W_KING][this.wksq]
                       : Utils.StepAttacksBB[PieceC.B_KING][this.bksq];
        }

        private ulong p_attacks()
        {
            return Utils.StepAttacksBB[PieceC.W_PAWN][this.psq];
        }

        // A KPK bitbase index is an integer in [0, IndexMax] range
        //
        // Information is mapped in this way
        //
        // bit     0: side to move (WHITE or BLACK)
        // bit  1- 6: black king square (from SQ_A1 to SQ_H8)
        // bit  7-12: white king square (from SQ_A1 to SQ_H8)
        // bit 13-14: white pawn file (from FILE_A to FILE_D)
        // bit 15-17: white pawn rank - 1 (from RANK_2 - 1 to RANK_7 - 1)
        private static int index(Color stm, Square bksq, Square wksq, Square psq)
        {
            Debug.Assert(Utils.file_of(psq) <= FileC.FILE_D);
            return stm + (bksq << 1) + (wksq << 7) + (Utils.file_of(psq) << 13) + ((Utils.rank_of(psq) - 1) << 15);
        }

        private void classify_leaf(int idx)
        {
            this.stm = (idx & 1);
            this.bksq = ((idx >> 1) & 63);
            this.wksq = ((idx >> 7) & 63);
            this.psq = Utils.make_square(((idx >> 13) & 3), ((idx >> 15) + 1));

            // Check if two pieces are on the same square or if a king can be captured
            if (this.wksq == this.psq || this.wksq == this.bksq || this.bksq == this.psq
                || (Utils.bit_is_set(this.k_attacks(ColorC.WHITE), this.bksq) != 0)
                || (this.stm == ColorC.WHITE && (Utils.bit_is_set(Utils.StepAttacksBB[PieceTypeC.PAWN][psq], this.bksq) != 0)))
            {
                res = ResultC.INVALID;
            }

            // The position is an immediate win if it is white to move and the white
            // pawn can be promoted without getting captured.
            else if (Utils.rank_of(this.psq) == RankC.RANK_7 
                && this.stm == ColorC.WHITE
                && this.wksq != this.psq + SquareC.DELTA_N
                && (Utils.square_distance(this.bksq, this.psq + SquareC.DELTA_N) > 1
                    || (Utils.bit_is_set(this.k_attacks(ColorC.WHITE), (this.psq + SquareC.DELTA_N)) != 0)))
            {
                res = ResultC.WIN;
            }

            // Check for known draw positions

            // Case 1: Stalemate
            else if (this.stm == ColorC.BLACK
                && ((this.k_attacks(ColorC.BLACK) & ~(this.k_attacks(ColorC.WHITE) | Utils.StepAttacksBB[PieceTypeC.PAWN][psq])) == 0))
            {
                res = ResultC.DRAW;
            }

            // Case 2: King can capture undefended pawn
            else if (this.stm == ColorC.BLACK
                && ((Utils.bit_is_set(this.k_attacks(ColorC.BLACK), this.psq) & ~this.k_attacks(ColorC.WHITE)) != 0))
            {
                res = ResultC.DRAW;
            }

            // Case 3: Black king in front of white pawn
            else if (this.bksq == this.psq + SquareC.DELTA_N && Utils.rank_of(this.psq) < RankC.RANK_7)
            {
                res = ResultC.DRAW;
            }

            //  Case 4: White king in front of pawn and black has opposition
            else if (this.stm == ColorC.WHITE && this.wksq == this.psq + SquareC.DELTA_N
                && this.bksq == this.wksq + SquareC.DELTA_N + SquareC.DELTA_N && Utils.rank_of(this.psq) < RankC.RANK_5)
            {
                res = ResultC.DRAW;
            }

            // Case 5: Stalemate with rook pawn
            else if (this.bksq == SquareC.SQ_A8 && Utils.file_of(this.psq) == FileC.FILE_A)
            {
                res = ResultC.DRAW;
            }

            // Case 6: White king trapped on the rook file
            else if (Utils.file_of(this.wksq) == FileC.FILE_A && Utils.file_of(this.psq) == FileC.FILE_A
                && Utils.rank_of(this.wksq) > Utils.rank_of(this.psq) && this.bksq == this.wksq + 2)
            {
                res = ResultC.DRAW;
            }
            else
            {
                res = ResultC.UNKNOWN;
            }
        }

        Result classify(KPKPosition[] db)
        {
            return classify(stm, db);
        }

        private int classify(int Us, KPKPosition[] db)
        {
            // White to Move: If one move leads to a position classified as RESULT_WIN,
            // the result of the current position is RESULT_WIN. If all moves lead to
            // positions classified as RESULT_DRAW, the current position is classified
            // RESULT_DRAW otherwise the current position is classified as RESULT_UNKNOWN.
            //
            // Black to Move: If one move leads to a position classified as RESULT_DRAW,
            // the result of the current position is RESULT_DRAW. If all moves lead to
            // positions classified as RESULT_WIN, the position is classified RESULT_WIN.
            // Otherwise, the current position is classified as RESULT_UNKNOWN.

            var r = ResultC.INVALID;
            var b = this.k_attacks(Us);

            while (b != 0)
            {
                r |= Us == ColorC.WHITE
                         ? db[index(ColorC.BLACK, bksq, Utils.pop_lsb(ref b), psq)].res
                         : db[index(ColorC.WHITE, Utils.pop_lsb(ref b), wksq, psq)].res;

                if (Us == ColorC.WHITE && ((r & ResultC.WIN) != 0))
                {
                    return res = ResultC.WIN;
                }

                if (Us == ColorC.BLACK && ((r & ResultC.DRAW) != 0))
                {
                    return res = ResultC.DRAW;
                }
            }

            if (Us == ColorC.WHITE && Utils.rank_of(this.psq) < RankC.RANK_7)
            {
                var s = this.psq + SquareC.DELTA_N;
                r |= db[index(ColorC.BLACK, bksq, wksq, s)].res; // Single push

                if (Utils.rank_of(s) == RankC.RANK_3 && s != this.wksq && s != this.bksq)
                {
                    r |= db[index(ColorC.BLACK, bksq, wksq, s + SquareC.DELTA_N)].res; // Double push
                }

                if ((r & ResultC.WIN) != 0)
                {
                    return res = ResultC.WIN;
                }
            }

            return res = (r & ResultC.UNKNOWN) != 0 ? ResultC.UNKNOWN : Us == ColorC.WHITE ? ResultC.DRAW : ResultC.WIN;
        }
    };
}