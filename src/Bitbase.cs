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

        internal static uint probe_kpk_bitbase(int wksq, int wpsq, int bksq, int stm)
        {
            var idx = stm + (bksq << 1) + (wksq << 7) + ((wpsq & 7) << 13) + (((wpsq >> 3) - 1) << 15);
            return KPKBitbase[idx / 32] & (Constants.UInt32One << (idx & 31));
        }

        internal static void init()
        {
            var db = new int[IndexMax];
            var pos = new KPKPosition();
            int idx, bit, repeat = 1;

            // Initialize table with known win / draw positions
            for (idx = 0; idx < IndexMax; idx++)
            {
                db[idx] = pos.classify_leaf(idx);
            }

            // Iterate until all positions are classified (30 cycles needed)
            while (repeat != 0)
            {
                for (repeat = idx = 0; idx < IndexMax; idx++)
                {
                    if (db[idx] == ResultC.UNKNOWN && (db[idx] = pos.classify_index(idx, db)) != ResultC.UNKNOWN)
                    {
                        repeat = 1;
                    }
                }
            }

            // Map 32 position results into one KPKBitbase[] entry
            uint one = 1;
            for (idx = 0; idx < IndexMax / 32; idx++)
            {
                for (bit = 0; bit < 32; bit++)
                {
                    if (db[32 * idx + bit] == ResultC.WIN)
                    {
                        KPKBitbase[idx] |= (one << bit);
                    }
                }
            }
        }

        private int wksq, bksq, psq;

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
        private static int index(int w, int b, int p, int c)
        {
            Debug.Assert(Utils.file_of(p) <= FileC.FILE_D);
            return c + (b << 1) + (w << 7) + (Utils.file_of(p) << 13) + ((Utils.rank_of(p) - 1) << 15);
        }

        private void decode_index(int idx)
        {
            this.stm = (idx & 1);
            this.bksq = ((idx >> 1) & 63);
            this.wksq = ((idx >> 7) & 63);
            this.psq = Utils.make_square(((idx >> 13) & 3), ((idx >> 15) + 1));
        }

        private int classify_leaf(int idx)
        {
            this.decode_index(idx);

            // Check if two pieces are on the same square or if a king can be captured
            if (this.wksq == this.psq || this.wksq == this.bksq || this.bksq == this.psq
                || (Utils.bit_is_set(this.k_attacks(ColorC.WHITE), this.bksq) != 0)
                || (this.stm == ColorC.WHITE && (Utils.bit_is_set(this.p_attacks(), this.bksq) != 0)))
            {
                return ResultC.INVALID;
            }

            // The position is an immediate win if it is white to move and the white
            // pawn can be promoted without getting captured.
            if (Utils.rank_of(this.psq) == RankC.RANK_7 && this.stm == ColorC.WHITE
                && this.wksq != this.psq + SquareC.DELTA_N
                && (Utils.square_distance(this.bksq, this.psq + SquareC.DELTA_N) > 1
                    || (Utils.bit_is_set(this.k_attacks(ColorC.WHITE), (this.psq + SquareC.DELTA_N)) != 0)))
            {
                return ResultC.WIN;
            }

            // Check for known draw positions

            // Case 1: Stalemate
            if (this.stm == ColorC.BLACK
                && ((this.k_attacks(ColorC.BLACK) & ~(this.k_attacks(ColorC.WHITE) | this.p_attacks())) == 0))
            {
                return ResultC.DRAW;
            }

            // Case 2: King can capture undefended pawn
            if (this.stm == ColorC.BLACK
                && ((Utils.bit_is_set(this.k_attacks(ColorC.BLACK), this.psq) & ~this.k_attacks(ColorC.WHITE)) != 0))
            {
                return ResultC.DRAW;
            }

            // Case 3: Black king in front of white pawn
            if (this.bksq == this.psq + SquareC.DELTA_N && Utils.rank_of(this.psq) < RankC.RANK_7)
            {
                return ResultC.DRAW;
            }

            //  Case 4: White king in front of pawn and black has opposition
            if (this.stm == ColorC.WHITE && this.wksq == this.psq + SquareC.DELTA_N
                && this.bksq == this.wksq + SquareC.DELTA_N + SquareC.DELTA_N && Utils.rank_of(this.psq) < RankC.RANK_5)
            {
                return ResultC.DRAW;
            }

            // Case 5: Stalemate with rook pawn
            if (this.bksq == SquareC.SQ_A8 && Utils.file_of(this.psq) == FileC.FILE_A)
            {
                return ResultC.DRAW;
            }

            // Case 6: White king trapped on the rook file
            if (Utils.file_of(this.wksq) == FileC.FILE_A && Utils.file_of(this.psq) == FileC.FILE_A
                && Utils.rank_of(this.wksq) > Utils.rank_of(this.psq) && this.bksq == this.wksq + 2)
            {
                return ResultC.DRAW;
            }

            return ResultC.UNKNOWN;
        }

        private int classify(int Us, int[] db)
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
                         ? db[index(Utils.pop_lsb(ref b), this.bksq, this.psq, ColorC.BLACK)]
                         : db[index(this.wksq, Utils.pop_lsb(ref b), this.psq, ColorC.WHITE)];

                if (Us == ColorC.WHITE && ((r & ResultC.WIN) != 0))
                {
                    return ResultC.WIN;
                }

                if (Us == ColorC.BLACK && ((r & ResultC.DRAW) != 0))
                {
                    return ResultC.DRAW;
                }
            }

            if (Us == ColorC.WHITE && Utils.rank_of(this.psq) < RankC.RANK_7)
            {
                var s = this.psq + SquareC.DELTA_N;
                r |= db[index(this.wksq, this.bksq, s, ColorC.BLACK)]; // Single push

                if (Utils.rank_of(s) == RankC.RANK_3 && s != this.wksq && s != this.bksq)
                {
                    r |= db[index(this.wksq, this.bksq, s + SquareC.DELTA_N, ColorC.BLACK)]; // Double push
                }

                if ((r & ResultC.WIN) != 0)
                {
                    return ResultC.WIN;
                }
            }

            return ((r & ResultC.UNKNOWN) != 0) ? ResultC.UNKNOWN : Us == ColorC.WHITE ? ResultC.DRAW : ResultC.WIN;
        }

        private int classify_index(int idx, int[] db)
        {
            this.decode_index(idx);
            return this.stm == ColorC.WHITE ? this.classify(ColorC.WHITE, db) : this.classify(ColorC.BLACK, db);
        }
    };
}