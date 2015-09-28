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
using CastlingSide = System.Int32;

namespace Portfish
{
    using System;
    using System.Diagnostics;
    using System.Runtime.CompilerServices;

    internal sealed class PawnEntry
    {
        internal ulong key;

        internal ulong passedPawnsWHITE, passedPawnsBLACK;

        internal ulong pawnAttacksWHITE, pawnAttacksBLACK;

        internal int kingSquaresWHITE, kingSquaresBLACK;

        internal int minKPdistanceWHITE, minKPdistanceBLACK;

        internal int halfOpenFilesWHITE, halfOpenFilesBLACK;

        internal int kingSafetyWHITE, kingSafetyBLACK;

        private int castleRightsWHITE, castleRightsBLACK;

        internal int value;

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int pawns_value()
        {
            return this.value;
        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal Bitboard pawn_attacks(Color c)
        //        {
        //            return (c == ColorC.WHITE) ? pawnAttacksWHITE : pawnAttacksBLACK;
        //        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong passed_pawns(int c)
        {
            return (c == ColorC.WHITE) ? this.passedPawnsWHITE : this.passedPawnsBLACK;
        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal int file_is_half_open(Color c, File f)
        //        {
        //            return (c == ColorC.WHITE) ? (halfOpenFilesWHITE & (1 << f)) : (halfOpenFilesBLACK & (1 << f));
        //        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal int has_open_file_to_left(Color c, File f)
        //        {
        //            return (c == ColorC.WHITE) ? (halfOpenFilesWHITE & ((1 << f) - 1)) : (halfOpenFilesBLACK & ((1 << f) - 1));
        //        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal int has_open_file_to_right(Color c, File f)
        //        {
        //            return (c == ColorC.WHITE) ? (halfOpenFilesWHITE & ~((1 << (f + 1)) - 1)) : (halfOpenFilesBLACK & ~((1 << (f + 1)) - 1));
        //        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int king_safety(int Us, Position pos, int ksq)
        {
            if (Us == ColorC.WHITE)
            {
                return this.kingSquaresWHITE == ksq
                       && this.castleRightsWHITE == (pos.st.castleRights & (CastleRightC.WHITE_ANY))
                           ? this.kingSafetyWHITE
                           : this.update_safety_WHITE(pos, ksq);
            }
            return this.kingSquaresBLACK == ksq
                   && this.castleRightsBLACK == (pos.st.castleRights & (CastleRightC.WHITE_ANY << 2))
                       ? this.kingSafetyBLACK
                       : this.update_safety_BLACK(pos, ksq);
        }

        /// PawnEntry::shelter_storm() calculates shelter and storm penalties for the file
        /// the king is on, as well as the two adjacent files.
        internal static int shelter_storm(int Us, Position pos, int ksq)
        {
            var Them = (Us == ColorC.WHITE ? ColorC.BLACK : ColorC.WHITE);

            var safety = PawnTable.MaxSafetyBonus;
            var b = pos.byTypeBB[PieceTypeC.PAWN] & (Utils.InFrontBB[Us][ksq >> 3] | Utils.RankBB[ksq >> 3]);
            var ourPawns = b & pos.byColorBB[Us] & ~Utils.RankBB[ksq >> 3];
            var theirPawns = b & pos.byColorBB[Them];
            int rkUs, rkThem;
            var kf = ksq & 7;

            kf = (kf == FileC.FILE_A) ? kf + 1 : ((kf == FileC.FILE_H) ? kf - 1 : kf);

            for (var f = kf - 1; f <= kf + 1; f++)
            {
                // Shelter penalty is higher for the pawn in front of the king
                b = ourPawns & Utils.FileBB[f];
                rkUs = (b != 0) ? ((Us == ColorC.WHITE ? Utils.lsb(b) : (Utils.msb(b) ^ 56)) >> 3) : RankC.RANK_1;
                safety -= PawnTable.ShelterWeakness[f != kf ? 1 : 0][rkUs];

                // Storm danger is smaller if enemy pawn is blocked
                b = theirPawns & Utils.FileBB[f];
                rkThem = (b != 0)
                             ? ((Us == ColorC.WHITE ? Utils.lsb(b) : (Utils.msb(b) ^ 56)) >> 3)
                             : RankC.RANK_1;
                safety -= PawnTable.StormDanger[rkThem == rkUs + 1 ? 1 : 0][rkThem];
            }

            return safety;
        }

        /// PawnEntry::update_safety() calculates and caches a bonus for king safety. It is
        /// called only when king square changes, about 20% of total king_safety() calls.
        internal int update_safety_WHITE(Position pos, int ksq)
        {
            this.kingSquaresWHITE = ksq;
            this.castleRightsWHITE = pos.st.castleRights & CastleRightC.WHITE_ANY;

            ulong pawns = (ulong)pos.piece_count(ColorC.WHITE, PieceTypeC.PAWN);
            if (pawns > 0)
            {
                var minDist = this.minKPdistanceWHITE;
                while ((Utils.DistanceRingsBB[ksq][minDist++] & pawns) != pawns)
                {
                }
            }

            if (((ksq >> 3) ^ (ColorC.WHITE * 7)) > RankC.RANK_4)
            {
                return this.kingSafetyWHITE = Utils.make_score(0, -16 * this.minKPdistanceWHITE);
            }

            var bonus = shelter_storm(ColorC.WHITE, pos, ksq);

            // If we can castle use the bonus after the castle if is bigger
            if ((pos.st.castleRights & CastleRightC.WHITE_OO) != 0)
            {
                bonus = Math.Max(bonus, shelter_storm(ColorC.WHITE, pos, (SquareC.SQ_G1 ^ (ColorC.WHITE * 56))));
            }

            if ((pos.st.castleRights & CastleRightC.WHITE_OOO) != 0)
            {
                bonus = Math.Max(bonus, shelter_storm(ColorC.WHITE, pos, (SquareC.SQ_C1 ^ (ColorC.WHITE * 56))));
            }

            return this.kingSafetyWHITE = Utils.make_score(bonus, -16 * this.minKPdistanceWHITE);
        }

        internal int update_safety_BLACK(Position pos, int ksq)
        {
            this.kingSquaresBLACK = ksq;
            this.castleRightsBLACK = pos.st.castleRights & CastleRightC.BLACK_ANY;

            ulong pawns = (ulong)pos.piece_count(ColorC.BLACK, PieceTypeC.PAWN);
            if (pawns > 0)
            {
                var minDist = this.minKPdistanceBLACK;
                while ((Utils.DistanceRingsBB[ksq][minDist++] & pawns) != pawns)
                {
                }
            }

            if (((ksq >> 3) ^ (ColorC.BLACK * 7)) > RankC.RANK_4)
            {
                return this.kingSafetyBLACK = Utils.make_score(0, -16 * this.minKPdistanceBLACK);
            }

            var bonus = shelter_storm(ColorC.BLACK, pos, ksq);

            // If we can castle use the bonus after the castle if is bigger
            if ((pos.st.castleRights & CastleRightC.BLACK_OO) != 0)
            {
                bonus = Math.Max(bonus, shelter_storm(ColorC.BLACK, pos, (SquareC.SQ_G1 ^ (ColorC.BLACK * 56))));
            }

            if ((pos.st.castleRights & CastleRightC.BLACK_OOO) != 0)
            {
                bonus = Math.Max(bonus, shelter_storm(ColorC.BLACK, pos, (SquareC.SQ_C1 ^ (ColorC.BLACK * 56))));
            }

            return this.kingSafetyBLACK = Utils.make_score(bonus, -16 * this.minKPdistanceBLACK);
        }
    }

    internal sealed class PawnTable
    {
        internal const int PawnStructureWeight = 15270089; // Utils.make_score(233, 201);

        // Max bonus for king safety. Corresponds to start position with all the pawns
        // in front of the king and no enemy pawn on the horizont.
        internal const int MaxSafetyBonus = 263;

        // Doubled pawn penalty by opposed flag and file
        internal static readonly int[][] DoubledPawnPenalty =
            {
                new[]
                    {
                        Utils.make_score(13, 43),
                        Utils.make_score(20, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(20, 48),
                        Utils.make_score(13, 43)
                    },
                new[]
                    {
                        Utils.make_score(13, 43),
                        Utils.make_score(20, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(23, 48),
                        Utils.make_score(20, 48),
                        Utils.make_score(13, 43)
                    }
            };

        // Isolated pawn penalty by opposed flag and file
        internal static readonly int[][] IsolatedPawnPenalty =
            {
                new[]
                    {
                        Utils.make_score(37, 45),
                        Utils.make_score(54, 52),
                        Utils.make_score(60, 52),
                        Utils.make_score(60, 52),
                        Utils.make_score(60, 52),
                        Utils.make_score(60, 52),
                        Utils.make_score(54, 52),
                        Utils.make_score(37, 45)
                    },
                new[]
                    {
                        Utils.make_score(25, 30),
                        Utils.make_score(36, 35),
                        Utils.make_score(40, 35),
                        Utils.make_score(40, 35),
                        Utils.make_score(40, 35),
                        Utils.make_score(40, 35),
                        Utils.make_score(36, 35),
                        Utils.make_score(25, 30)
                    }
            };

        // Backward pawn penalty by opposed flag and file
        internal static readonly int[][] BackwardPawnPenalty =
            {
                new[]
                    {
                        Utils.make_score(30, 42),
                        Utils.make_score(43, 46),
                        Utils.make_score(49, 46),
                        Utils.make_score(49, 46),
                        Utils.make_score(49, 46),
                        Utils.make_score(49, 46),
                        Utils.make_score(43, 46),
                        Utils.make_score(30, 42)
                    },
                new[]
                    {
                        Utils.make_score(20, 28),
                        Utils.make_score(29, 31),
                        Utils.make_score(33, 31),
                        Utils.make_score(33, 31),
                        Utils.make_score(33, 31),
                        Utils.make_score(33, 31),
                        Utils.make_score(29, 31),
                        Utils.make_score(20, 28)
                    }
            };

        // Pawn chain membership bonus by file
        internal static readonly int[] ChainBonus =
            {
                Utils.make_score(11, -1), Utils.make_score(13, -1),
                Utils.make_score(13, -1), Utils.make_score(14, -1),
                Utils.make_score(14, -1), Utils.make_score(13, -1),
                Utils.make_score(13, -1), Utils.make_score(11, -1)
            };

        // Candidate passed pawn bonus by rank
        internal static readonly int[] CandidateBonus =
            {
                Utils.make_score(0, 0), Utils.make_score(6, 13),
                Utils.make_score(6, 13), Utils.make_score(14, 29),
                Utils.make_score(34, 68), Utils.make_score(83, 166),
                Utils.make_score(0, 0), Utils.make_score(0, 0)
            };

        // Weakness of our pawn shelter in front of the king indexed by [king pawn][rank]
        internal static readonly int[][] ShelterWeakness =
            {
                new[] { 141, 0, 38, 102, 128, 141, 141, 0 },
                new[] { 61, 0, 16, 44, 56, 61, 61, 0 }
            };

        // Danger of enemy pawns moving toward our king indexed by [pawn blocked][rank]
        internal static readonly int[][] StormDanger =
            {
                new[] { 26, 0, 128, 51, 26, 0, 0, 0 },
                new[] { 13, 0, 64, 25, 13, 0, 0, 0 }
            };

        //private bool hasEntries = false;
        private readonly PawnEntry[] entries = new PawnEntry[Constants.PawnTableSize];

        internal PawnTable()
        {
            for (var i = 0; i < Constants.PawnTableSize; i++)
            {
                this.entries[i] = new PawnEntry();
            }
        }

        /// PawnTable::pawn_info() takes a position object as input, computes
        /// a PawnInfo object, and returns a pointer to it. The result is also stored
        /// in an hash table, so we don't have to recompute everything when the same
        /// pawn structure occurs again.
        internal void probe(Position pos, out PawnEntry e)
        {
            var key = pos.pawn_key();
            e = this.entries[((uint)key) & Constants.PawnTableMask];

            // If pi.key matches the position's pawn hash key, it means that we
            // have analysed this pawn structure before, and we can simply return
            // the information we found the last time instead of recomputing it.
            if (e.key == key)
            {
                return;
            }

            // Initialize PawnInfo entry
            e.key = key;
            e.passedPawnsWHITE = e.passedPawnsBLACK = 0;
            e.kingSquaresWHITE = e.kingSquaresBLACK = SquareC.SQ_NONE;
            e.halfOpenFilesWHITE = e.halfOpenFilesBLACK = 0xFF;

            // Calculate pawn attacks
            var wPawns = pos.pieces_PTC(PieceTypeC.PAWN, ColorC.WHITE);
            var bPawns = pos.pieces_PTC(PieceTypeC.PAWN, ColorC.BLACK);

            e.pawnAttacksWHITE = ((wPawns & ~Constants.FileHBB) << 9) | ((wPawns & ~Constants.FileABB) << 7);
            e.pawnAttacksBLACK = ((bPawns & ~Constants.FileHBB) >> 7) | ((bPawns & ~Constants.FileABB) >> 9);

            // Evaluate pawns for both colors and weight the result
            e.value = evaluate_pawns(ColorC.WHITE, pos, wPawns, bPawns, e)
                      - evaluate_pawns(ColorC.BLACK, pos, bPawns, wPawns, e);

            e.value = Utils.apply_weight(e.value, PawnStructureWeight);
        }

        /// PawnTable::evaluate_pawns() evaluates each pawn of the given color
        internal static int evaluate_pawns(int Us, Position pos, ulong ourPawns, ulong theirPawns, PawnEntry e)
        {
            var Them = (Us == ColorC.WHITE ? ColorC.BLACK : ColorC.WHITE);

            ulong b;
            int s;
            int f;
            int r;
            bool passed, isolated, doubled, opposed, chain, backward, candidate;
            var value = ScoreC.SCORE_ZERO;
            var pl = pos.pieceList[Us][PieceTypeC.PAWN];
            var plPos = 0;

            // Loop through all pawns of the current color and score each pawn
            while ((s = pl[plPos++]) != SquareC.SQ_NONE)
            {
                Debug.Assert(pos.piece_on(s) == Utils.make_piece(Us, PieceTypeC.PAWN));

                f = (s & 7);
                r = (s >> 3);

                // This file cannot be half open
                if (Us == ColorC.WHITE)
                {
                    e.halfOpenFilesWHITE &= ~(1 << f);
                }
                else
                {
                    e.halfOpenFilesBLACK &= ~(1 << f);
                }

                // Our rank plus previous one. Used for chain detection
                b = Utils.RankBB[r] | Utils.RankBB[Us == ColorC.WHITE ? r - 1 : r + 1];

                // Flag the pawn as passed, isolated, doubled or member of a pawn
                // chain (but not the backward one).
                chain = (ourPawns & Utils.AdjacentFilesBB[f] & b) != 0;
                isolated = (ourPawns & Utils.AdjacentFilesBB[f]) == 0;
                doubled = (ourPawns & Utils.ForwardBB[Us][s]) != 0;
                opposed = (theirPawns & Utils.ForwardBB[Us][s]) != 0;
                passed = (theirPawns & Utils.PassedPawnMask[Us][s]) == 0;

                // Test for backward pawn
                backward = false;

                // If the pawn is passed, isolated, or member of a pawn chain it cannot
                // be backward. If there are friendly pawns behind on adjacent files
                // or if can capture an enemy pawn it cannot be backward either.
                if (!(passed | isolated | chain) && (((ourPawns & Utils.AttackSpanMask[Them][s])) == 0)
                    && ((((Utils.StepAttacksBB[((Us << 3) | PieceTypeC.PAWN)][s]) & theirPawns)) == 0))
                {
                    // We now know that there are no friendly pawns beside or behind this
                    // pawn on adjacent files. We now check whether the pawn is
                    // backward by looking in the forward direction on the adjacent
                    // files, and seeing whether we meet a friendly or an enemy pawn first.
                    b = Utils.StepAttacksBB[((Us << 3) | PieceTypeC.PAWN)][s];

                    // Note that we are sure to find something because pawn is not passed
                    // nor isolated, so loop is potentially infinite, but it isn't.
                    while ((b & (ourPawns | theirPawns)) == 0)
                    {
                        if (Us == ColorC.WHITE)
                        {
                            b <<= 8;
                        }
                        else
                        {
                            b >>= 8;
                        }
                    }

                    // The friendly pawn needs to be at least two ranks closer than the
                    // enemy pawn in order to help the potentially backward pawn advance.
                    backward = (((b | (Us == ColorC.WHITE ? b << 8 : b >> 8)) & theirPawns) != 0);
                }

                Debug.Assert(opposed | passed | (((Utils.AttackSpanMask[Us][s] & theirPawns)) != 0));

                // A not passed pawn is a candidate to become passed if it is free to
                // advance and if the number of friendly pawns beside or behind this
                // pawn on adjacent files is higher or equal than the number of
                // enemy pawns in the forward direction on the adjacent files.
                candidate = !(opposed | passed | backward | isolated)
                            && (b =
                                Utils.AttackSpanMask[Them][s + (Us == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S)]
                                & ourPawns) != 0
                            && Bitcount.popcount_1s_Max15(b)
                            >= Bitcount.popcount_1s_Max15(Utils.AttackSpanMask[Us][s] & theirPawns);

                // Passed pawns will be properly scored in evaluation because we need
                // full attack info to evaluate passed pawns. Only the frontmost passed
                // pawn on each file is considered a true passed pawn.
                if (passed && !doubled)
                {
                    if (Us == ColorC.WHITE)
                    {
                        e.passedPawnsWHITE |= Utils.SquareBB[s];
                    }
                    else
                    {
                        e.passedPawnsBLACK |= Utils.SquareBB[s];
                    }
                }

                // Score this pawn
                if (isolated)
                {
                    value -= IsolatedPawnPenalty[opposed ? 1 : 0][f];
                }

                if (doubled)
                {
                    value -= DoubledPawnPenalty[opposed ? 1 : 0][f];
                }

                if (backward)
                {
                    value -= BackwardPawnPenalty[opposed ? 1 : 0][f];
                }

                if (chain)
                {
                    value += ChainBonus[f];
                }

                if (candidate)
                {
                    value += CandidateBonus[((s >> 3) ^ (Us * 7))];
                }
            }
            return value;
        }
    }
}