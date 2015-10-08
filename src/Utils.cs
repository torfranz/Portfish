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
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Reflection;
    using System.Runtime.CompilerServices;
    using System.Text;

    internal static class Utils
    {
        #region Defines

#if X64
        private const string cpu64 = " 64bit";
#else
        private const string cpu64 = "";
#endif

        #endregion

        #region Fields

        internal static readonly ulong[] RMasks = new ulong[SquareC.SQUARE_NB];

        internal static readonly ulong[] RMagics = new ulong[SquareC.SQUARE_NB];

        internal static readonly ulong[][] RAttacks = new ulong[SquareC.SQUARE_NB][];

        internal static readonly int[] RShifts = new int[SquareC.SQUARE_NB];

        internal static readonly ulong[] BMasks = new ulong[SquareC.SQUARE_NB];

        internal static readonly ulong[] BMagics = new ulong[SquareC.SQUARE_NB];

        internal static readonly ulong[][] BAttacks = new ulong[SquareC.SQUARE_NB][];

        internal static readonly int[] BShifts = new int[SquareC.SQUARE_NB];

        internal static readonly ulong[] occupancy = new ulong[4096];

        internal static readonly ulong[] reference = new ulong[4096];

        internal static readonly ulong[] SquareBB = new ulong[SquareC.SQUARE_NB];

        internal static readonly ulong[] FileBB = new ulong[FileC.FILE_NB];

        internal static readonly ulong[] RankBB = new ulong[RankC.RANK_NB];

        internal static readonly ulong[] AdjacentFilesBB = new ulong[FileC.FILE_NB];

        internal static readonly ulong[] ThisAndAdjacentFilesBB = new ulong[FileC.FILE_NB];

        internal static readonly ulong[][] InFrontBB = new ulong[ColorC.COLOR_NB][]; // 2,8

        internal static readonly ulong[][] ForwardBB = new ulong[ColorC.COLOR_NB][]; // 2,64

        internal static readonly ulong[][] PassedPawnMask = new ulong[ColorC.COLOR_NB][]; // 2,64

        internal static readonly ulong[][] AttackSpanMask = new ulong[ColorC.COLOR_NB][]; // 2,64

        internal static readonly ulong[][] BetweenBB = new ulong[SquareC.SQUARE_NB][]; // 64, 64

        internal static readonly ulong[][] DistanceRingsBB = new ulong[SquareC.SQUARE_NB][]; //64, 8
        
        internal static readonly ulong[][] StepAttacksBB = new ulong[PieceC.PIECE_NB][]; // 16, 64

        internal static readonly ulong[] StepAttacksBB_KING = new ulong[SquareC.SQUARE_NB]; // 64

        internal static readonly ulong[] StepAttacksBB_KNIGHT = new ulong[SquareC.SQUARE_NB]; // 64

        internal static readonly ulong[][] PseudoAttacks = new ulong[PieceTypeC.PIECE_TYPE_NB][]; // 6, 64

        internal static readonly ulong[] PseudoAttacks_ROOK = new ulong[SquareC.SQUARE_NB]; // 64
        
        internal static readonly ulong[] PseudoAttacks_BISHOP = new ulong[SquareC.SQUARE_NB]; // 64

        internal static readonly ulong[] PseudoAttacks_QUEEN = new ulong[SquareC.SQUARE_NB]; // 64

        internal static readonly int[] BSFTable = new int[SquareC.SQUARE_NB];

        internal static readonly int[][] SquareDistance = new int[SquareC.SQUARE_NB][]; // 64, 64

        internal static readonly int[] MS1BTable = new int[256];

        internal static readonly int[] RDeltas = { SquareC.DELTA_N, SquareC.DELTA_E, SquareC.DELTA_S, SquareC.DELTA_W };

        internal static readonly int[] BDeltas =
            {
                SquareC.DELTA_NE, SquareC.DELTA_SE, SquareC.DELTA_SW,
                SquareC.DELTA_NW
            };

        internal static readonly int[][] steps =
            {
                new int[] { }, new[] { 7, 9 },
                new[] { 17, 15, 10, 6, -6, -10, -15, -17 }, new int[] { },
                new int[] { }, new int[] { }, new[] { 9, 7, -7, -9, 8, 1, -1, -8 }
            };

        internal static readonly int[][] MagicBoosters =
            {
                new[] { 3191, 2184, 1310, 3618, 2091, 1308, 2452, 3996 },
                new[] { 1059, 3608, 605, 3234, 3326, 38, 2029, 3043 }
            };

        internal static readonly RKISS rk = new RKISS();

        internal static readonly char[] _pieces = " PNBRQK".ToCharArray();
        internal static readonly string[] PieceToChar = { " PNBRQK", " pnbrqk" };

        internal const ulong DeBruijn_64 = 0x3F79D71B4CB0A89UL;
        internal const uint DeBruijn_32 = 0x783A9B23;
        #endregion

        #region Lookup init

        internal static void init()
        {
            for (int k = 0, i = 0; i < 8; i++)
            {
                while (k < (2 << i))
                {
                    MS1BTable[k++] = i;
                }
            }

            for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
            {
                SquareBB[s] = 1UL << s;
            }

            FileBB[FileC.FILE_A] = Constants.FileABB;
            RankBB[RankC.RANK_1] = Constants.Rank1BB;

            for (var i = 1; i < 8; i++)
            {
                FileBB[i] = FileBB[i - 1] << 1;
                RankBB[i] = RankBB[i - 1] << 8;
            }

            for (var f = FileC.FILE_A; f <= FileC.FILE_H; f++)
            {
                AdjacentFilesBB[f] = (f > FileC.FILE_A ? FileBB[f - 1] : 0) | (f < FileC.FILE_H ? FileBB[f + 1] : 0);
                ThisAndAdjacentFilesBB[f] = FileBB[f] | AdjacentFilesBB[f];
            }

            InFrontBB[ColorC.WHITE] = new ulong[RankC.RANK_NB];
            InFrontBB[ColorC.BLACK] = new ulong[RankC.RANK_NB];
            for (var r = RankC.RANK_1; r < RankC.RANK_8; r++)
            {
                InFrontBB[ColorC.WHITE][r] = ~(InFrontBB[ColorC.BLACK][r + 1] = InFrontBB[ColorC.BLACK][r] | RankBB[r]);
            }

            for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
            {
                ForwardBB[c] = new ulong[SquareC.SQUARE_NB];
                PassedPawnMask[c] = new ulong[SquareC.SQUARE_NB];
                AttackSpanMask[c] = new ulong[SquareC.SQUARE_NB];
                for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
                {
                    ForwardBB[c][s] = InFrontBB[c][rank_of(s)] & FileBB[file_of(s)];
                    PassedPawnMask[c][s] = InFrontBB[c][rank_of(s)] & ThisAndAdjacentFilesBB[file_of(s)];
                    AttackSpanMask[c][s] = InFrontBB[c][rank_of(s)] & AdjacentFilesBB[file_of(s)];
                }
            }

            for (var s1 = SquareC.SQ_A1; s1 <= SquareC.SQ_H8; s1++)
            {
                SquareDistance[s1] = new int[64];
                for (var s2 = SquareC.SQ_A1; s2 <= SquareC.SQ_H8; s2++)
                {
                    SquareDistance[s1][s2] = Math.Max(file_distance(s1, s2), rank_distance(s1, s2));
                }
            }

            for (Square s1 = SquareC.SQ_A1; s1 <= SquareC.SQ_H8; s1++)
            {
                DistanceRingsBB[s1] = new ulong[8];

                for (int d = 1; d < 8; d++)
                {
                    for (Square s2 = SquareC.SQ_A1; s2 <= SquareC.SQ_H8; s2++)
                    {
                        if (SquareDistance[s1][s2] == d)
                        {
                            DistanceRingsBB[s1][d - 1] |= (ulong)s2;
                        }
                    }
                }
            }

            // Matt Taylor's folding trick for 32 bit systems
            for (var i = 0; i < 64; i++)
            {
                var b = (1UL << i);
                b ^= b - 1;
#if X64
                BSFTable[(b * DeBruijn_64) >> 58] = i;
#else
                b ^= b >> 32;
                BSFTable[(uint)(b * DeBruijn_32) >> 26] = i;
#endif
            }


            for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
            {
                for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
                {
                    var piece = make_piece(c, pt);
                    StepAttacksBB[piece] = new ulong[SquareC.SQUARE_NB];
                    for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
                    {
                        for (var k = 0; k < steps[pt].Length; k++)
                        {
                            var to = s + (c == ColorC.WHITE ? steps[pt][k] : -steps[pt][k]);
                            if (is_ok_S(to) && SquareDistance[s][to] < 3)
                            {
                                StepAttacksBB[piece][s] |= SquareBB[to];
                            }
                        }
                    }
                }
            }

            for (var i = 0; i < 64; i++)
            {
                StepAttacksBB_KING[i] = StepAttacksBB[PieceTypeC.KING][i];
                StepAttacksBB_KNIGHT[i] = StepAttacksBB[PieceTypeC.KNIGHT][i];
            }

            for (var i = 0; i < 6; i++)
            {
                PseudoAttacks[i] = new ulong[64];
            }

            init_magics(PieceTypeC.ROOK, RAttacks, RMagics, RMasks, RShifts, RDeltas, magic_index);
            init_magics(PieceTypeC.BISHOP, BAttacks, BMagics, BMasks, BShifts, BDeltas, magic_index);

            for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
            {
                PseudoAttacks[PieceTypeC.QUEEN][s] = PseudoAttacks[PieceTypeC.BISHOP][s] = bishop_attacks_bb(s, 0);
                PseudoAttacks[PieceTypeC.QUEEN][s] |= PseudoAttacks[PieceTypeC.ROOK][s] = rook_attacks_bb(s, 0);

                PseudoAttacks_BISHOP[s] = PseudoAttacks[PieceTypeC.BISHOP][s];
                PseudoAttacks_ROOK[s] = PseudoAttacks[PieceTypeC.ROOK][s];
                PseudoAttacks_QUEEN[s] = PseudoAttacks[PieceTypeC.QUEEN][s];
            }

            for (var s1 = SquareC.SQ_A1; s1 <= SquareC.SQ_H8; s1++)
            {
                BetweenBB[s1] = new ulong[64];
                for (var s2 = SquareC.SQ_A1; s2 <= SquareC.SQ_H8; s2++)
                {
                    if ((PseudoAttacks[PieceTypeC.QUEEN][s1] & SquareBB[s2]) != 0)
                    {
                        var delta = ((s2 - s1) / SquareDistance[s1][s2]);
                        for (var s = s1 + delta; s != s2; s += delta)
                        {
                            set_bit(ref BetweenBB[s1][s2], s);
                        }
                    }
                }
            }
        }

        private static ulong sliding_attack(int[] deltas, int sq, ulong occupied)
        {
            ulong attack = 0;

            for (var i = 0; i < 4; i++)
            {
                for (var s = sq + deltas[i]; is_ok_S(s) && square_distance(s, s - deltas[i]) == 1; s += deltas[i])
                {
                    set_bit(ref attack, s);

                    if (bit_is_set(occupied, s) != 0)
                    {
                        break;
                    }
                }
            }

            return attack;
        }

        private static ulong pick_random(RKISS rk, int booster)
        {
            // Values s1 and s2 are used to rotate the candidate magic of a
            // quantity known to be the optimal to quickly find the magics.
            int s1 = booster & 63, s2 = (booster >> 6) & 63;

            ulong m = rk.rand();
            m = (m >> s1) | (m << (64 - s1));
            m &= rk.rand();
            m = (m >> s2) | (m << (64 - s2));
            return m & rk.rand();
        }

        // init_magics() computes all rook and bishop attacks at startup. Magic
        // bitboards are used to look up attacks of sliding pieces. As a reference see
        // chessprogramming.wikispaces.com/Magic+Bitboards. In particular, here we
        // use the so called "fancy" approach.

        private static void init_magics(
            int Pt,
            ulong[][] attacks,
            ulong[] magics,
            ulong[] masks,
            int[] shifts,
            int[] deltas,
            Fn index)
        {
            ulong edges, b;
            int i, size, booster;

            for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
            {
                // Board edges are not considered in the relevant occupancies
                edges = ((Constants.Rank1BB | Constants.Rank8BB) & ~rank_bb_S(s))
                        | ((Constants.FileABB | Constants.FileHBB) & ~file_bb_S(s));

                // Given a square 's', the mask is the bitboard of sliding attacks from
                // 's' computed on an empty board. The index must be big enough to contain
                // all the attacks for each possible subset of the mask and so is 2 power
                // the number of 1s of the mask. Hence we deduce the size of the shift to
                // apply to the 64 or 32 bits word to get the index.
                masks[s] = sliding_attack(deltas, s, 0) & ~edges;

#if X64
                shifts[s] = 64 - Bitcount.popcount_1s_Max15(masks[s]);
#else
                shifts[s] = 32 - Bitcount.popcount_1s_Max15(masks[s]);
#endif

                // Use Carry-Rippler trick to enumerate all subsets of masks[s] and
                // store the corresponding sliding attack bitboard in reference[].
                b = 0;
                size = 0;
                do
                {
                    occupancy[size] = b;
                    reference[size++] = sliding_attack(deltas, s, b);
                    b = (b - masks[s]) & masks[s];
                }
                while (b != 0);

                // Set the offset for the table of the next square. We have individual
                // table sizes for each square with "Fancy Magic Bitboards".
#if X64
                booster = MagicBoosters[1][rank_of(s)];
#else
                booster = MagicBoosters[0][rank_of(s)];
#endif

                attacks[s] = new ulong[size];

                // Find a magic for square 's' picking up an (almost) random number
                // until we find the one that passes the verification test.
                do
                {
                    do
                    {
                        magics[s] = pick_random(rk, booster);
                    }
                    while (Bitcount.popcount_1s_Max15((magics[s] * masks[s]) >> 56) < 6);

                    Array.Clear(attacks[s], 0, size);

                    // A good magic must map every possible occupancy to an index that
                    // looks up the correct sliding attack in the attacks[s] database.
                    // Note that we build up the database for square 's' as a side
                    // effect of verifying the magic.
                    for (i = 0; i < size; i++)
                    {
                        var idx = index(Pt, s, occupancy[i]);

                        var attack = attacks[s][idx];

                        if ((attack != 0) && attack != reference[i])
                        {
                            break;
                        }
                        Debug.Assert(reference[i] != 0);
                        attacks[s][idx] = reference[i];
                    }
                }
                while (i != size);
            }
        }

        #endregion

        #region String operations

        internal static bool isdigit(char c)
        {
            return c >= '0' && c <= '9';
        }

        internal static bool islower(char token)
        {
            return token.ToString().ToLowerInvariant() == token.ToString();
        }

        internal static char toupper(char token)
        {
            return token.ToString().ToUpperInvariant()[0];
        }

        internal static char tolower(char token)
        {
            return token.ToString().ToLowerInvariant()[0];
        }

        internal static char file_to_char(int f, bool tolower = true)
        {
            return (char)(f - FileC.FILE_A + (tolower ? 'a' : 'A'));
        }

        internal static char rank_to_char(int r)
        {
            return (char)(r - RankC.RANK_1 + '1');
        }

        internal static string square_to_string(int s)
        {
            return string.Concat(file_to_char(file_of(s)), rank_to_char(rank_of(s)));
        }

        internal static Stack<string> CreateStack(string input)
        {
            var lines = input.Trim().Split(' ');
            var stack = new Stack<string>(); // LIFO
            for (var i = (lines.Length - 1); i >= 0; i--)
            {
                var line = lines[i];
                if (!string.IsNullOrEmpty(line))
                {
                    line = line.Trim();
                    stack.Push(line);
                }
            }
            return stack;
        }

        #endregion

        #region Bit operations

        /// Functions for testing whether a given bit is set in a bitboard, and for
        /// setting and clearing bits.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong bit_is_set(ulong b, int s)
        {
            return b & SquareBB[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void set_bit(ref ulong b, int s)
        {
            b |= SquareBB[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static void xor_bit(ref ulong b, int s)
        {
            b ^= SquareBB[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong set_bit(ulong b, int s)
        {
            return b | SquareBB[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong xor_bit(ulong b, int s)
        {
            return b ^ SquareBB[s];
        }

        /// single_bit() returns true if in the 'b' bitboard is set a single bit (or if
        /// b == 0).
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static bool single_bit(ulong b)
        {
            return ((b & (b - 1)) == 0);
        }

        /// more_than_one() returns true if in 'b' there is more than one bit set
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static bool more_than_one(ulong b)
        {
            return ((b & (b - 1)) != 0);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int pop_lsb(ref ulong b)
        {
#if X64
            Bitboard bb = b;
            b &= (b - 1);
            return (BSFTable[((bb & (0xffffffffffffffff - bb + 1)) * DeBruijn_64) >> 58]);
#else
            var bb = b ^ (b - 1);
            b &= (b - 1);
            return BSFTable[(((uint)((bb & 0xffffffff) ^ (bb >> 32))) * DeBruijn_32) >> 26];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int lsb(ulong b)
        {
#if X64
            return BSFTable[((b & (0xffffffffffffffff - b + 1)) * DeBruijn_64) >> 58];
#else
            b ^= (b - 1);
            return BSFTable[(((uint)((b & 0xffffffff) ^ (b >> 32))) * DeBruijn_32) >> 26];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int msb(ulong b)
        {
            var result = 0;
            if (b > 0xFFFFFFFF)
            {
                b >>= 32;
                result = 32;
            }
            if (b > 0xFFFF)
            {
                b >>= 16;
                result += 16;
            }
            if (b > 0xFF)
            {
                b >>= 8;
                result += 8;
            }
            return result + MS1BTable[b];
        }

        #endregion

        #region Bitboard operations

        /// Functions used to update a bitboard after a move. This is faster
        /// then calling a sequence of clear_bit() + set_bit()
        /// rank_bb() and file_bb() take a file or a square as input and return
        /// a bitboard representing all squares on the given file or rank.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong rank_bb_R(int r)
        {
            return RankBB[r];
        }

        /// rank_bb() and file_bb() take a file or a square as input and return
        /// a bitboard representing all squares on the given file or rank.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong rank_bb_S(int s)
        {
            return RankBB[rank_of(s)];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong file_bb_S(int s)
        {
            return FileBB[file_of(s)];
        }

        /// rank_bb() and file_bb() take a file or a square as input and return
        /// a bitboard representing all squares on the given file or rank.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong file_bb_F(int f)
        {
            return FileBB[f];
        }

        /// adjacent_files_bb takes a file as input and returns a bitboard representing
        /// all squares on the adjacent files.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong adjacent_files_bb(int f)
        {
            return AdjacentFilesBB[f];
        }

        /// this_and_adjacent_files_bb takes a file as input and returns a bitboard
        /// representing all squares on the given and adjacent files.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong this_and_adjacent_files_bb(int f)
        {
            return ThisAndAdjacentFilesBB[f];
        }

        /// in_front_bb() takes a color and a rank or square as input, and returns a
        /// bitboard representing all the squares on all ranks in front of the rank
        /// (or square), from the given color's point of view.  For instance,
        /// in_front_bb(WHITE, RANK_5) will give all squares on ranks 6, 7 and 8, while
        /// in_front_bb(BLACK, SQ_D3) will give all squares on ranks 1 and 2.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong in_front_bb_CR(int c, int r)
        {
            return InFrontBB[c][r];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong in_front_bb_CS(int c, int s)
        {
            return InFrontBB[c][rank_of(s)];
        }

        /// Functions for computing sliding attack bitboards. rook_attacks_bb(),
        /// bishop_attacks_bb() and queen_attacks_bb() all take a square and a
        /// bitboard of occupied squares as input, and return a bitboard representing
        /// all squares attacked by a rook, bishop or queen on the given square.
        internal delegate uint Fn(int Pt, int s, ulong occ);

        /// Functions for computing sliding attack bitboards. Function attacks_bb() takes
        /// a square and a bitboard of occupied squares as input, and returns a bitboard
        /// representing all squares attacked by Pt (bishop or rook) on the given square.
        internal static uint magic_index(int Pt, int s, ulong occ)
        {
#if X64
            Bitboard[] Masks = Pt == PieceTypeC.ROOK ? RMasks : BMasks;
            Bitboard[] Magics = Pt == PieceTypeC.ROOK ? RMagics : BMagics;
            int[] Shifts = Pt == PieceTypeC.ROOK ? RShifts : BShifts;
            return (uint)(((occ & Masks[s]) * Magics[s]) >> Shifts[s]);
#else
            var Masks = Pt == PieceTypeC.ROOK ? RMasks : BMasks;
            var Magics = Pt == PieceTypeC.ROOK ? RMagics : BMagics;
            var Shifts = Pt == PieceTypeC.ROOK ? RShifts : BShifts;

            var lo = (uint)(occ) & (uint)(Masks[s]);
            var hi = (uint)(occ >> 32) & (uint)(Masks[s] >> 32);
            return (lo * (uint)(Magics[s]) ^ hi * (uint)(Magics[s] >> 32)) >> Shifts[s];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong rook_attacks_bb(int s, ulong occ)
        {
#if X64
            return RAttacks[s][(((occ & RMasks[s]) * RMagics[s]) >> RShifts[s])];
#else
            var lo = (uint)(occ) & (uint)(RMasks[s]);
            var hi = (uint)(occ >> 32) & (uint)(RMasks[s] >> 32);
            return RAttacks[s][(lo * (uint)(RMagics[s]) ^ hi * (uint)(RMagics[s] >> 32)) >> RShifts[s]];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong bishop_attacks_bb(int s, ulong occ)
        {
#if X64
            return BAttacks[s][(((occ & BMasks[s]) * BMagics[s]) >> BShifts[s])];
#else
            var lo = (uint)(occ) & (uint)(BMasks[s]);
            var hi = (uint)(occ >> 32) & (uint)(BMasks[s] >> 32);
            return BAttacks[s][(lo * (uint)(BMagics[s]) ^ hi * (uint)(BMagics[s] >> 32)) >> BShifts[s]];
#endif
        }

        /// between_bb returns a bitboard representing all squares between two squares.
        /// For instance, between_bb(SQ_C4, SQ_F7) returns a bitboard with the bits for
        /// square d5 and e6 set.  If s1 and s2 are not on the same line, file or diagonal,
        /// 0 is returned.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong between_bb(int s1, int s2)
        {
            return BetweenBB[s1][s2];
        }

        /// forward_bb takes a color and a square as input, and returns a bitboard
        /// representing all squares along the line in front of the square, from the
        /// point of view of the given color. Definition of the table is:
        /// ForwardBB[c][s] = in_front_bb(c, s) & file_bb(s)
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong forward_bb(int c, int s)
        {
            return ForwardBB[c][s];
        }

        /// passed_pawn_mask takes a color and a square as input, and returns a
        /// bitboard mask which can be used to test if a pawn of the given color on
        /// the given square is a passed pawn. Definition of the table is:
        /// PassedPawnMask[c][s] = in_front_bb(c, s) & this_and_adjacent_files_bb(s)
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong passed_pawn_mask(int c, int s)
        {
            return PassedPawnMask[c][s];
        }

        /// attack_span_mask takes a color and a square as input, and returns a bitboard
        /// representing all squares that can be attacked by a pawn of the given color
        /// when it moves along its file starting from the given square. Definition is:
        /// AttackSpanMask[c][s] = in_front_bb(c, s) & adjacent_files_bb(s);
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong attack_span_mask(int c, int s)
        {
            return AttackSpanMask[c][s];
        }

        /// squares_aligned returns true if the squares s1, s2 and s3 are aligned
        /// either on a straight or on a diagonal line.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static bool squares_aligned(int s1, int s2, int s3)
        {
            return ((BetweenBB[s1][s2] | BetweenBB[s1][s3] | BetweenBB[s2][s3])
                    & (SquareBB[s1] | SquareBB[s2] | SquareBB[s3])) != 0;
        }

        /// same_color_squares() returns a bitboard representing all squares with
        /// the same color of the given square.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong same_color_squares(int s)
        {
            return (bit_is_set(0xAA55AA55AA55AA55UL, s) != 0) ? 0xAA55AA55AA55AA55UL : ~0xAA55AA55AA55AA55UL;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int square_distance(int s1, int s2)
        {
            return SquareDistance[s1][s2];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int mate_in(int ply)
        {
            return ValueC.VALUE_MATE - ply;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int mated_in(int ply)
        {
            return -ValueC.VALUE_MATE + ply;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make_piece(int c, int pt)
        {
            return ((c << 3) | pt);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int type_of_move(Move m)
        {
             return m & (3 << 14);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int type_of(int p)
        {
            return (p & 7);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int color_of(int p)
        {
            Debug.Assert(p != PieceC.NO_PIECE);
            return p >> 3;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int flip_C(int c)
        {
            return (c ^ 1);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make_square(int f, int r)
        {
            return ((r << 3) | f);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static bool is_ok_S(int s)
        {
            return s >= SquareC.SQ_A1 && s <= SquareC.SQ_H8;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static bool is_ok_M(int m)
        {
            return from_sq(m) != to_sq(m); // Catches also MOVE_NULL and MOVE_NONE
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int file_of(int s)
        {
            return (s & 7);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int rank_of(int s)
        {
            return (s >> 3);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int flip_S(int s)
        {
            return (s ^ 56);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int mirror(int s)
        {
            return (s ^ 7); // Horizontal flip SQ_A1 -> SQ_H1
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int relative_square(int c, int s)
        {
            return (s ^ (c * 56));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int relative_rank_CR(int c, int r)
        {
            return (r ^ (c * 7));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int relative_rank_CS(int c, int s)
        {
            return ((s >> 3) ^ (c * 7));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static bool opposite_colors(int s1, int s2)
        {
            var s = s1 ^ s2;
            return (((s >> 3) ^ s) & 1) != 0;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int file_distance(int s1, int s2)
        {
            return Math.Abs(file_of(s1) - file_of(s2));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int rank_distance(int s1, int s2)
        {
            return Math.Abs(rank_of(s1) - rank_of(s2));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int pawn_push(int c)
        {
            return c == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int from_sq(int m)
        {
            return ((m >> 6) & 0x3F);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int to_sq(int m)
        {
            return (m & 0x3F);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int promotion_type(int m)
        {
            return (((m >> 12) & 3) + 2);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make_move(int from, int to)
        {
            return (to | (from << 6));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make_enpassant(int from, int to)
        {
            return (to | (from << 6) | (2 << 14));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make(int from, int to, int moveType, PieceType pt = PieceTypeC.KNIGHT)
        {
            return to | (from << 6) | moveType | ((pt - PieceTypeC.KNIGHT) << 12);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make_castle_right(int c, int s)
        {
            return CastleRightC.WHITE_OO << ((s == CastlingSideC.QUEEN_SIDE ? 1 : 0) + 2 * c);
        }

        #endregion

        #region Engine info

        /// engine_info() returns the full name of the current Stockfish version.
        /// This will be either "Portfish YYMMDD" (where YYMMDD is the date when
        /// the program was compiled) or "Portfish
        /// <version number>
        ///     ", depending
        ///     on whether Version is empty.
        internal static string engine_info()
        {
            return engine_info(false);
        }

        internal static string engine_info(bool to_uci)
        {
#if WINDOWS_RT
    // Assembly and file version
            Assembly assembly = typeof(Engine).GetTypeInfo().Assembly;
            Version fileVersion = null;
            AssemblyFileVersionAttribute fileVersionRaw = assembly.GetCustomAttribute<AssemblyFileVersionAttribute>();
            if (fileVersionRaw != null)
            {
                fileVersion = new Version(fileVersionRaw.Version);
            }
#else
            // Assembly and file version
            var assembly = Assembly.GetExecutingAssembly();
            Version fileVersion = null;
            var attribs = assembly.GetCustomAttributes(typeof(AssemblyFileVersionAttribute), false);
            if (attribs.Length > 0)
            {
                var fileVersionRaw = (AssemblyFileVersionAttribute)(attribs[0]);
                fileVersion = new Version(fileVersionRaw.Version);
            }
#endif
            // Extract version/build date
            var fullName = assembly.FullName;
            var vspos = fullName.IndexOf("Version=");
            var vepos = fullName.IndexOf(",", vspos);
            var versionRaw = fullName.Substring(vspos + 8, vepos - vspos - 8);
            var version = new Version(versionRaw);
            var buildDateTime =
                new DateTime(2000, 1, 1).Add(
                    new TimeSpan(
                        TimeSpan.TicksPerDay * version.Build + // days since 1 January 2000
                        TimeSpan.TicksPerSecond * 2 * version.Revision));
            // seconds since midnight, (multiply by 2 to get original)

            // Get version info
            var versionInfo = buildDateTime.Year + buildDateTime.Month.ToString().PadLeft(2, '0')
                              + buildDateTime.Day.ToString().PadLeft(2, '0');
            if (fileVersion != null)
            {
                versionInfo = fileVersion.ToString();
            }

            // Create version
            var sb = new StringBuilder();
            sb.Append("Portfish ").Append(versionInfo).Append(cpu64);
            sb.Append(to_uci ? "\nid author " : " by ")
                .Append("Tord Romstad, Marco Costalba, Joona Kiiski and Balint Pfliegel");
            return sb.ToString();
        }

        #endregion

        #region Move printing

        /// move_to_uci() converts a move to a string in coordinate notation
        /// (g1f3, a7a8q, etc.). The only special case is castling moves, where we print
        /// in the e1g1 notation in normal chess mode, and in e1h1 notation in chess960
        /// mode. Internally castle moves are always coded as "king captures rook".
        internal static string move_to_uci(int m, bool chess960)
        {
            var from = from_sq(m);
            var to = to_sq(m);
            var promotion = string.Empty;

            if (m == MoveC.MOVE_NONE)
            {
                return "(none)";
            }

            if (m == MoveC.MOVE_NULL)
            {
                return "0000";
            }

            if (type_of_move(m) == MoveTypeC.CASTLING && !chess960)
            {
                to = make_square(to > from ? FileC.FILE_G : FileC.FILE_C, rank_of(from));
            }

            string move = square_to_string(from) + square_to_string(to);
            if (type_of_move(m) == MoveTypeC.PROMOTION)
            {
                move += PieceToChar[ColorC.BLACK][promotion_type(m)]; // Lower case
            }

            return move;
        }

        /// move_from_uci() takes a position and a string representing a move in
        /// simple coordinate notation and returns an equivalent legal Move if any.
        internal static int move_from_uci(Position pos, string str)
        {
            var strLowerPromotion = (str.Length == 5 ? str.Substring(0, 4) + str.Substring(4).ToLowerInvariant() : str);
            var mlist = MListBroker.GetObject();
            mlist.pos = 0;
            Movegen.generate_legal(pos, mlist.moves, ref mlist.pos);
            for (var i = 0; i < mlist.pos; i++)
            {
                if (strLowerPromotion == move_to_uci(mlist.moves[i].move, pos.chess960))
                {
                    var retval = mlist.moves[i].move;
                    MListBroker.Free();
                    return retval;
                }
            }
            MListBroker.Free();
            return MoveC.MOVE_NONE;
        }

        /// move_to_san() takes a position and a legal Move as input and returns its
        /// short algebraic notation representation.
        internal static string move_to_san(Position pos, int m)
        {
            if (m == MoveC.MOVE_NONE)
            {
                return "(none)";
            }

            if (m == MoveC.MOVE_NULL)
            {
                return "(null)";
            }

            Debug.Assert(pos.move_is_legal(m));

            Bitboard others, b;
            Color us = pos.sideToMove;
            var san = new StringBuilder();

            Square from = from_sq(m);
            Square to = to_sq(m);
            Piece pc = pos.piece_on(from);
            PieceType pt = type_of(pc);

            if (type_of_move(m) == MoveTypeC.CASTLING)
            {
                san.Append(to > from ? "O-O" : "O-O-O");
            }
            else
            {
                if (pt != PieceTypeC.PAWN)
                {
                    san.Append(PieceToChar[ColorC.WHITE][pt]); // Upper case

                    // Disambiguation if we have more then one piece of type 'pt' that can
                    // reach 'to' with a legal move.
                    others = b = (pos.attacks_from_PS(pc, to) & pos.pieces_PTC(pt, us)) ^ (ulong)from;
                    while (others != 0)
                    {
                        Move move = make_move(pop_lsb(ref b), to);
                        if (!pos.pl_move_is_legal(move, pos.pinned_pieces()))
                        {
                            others ^= (ulong)from_sq(move);
                        }
                    }

                    if (others != 0)
                    {
                        if ((others & file_bb_S(from)) == 0)
                        {
                            san.Append(file_to_char(file_of(from)));
                        }
                        else if ((others & rank_bb_S(from)) == 0)
                        {
                            san.Append(rank_to_char(rank_of(from)));
                        }
                        else
                        {
                            san.Append(square_to_string(from));
                        }
                    }
                }
                else if (pos.is_capture(m))
                {
                    san.Append(file_to_char(file_of(from)));
                }

                if (pos.is_capture(m))
                {
                    san.Append('x');
                }

                san.Append(square_to_string(to));

                if (type_of_move(m) == MoveTypeC.PROMOTION)
                {
                    san.Append('=');
                    san.Append(PieceToChar[ColorC.WHITE][promotion_type(m)]);
                }
            }

            var ci = CheckInfoBroker.GetObject();
            ci.CreateCheckInfo(pos);
            if (pos.move_gives_check(m, ci))
            {
                var st = new StateInfo();
                pos.do_move(m, st);
                var mlist = MListBroker.GetObject();
                mlist.pos = 0;
                Movegen.generate_legal(pos, mlist.moves, ref mlist.pos);
                san.Append(mlist.pos > 0 ? "+" : "#");
                MListBroker.Free();
                pos.undo_move(m);
            }
            CheckInfoBroker.Free();

            return san.ToString();
        }

        #endregion

        #region Scores

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int make_score(int mg, int eg)
        {
            return ((mg << 16) + eg);
        }

        /// Extracting the signed lower and upper 16 bits it not so trivial because
        /// according to the standard a simple cast to short is implementation defined
        /// and so is a right shift of a signed integer.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static int mg_value(int s)
        {
            return (((s + 32768) & ~0xffff) / 0x10000);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int eg_value(int s)
        {
            return ((short)(s & 0xffff));
        }

        // apply_weight() applies an evaluation weight to a value trying to prevent overflow
        internal static int apply_weight(int v, int w)
        {
            return ((((((v + 32768) & ~0xffff) / 0x10000 * (((w + 32768) & ~0xffff) / 0x10000)) / 0x100) << 16)
                    + (((short)(v & 0xffff) * ((short)(w & 0xffff))) / 0x100));
        }

        #endregion

        #region Sort and existance

        internal static void sort(MoveStack[] data, int firstMove, int lastMove)
        {
            MoveStack tmp;
            int p, q;

            for (p = firstMove + 1; p < lastMove; p++)
            {
                tmp = data[p];
                for (q = p; q != firstMove && data[q - 1].score < tmp.score; --q)
                {
                    data[q] = data[q - 1];
                }
                data[q] = tmp;
            }
        }

        internal static void sort(List<RootMove> data, int firstMove, int lastMove)
        {
            RootMove tmp;
            int p, q;

            for (p = firstMove + 1; p < lastMove; p++)
            {
                tmp = data[p];
                for (q = p; q != firstMove && data[q - 1].score < tmp.score; --q)
                {
                    data[q] = data[q - 1];
                }
                data[q] = tmp;
            }
        }

        internal static bool existSearchMove(List<int> moves, int m) // count elements that match _Val
        {
            var moveLength = moves.Count;
            if (moveLength == 0)
            {
                return false;
            }
            for (var i = 0; i < moveLength; i++)
            {
                if (moves[i] == m)
                {
                    return true;
                }
            }
            return false;
        }

        internal static bool existRootMove(List<RootMove> moves, int m) // count elements that match _Val
        {
            var moveLength = moves.Count;
            if (moveLength == 0)
            {
                return false;
            }
            for (var i = 0; i < moveLength; i++)
            {
                if (moves[i].pv[0] == m)
                {
                    return true;
                }
            }
            return false;
        }

        #endregion

        #region PSQT

        /// PSQT[PieceType][Square] contains Piece-Square scores. For each piece type on
        /// a given square a (midgame, endgame) score pair is assigned. PSQT is defined
        /// for white side, for black side the tables are symmetric.
        internal static readonly int[][] PSQT =
            {
                new int[] { }, new[]
                                   {
                                       // Pawn
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(-28, -8), make_score(-6, -8),
                                       make_score(4, -8), make_score(14, -8),
                                       make_score(14, -8), make_score(4, -8),
                                       make_score(-6, -8), make_score(-28, -8),
                                       make_score(-28, -8), make_score(-6, -8),
                                       make_score(9, -8), make_score(36, -8),
                                       make_score(36, -8), make_score(9, -8),
                                       make_score(-6, -8), make_score(-28, -8),
                                       make_score(-28, -8), make_score(-6, -8),
                                       make_score(17, -8), make_score(58, -8),
                                       make_score(58, -8), make_score(17, -8),
                                       make_score(-6, -8), make_score(-28, -8),
                                       make_score(-28, -8), make_score(-6, -8),
                                       make_score(17, -8), make_score(36, -8),
                                       make_score(36, -8), make_score(17, -8),
                                       make_score(-6, -8), make_score(-28, -8),
                                       make_score(-28, -8), make_score(-6, -8),
                                       make_score(9, -8), make_score(14, -8),
                                       make_score(14, -8), make_score(9, -8),
                                       make_score(-6, -8), make_score(-28, -8),
                                       make_score(-28, -8), make_score(-6, -8),
                                       make_score(4, -8), make_score(14, -8),
                                       make_score(14, -8), make_score(4, -8),
                                       make_score(-6, -8), make_score(-28, -8),
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(0, 0), make_score(0, 0),
                                       make_score(0, 0), make_score(0, 0)
                                   },
                new[]
                    {
                        // Knight
                        make_score(-135, -104), make_score(-107, -79),
                        make_score(-80, -55), make_score(-67, -42),
                        make_score(-67, -42), make_score(-80, -55),
                        make_score(-107, -79), make_score(-135, -104),
                        make_score(-93, -79), make_score(-67, -55),
                        make_score(-39, -30), make_score(-25, -17),
                        make_score(-25, -17), make_score(-39, -30),
                        make_score(-67, -55), make_score(-93, -79),
                        make_score(-53, -55), make_score(-25, -30),
                        make_score(1, -6), make_score(13, 5), make_score(13, 5),
                        make_score(1, -6), make_score(-25, -30),
                        make_score(-53, -55), make_score(-25, -42),
                        make_score(1, -17), make_score(27, 5), make_score(41, 18),
                        make_score(41, 18), make_score(27, 5), make_score(1, -17),
                        make_score(-25, -42), make_score(-11, -42),
                        make_score(13, -17), make_score(41, 5), make_score(55, 18),
                        make_score(55, 18), make_score(41, 5), make_score(13, -17),
                        make_score(-11, -42), make_score(-11, -55),
                        make_score(13, -30), make_score(41, -6), make_score(55, 5),
                        make_score(55, 5), make_score(41, -6), make_score(13, -30),
                        make_score(-11, -55), make_score(-53, -79),
                        make_score(-25, -55), make_score(1, -30),
                        make_score(13, -17), make_score(13, -17), make_score(1, -30),
                        make_score(-25, -55), make_score(-53, -79),
                        make_score(-193, -104), make_score(-67, -79),
                        make_score(-39, -55), make_score(-25, -42),
                        make_score(-25, -42), make_score(-39, -55),
                        make_score(-67, -79), make_score(-193, -104)
                    },
                new[]
                    {
                        // Bishop
                        make_score(-40, -59), make_score(-40, -42),
                        make_score(-35, -35), make_score(-30, -26),
                        make_score(-30, -26), make_score(-35, -35),
                        make_score(-40, -42), make_score(-40, -59),
                        make_score(-17, -42), make_score(0, -26),
                        make_score(-4, -18), make_score(0, -11), make_score(0, -11),
                        make_score(-4, -18), make_score(0, -26),
                        make_score(-17, -42), make_score(-13, -35),
                        make_score(-4, -18), make_score(8, -11), make_score(4, -4),
                        make_score(4, -4), make_score(8, -11), make_score(-4, -18),
                        make_score(-13, -35), make_score(-8, -26),
                        make_score(0, -11), make_score(4, -4), make_score(17, 4),
                        make_score(17, 4), make_score(4, -4), make_score(0, -11),
                        make_score(-8, -26), make_score(-8, -26), make_score(0, -11),
                        make_score(4, -4), make_score(17, 4), make_score(17, 4),
                        make_score(4, -4), make_score(0, -11), make_score(-8, -26),
                        make_score(-13, -35), make_score(-4, -18),
                        make_score(8, -11), make_score(4, -4), make_score(4, -4),
                        make_score(8, -11), make_score(-4, -18),
                        make_score(-13, -35), make_score(-17, -42),
                        make_score(0, -26), make_score(-4, -18), make_score(0, -11),
                        make_score(0, -11), make_score(-4, -18), make_score(0, -26),
                        make_score(-17, -42), make_score(-17, -59),
                        make_score(-17, -42), make_score(-13, -35),
                        make_score(-8, -26), make_score(-8, -26),
                        make_score(-13, -35), make_score(-17, -42),
                        make_score(-17, -59)
                    },
                new[]
                    {
                        // Rook
                        make_score(-12, 3), make_score(-7, 3), make_score(-2, 3),
                        make_score(2, 3), make_score(2, 3), make_score(-2, 3),
                        make_score(-7, 3), make_score(-12, 3), make_score(-12, 3),
                        make_score(-7, 3), make_score(-2, 3), make_score(2, 3),
                        make_score(2, 3), make_score(-2, 3), make_score(-7, 3),
                        make_score(-12, 3), make_score(-12, 3), make_score(-7, 3),
                        make_score(-2, 3), make_score(2, 3), make_score(2, 3),
                        make_score(-2, 3), make_score(-7, 3), make_score(-12, 3),
                        make_score(-12, 3), make_score(-7, 3), make_score(-2, 3),
                        make_score(2, 3), make_score(2, 3), make_score(-2, 3),
                        make_score(-7, 3), make_score(-12, 3), make_score(-12, 3),
                        make_score(-7, 3), make_score(-2, 3), make_score(2, 3),
                        make_score(2, 3), make_score(-2, 3), make_score(-7, 3),
                        make_score(-12, 3), make_score(-12, 3), make_score(-7, 3),
                        make_score(-2, 3), make_score(2, 3), make_score(2, 3),
                        make_score(-2, 3), make_score(-7, 3), make_score(-12, 3),
                        make_score(-12, 3), make_score(-7, 3), make_score(-2, 3),
                        make_score(2, 3), make_score(2, 3), make_score(-2, 3),
                        make_score(-7, 3), make_score(-12, 3), make_score(-12, 3),
                        make_score(-7, 3), make_score(-2, 3), make_score(2, 3),
                        make_score(2, 3), make_score(-2, 3), make_score(-7, 3),
                        make_score(-12, 3)
                    },
                new[]
                    {
                        // Queen
                        make_score(8, -80), make_score(8, -54), make_score(8, -42),
                        make_score(8, -30), make_score(8, -30), make_score(8, -42),
                        make_score(8, -54), make_score(8, -80), make_score(8, -54),
                        make_score(8, -30), make_score(8, -18), make_score(8, -6),
                        make_score(8, -6), make_score(8, -18), make_score(8, -30),
                        make_score(8, -54), make_score(8, -42), make_score(8, -18),
                        make_score(8, -6), make_score(8, 6), make_score(8, 6),
                        make_score(8, -6), make_score(8, -18), make_score(8, -42),
                        make_score(8, -30), make_score(8, -6), make_score(8, 6),
                        make_score(8, 18), make_score(8, 18), make_score(8, 6),
                        make_score(8, -6), make_score(8, -30), make_score(8, -30),
                        make_score(8, -6), make_score(8, 6), make_score(8, 18),
                        make_score(8, 18), make_score(8, 6), make_score(8, -6),
                        make_score(8, -30), make_score(8, -42), make_score(8, -18),
                        make_score(8, -6), make_score(8, 6), make_score(8, 6),
                        make_score(8, -6), make_score(8, -18), make_score(8, -42),
                        make_score(8, -54), make_score(8, -30), make_score(8, -18),
                        make_score(8, -6), make_score(8, -6), make_score(8, -18),
                        make_score(8, -30), make_score(8, -54), make_score(8, -80),
                        make_score(8, -54), make_score(8, -42), make_score(8, -30),
                        make_score(8, -30), make_score(8, -42), make_score(8, -54),
                        make_score(8, -80)
                    },
                new[]
                    {
                        // King
                        make_score(287, 18), make_score(311, 77),
                        make_score(262, 105), make_score(214, 135),
                        make_score(214, 135), make_score(262, 105),
                        make_score(311, 77), make_score(287, 18),
                        make_score(262, 77), make_score(287, 135),
                        make_score(238, 165), make_score(190, 193),
                        make_score(190, 193), make_score(238, 165),
                        make_score(287, 135), make_score(262, 77),
                        make_score(214, 105), make_score(238, 165),
                        make_score(190, 193), make_score(142, 222),
                        make_score(142, 222), make_score(190, 193),
                        make_score(238, 165), make_score(214, 105),
                        make_score(190, 135), make_score(214, 193),
                        make_score(167, 222), make_score(119, 251),
                        make_score(119, 251), make_score(167, 222),
                        make_score(214, 193), make_score(190, 135),
                        make_score(167, 135), make_score(190, 193),
                        make_score(142, 222), make_score(94, 251),
                        make_score(94, 251), make_score(142, 222),
                        make_score(190, 193), make_score(167, 135),
                        make_score(142, 105), make_score(167, 165),
                        make_score(119, 193), make_score(69, 222),
                        make_score(69, 222), make_score(119, 193),
                        make_score(167, 165), make_score(142, 105),
                        make_score(119, 77), make_score(142, 135),
                        make_score(94, 165), make_score(46, 193),
                        make_score(46, 193), make_score(94, 165),
                        make_score(142, 135), make_score(119, 77),
                        make_score(94, 18), make_score(119, 77), make_score(69, 105),
                        make_score(21, 135), make_score(21, 135),
                        make_score(69, 105), make_score(119, 77), make_score(94, 18)
                    }
            };

        #endregion

        #region Debug methods

        /// Debug functions used mainly to collect run-time statistics
        private static readonly ulong[] hits = new ulong[2];

        private static readonly ulong[] means = new ulong[2];

        internal static void dbg_hit_on(bool b)
        {
            hits[0]++;
            if (b)
            {
                hits[1]++;
            }
        }

        internal static void dbg_hit_on_c(bool c, bool b)
        {
            if (c)
            {
                dbg_hit_on(b);
            }
        }

        internal static void dbg_mean_of(int v)
        {
            means[0]++;
            means[1] += (ulong)v;
        }

        internal static void dbg_print()
        {
            if (hits[0] != 0)
            {
                Plug.Write("Total ");
                Plug.Write(hits[0].ToString());
                Plug.Write(" Hits ");
                Plug.Write(hits[1].ToString());
                Plug.Write(" hit rate (%) ");
                Plug.Write((100 * hits[1] / hits[0]).ToString());
                Plug.Write(Constants.endl);
            }

            if (means[0] != 0)
            {
                Plug.Write("Total ");
                Plug.Write(means[0].ToString());
                Plug.Write(" Mean ");
                Plug.Write(((float)means[1] / means[0]).ToString());
                Plug.Write(Constants.endl);
            }
        }

        #endregion
    }
}