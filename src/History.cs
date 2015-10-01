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
    using System;
    using System.Runtime.CompilerServices;

    /// The History class stores statistics about how often different moves
    /// have been successful or unsuccessful during the current search. These
    /// statistics are used for reduction and move ordering decisions. History
    /// entries are stored according only to moving piece and destination square,
    /// in particular two moves with different origin but same destination and
    /// same piece will be considered identical.
    internal sealed class History
    {
        internal const int MaxValue = 2000;

        internal readonly int[][] history = new int[PieceC.PIECE_NB][]; // [piece][to_square] 16, 64

        internal readonly int[][] maxGains = new int[PieceC.PIECE_NB][]; // [piece][to_square] 16, 64

        internal History()
        {
            for (var i = 0; i < PieceC.PIECE_NB; i++)
            {
                this.history[i] = new int[SquareC.SQUARE_NB];
                this.maxGains[i] = new int[SquareC.SQUARE_NB];
            }
        }

        internal void clear()
        {
            for (var i = 0; i < 16; i++)
            {
                Array.Clear(this.history[i], 0, 64);
                Array.Clear(this.maxGains[i], 0, 64);
            }
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int value(int p, int to)
        {
            return this.history[p][to];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal void add(int p, int to, int bonus)
        {
            if (Math.Abs(this.history[p][to] + bonus) < MaxValue)
            {
                this.history[p][to] += bonus;
            }
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int gain(int p, int to)
        {
            return this.maxGains[p][to];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal void update_gain(int p, int to, int g)
        {
            this.maxGains[p][to] = Math.Max(g, this.maxGains[p][to] - 1);
        }
    }
}