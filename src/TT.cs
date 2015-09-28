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
using TracedType = System.Int32;

namespace Portfish
{
    using System;
    using System.Runtime.CompilerServices;

    /// The TTEntry is the class of transposition table entries
    /// 
    /// A TTEntry needs 128 bits to be stored
    /// 
    /// bit  0-31: key
    /// bit 32-63: data
    /// bit 64-79: value
    /// bit 80-95: depth
    /// bit 96-111: static value
    /// bit 112-127: margin of static value
    /// 
    /// the 32 bits of the data field are so defined
    /// 
    /// bit  0-15: move
    /// bit 16-20: not used
    /// bit 21-22: value type
    /// bit 23-31: generation
    internal struct TTEntry
    {
        internal uint key;

        internal ushort move16;

        internal byte bound, generation8;

        internal short value16, depth16, staticValue, staticMargin;

        internal void save(uint k, int v, Bound t, int d, int m, int g, int statV, int statM)
        {
            this.key = k;
            this.move16 = (ushort)m;
            this.bound = (byte)t;
            this.generation8 = (byte)g;
            this.value16 = (short)v;
            this.depth16 = (short)d;
            this.staticValue = (short)statV;
            this.staticMargin = (short)statM;
        }

        internal void set_generation(int g)
        {
            this.generation8 = (byte)g;
        }

        //internal UInt32 key() { return key32; }
        internal int depth()
        {
            return this.depth16;
        }

        internal int move()
        {
            return this.move16;
        }

        internal int value()
        {
            return this.value16;
        }

        internal Bound type()
        {
            return (Bound)this.bound;
        }

        internal int generation()
        {
            return this.generation8;
        }

        internal int static_value()
        {
            return this.staticValue;
        }

        internal int static_value_margin()
        {
            return this.staticMargin;
        }
    };

    /// The transposition table class. This is basically just a huge array containing
    /// TTCluster objects, and a few methods for writing and reading entries.
    internal static class TT
    {
        internal static readonly TTEntry StaticEntry = new TTEntry();

        internal static uint size;

        internal static uint sizeMask;

        internal static TTEntry[] entries;

        internal static byte generation; // Size must be not bigger then TTEntry::generation8

        /// TranspositionTable::refresh() updates the 'generation' value of the TTEntry
        /// to avoid aging. Normally called after a TT hit.
        /// TranspositionTable::set_size() sets the size of the transposition table,
        /// measured in megabytes.
        internal static void set_size(uint mbSize)
        {
            uint newSize = 1024;

            // Transposition table consists of clusters and each cluster consists
            // of ClusterSize number of TTEntries. Each non-empty entry contains
            // information of exactly one position and newSize is the number of
            // clusters we are going to allocate.
            while (2UL * newSize * 64 <= (mbSize << 20))
            {
                newSize *= 2;
            }

            if (newSize == size)
            {
                return;
            }

            size = newSize;
            sizeMask = size - 1;

            entries = new TTEntry[size * 4];
        }

        /// TranspositionTable::clear() overwrites the entire transposition table
        /// with zeroes. It is called whenever the table is resized, or when the
        /// user asks the program to clear the table (from the UCI interface).
        internal static void clear()
        {
            if (entries != null)
            {
                Array.Clear(entries, 0, entries.Length);
            }
        }

        /// TranspositionTable::store() writes a new entry containing position key and
        /// valuable information of current position. The lowest order bits of position
        /// key are used to decide on which cluster the position will be placed.
        /// When a new entry is written and there are no empty entries available in cluster,
        /// it replaces the least valuable of entries. A TTEntry t1 is considered to be
        /// more valuable than a TTEntry t2 if t1 is from the current search and t2 is from
        /// a previous search, or if the depth of t1 is bigger than the depth of t2.
        internal static void store(ulong posKey, int v, Bound t, int d, int m, int statV, int kingD)
        {
            var posKey32 = (uint)(posKey >> 32); // Use the high 32 bits as key inside the cluster
            uint ttePos = 0;
            uint replacePos = 0;
            ttePos = replacePos = (((uint)posKey) & sizeMask) << 2;

            for (uint i = 0; i < Constants.ClusterSize; i++)
            {
                var tte = entries[ttePos];

                if ((tte.key == 0) || tte.key == posKey32) // Empty or overwrite old
                {
                    // Preserve any existing ttMove
                    if (m == MoveC.MOVE_NONE)
                    {
                        m = tte.move16;
                    }

                    entries[ttePos].save(posKey32, v, t, d, m, generation, statV, kingD);
                    return;
                }

                // Implement replace strategy
                //if ((entries[replacePos].generation8 == generation ? 2 : 0) + (tte.generation8 == generation || tte.bound == 3/*Bound.BOUND_EXACT*/ ? -2 : 0) + (tte.depth16 < entries[replacePos].depth16 ? 1 : 0) > 0)
                //{
                //    replacePos = ttePos;
                //}

                if (entries[replacePos].generation8 == generation)
                {
                    // +2
                    if (tte.generation8 == generation || tte.bound == 3 /*Bound.BOUND_EXACT*/)
                    {
                        // 0
                        if (tte.depth16 < entries[replacePos].depth16)
                        {
                            // +1
                            replacePos = ttePos;
                        }
                    }
                    else
                    {
                        // +2
                        replacePos = ttePos;
                    }
                }
                else
                {
                    // 0
                    if ((!(tte.generation8 == generation || tte.bound == 3 /*Bound.BOUND_EXACT*/))
                        && (tte.depth16 < entries[replacePos].depth16))
                    {
                        // +1
                        replacePos = ttePos;
                    }
                }

                ttePos++;
            }
            entries[replacePos].save(posKey32, v, t, d, m, generation, statV, kingD);
        }

        /// TranspositionTable::probe() looks up the current position in the
        /// transposition table. Returns a pointer to the TTEntry or NULL if
        /// position is not found.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static bool probe(ulong posKey, ref uint ttePos, out TTEntry entry)
        {
            var posKey32 = (uint)(posKey >> 32);
            var offset = (((uint)posKey) & sizeMask) << 2;

            for (var i = offset; i < (Constants.ClusterSize + offset); i++)
            {
                if (entries[i].key == posKey32)
                {
                    ttePos = i;
                    entry = entries[i];
                    return true;
                }
            }

            entry = StaticEntry;
            return false;
        }

        /// TranspositionTable::new_search() is called at the beginning of every new
        /// search. It increments the "generation" variable, which is used to
        /// distinguish transposition table entries from previous searches from
        /// entries from the current search.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static void new_search()
        {
            generation++;
        }
    };
}