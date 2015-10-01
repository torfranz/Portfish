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
    internal static class Constants
    {
        // Set to true to force running with one thread. Used for debugging
        internal const bool FakeSplit = false;

        // Material and pawn table sizes
        internal const int MaterialTableSize = 8192;

        internal const int PawnTableSize = 16384;

        // Number of CPUs
        internal const int NumberOfCPUs = 4;

        internal const int BrokerCapacity = 4;

        internal const int MaterialTableMask = MaterialTableSize - 1;

        internal const int PawnTableMask = PawnTableSize - 1;

        internal const uint UInt32One = 1;

        // This is the number of TTEntry slots for each cluster
        internal const uint ClusterSize = 4;

        internal const string endl = "\n";

        internal const int BROKER_SLOTS = 128;

        internal const int BROKER_SLOT_MASK = BROKER_SLOTS - 1;

        internal const int MAX_THREADS = 32;

        internal const int MAX_SPLITPOINTS_PER_THREAD = 8;

        internal const int MAX_MOVES = 192;

        internal const int MAX_PLY = 100;

        internal const int MAX_PLY_PLUS_2 = MAX_PLY + 2;

        internal const int INT_MIN = (-2147483647 - 1); /* minimum (signed) int value */

        internal const int INT_MAX = 2147483647; /* maximum (signed) int value */

        internal const ulong FileABB = 0x0101010101010101UL;

        internal const ulong FileBBB = FileABB << 1;

        internal const ulong FileCBB = FileABB << 2;

        internal const ulong FileDBB = FileABB << 3;

        internal const ulong FileEBB = FileABB << 4;

        internal const ulong FileFBB = FileABB << 5;

        internal const ulong FileGBB = FileABB << 6;

        internal const ulong FileHBB = FileABB << 7;

        internal const ulong Rank1BB = 0xFF;

        internal const ulong Rank2BB = Rank1BB << (8 * 1);

        internal const ulong Rank3BB = Rank1BB << (8 * 2);

        internal const ulong Rank4BB = Rank1BB << (8 * 3);

        internal const ulong Rank5BB = Rank1BB << (8 * 4);

        internal const ulong Rank6BB = Rank1BB << (8 * 5);

        internal const ulong Rank7BB = Rank1BB << (8 * 6);

        internal const ulong Rank8BB = Rank1BB << (8 * 7);

        internal const int PawnValueMidgame = 198;

        internal const int PawnValueEndgame = 258;

        internal const int KnightValueMidgame = 817;

        internal const int KnightValueEndgame = 846;

        internal const int BishopValueMidgame = 836;

        internal const int BishopValueEndgame = 857;

        internal const int RookValueMidgame = 1270;

        internal const int RookValueEndgame = 1278;

        internal const int QueenValueMidgame = 2521;

        internal const int QueenValueEndgame = 2558;
    }
}