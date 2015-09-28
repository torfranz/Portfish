using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Portfish
{
    internal static class Zobrist
    {
        internal static readonly int[][] PieceSquareTable = new int[16][]; // [piece][square] 16, 64

        internal static readonly ulong[][][] psq = new ulong[2][][];
        // [color][pieceType][square / piece count]

        internal static readonly ulong[] enpassant = new ulong[8]; // [square]

        internal static readonly ulong[] castle = new ulong[16]; // [castleRight]

        internal static ulong side;

        internal static ulong exclusion;

        /// init() is a static member function which initializes at startup
        /// the various arrays used to compute hash keys and the piece square tables.
        /// The latter is a two-step operation: First, the white halves of the tables
        /// are copied from PSQT[] tables. Second, the black halves of the tables are
        /// initialized by flipping and changing the sign of the white scores.
        internal static void init()
        {
            var rk = new RKISS();

            for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
            {
                psq[c] = new ulong[8][];
                for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
                {
                    psq[c][pt] = new ulong[64];
                    for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
                    {
                        psq[c][pt][s] = rk.rand();
                    }
                }
            }

            for (var f = FileC.FILE_A; f <= FileC.FILE_H; f++)
            {
                enpassant[f] = rk.rand();
            }

            ulong one = 1;
            for (var cr = CastleRightC.CASTLES_NONE; cr <= CastleRightC.ALL_CASTLES; cr++)
            {
                var b = (ulong)cr;
                while (b != 0)
                {
                    var k = castle[one << Utils.pop_lsb(ref b)];
                    castle[cr] ^= ((k != 0) ? k : rk.rand());
                }
            }

            side = rk.rand();
            exclusion = rk.rand();

            for (var i = 0; i < 16; i++)
            {
                PieceSquareTable[i] = new int[64];
            }

            for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
            {
                Position.PieceValue[Constants.Midgame][Utils.make_piece(ColorC.BLACK, pt)] = Position.PieceValue[Constants.Midgame][pt];
                Position.PieceValue[Constants.Endgame][Utils.make_piece(ColorC.BLACK, pt)] = Position.PieceValue[Constants.Endgame][pt];
                
                var v = Utils.make_score(Position.PieceValue[Constants.Midgame][pt], Position.PieceValue[Constants.Endgame][pt]);
                
                for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
                {
                    PieceSquareTable[Utils.make_piece(ColorC.WHITE, pt)][s] = (v + Utils.PSQT[pt][s]);
                    PieceSquareTable[Utils.make_piece(ColorC.BLACK, pt)][Utils.flip_S(s)] = -(v + Utils.PSQT[pt][s]);
                }
            }
        }
    }
}
