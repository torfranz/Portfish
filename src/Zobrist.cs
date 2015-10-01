using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Portfish
{
    internal static class Zobrist
    {
        internal static readonly int[][] PieceSquareTable = new int[PieceC.PIECE_NB][]; // [piece][square] 16, 64

        internal static readonly ulong[][][] psq = new ulong[ColorC.COLOR_NB][][];
        // [color][pieceType][square / piece count]

        internal static readonly ulong[] enpassant = new ulong[FileC.FILE_NB]; // [square]

        internal static readonly ulong[] castle = new ulong[CastleRightC.CASTLE_RIGHT_NB]; // [castleRight]

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
                psq[c] = new ulong[PieceTypeC.PIECE_TYPE_NB][];
                for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
                {
                    psq[c][pt] = new ulong[SquareC.SQUARE_NB];
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

            for (var i = 0; i < PieceC.PIECE_NB; i++)
            {
                PieceSquareTable[i] = new int[SquareC.SQUARE_NB];
            }

            for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
            {
                Position.PieceValue[PhaseC.MG][Utils.make_piece(ColorC.BLACK, pt)] = Position.PieceValue[PhaseC.MG][pt];
                Position.PieceValue[PhaseC.EG][Utils.make_piece(ColorC.BLACK, pt)] = Position.PieceValue[PhaseC.EG][pt];
                
                var v = Utils.make_score(Position.PieceValue[PhaseC.MG][pt], Position.PieceValue[PhaseC.EG][pt]);
                
                for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
                {
                    PieceSquareTable[Utils.make_piece(ColorC.WHITE, pt)][s] = (v + Utils.PSQT[pt][s]);
                    PieceSquareTable[Utils.make_piece(ColorC.BLACK, pt)][Utils.flip_S(s)] = -(v + Utils.PSQT[pt][s]);
                }
            }
        }
    }
}
