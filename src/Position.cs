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
    using System.Text;

    internal sealed class CheckInfo
    {
        internal readonly ulong[] checkSq = new ulong[PieceTypeC.PIECE_TYPE_NB];

        internal ulong dcCandidates;

        internal int ksq;

        internal ulong pinned;

        // ALL CALLS INLINED
        //internal CheckInfo(Position pos)
        //{
        //    Color them = Utils.flip_C(pos.sideToMove);
        //    ksq = pos.king_square(them);

        //    pinned = pos.pinned_pieces();
        //    dcCandidates = pos.discovered_check_candidates();

        //    checkSq[PieceTypeC.PAWN] = Position.attacks_from_PAWN(ksq, them);
        //    checkSq[PieceTypeC.KNIGHT] = Position.attacks_from_KNIGHT(ksq);
        //    checkSq[PieceTypeC.BISHOP] = pos.attacks_from_BISHOP(ksq);
        //    checkSq[PieceTypeC.ROOK] = pos.attacks_from_ROOK(ksq);
        //    checkSq[PieceTypeC.QUEEN] = checkSq[PieceTypeC.BISHOP] | checkSq[PieceTypeC.ROOK];
        //    checkSq[PieceTypeC.KING] = 0;
        //}

        internal void CreateCheckInfo(Position pos)
        {
            var them = pos.sideToMove ^ 1;
            this.ksq = pos.pieceList[them][PieceTypeC.KING][0];

            this.pinned = pos.pinned_pieces();
            this.dcCandidates = pos.discovered_check_candidates();

            this.checkSq[PieceTypeC.PAWN] = Utils.StepAttacksBB[((them << 3) | PieceTypeC.PAWN)][this.ksq];
            this.checkSq[PieceTypeC.KNIGHT] = Utils.StepAttacksBB_KNIGHT[this.ksq];
#if X64
            checkSq[PieceTypeC.BISHOP] = Utils.BAttacks[ksq][(((pos.occupied_squares & Utils.BMasks[ksq]) * Utils.BMagics[ksq]) >> Utils.BShifts[ksq])];
            checkSq[PieceTypeC.ROOK] = Utils.RAttacks[ksq][(((pos.occupied_squares & Utils.RMasks[ksq]) * Utils.RMagics[ksq]) >> Utils.RShifts[ksq])];
#else
            this.checkSq[PieceTypeC.BISHOP] = pos.attacks_from_BISHOP(this.ksq);
            this.checkSq[PieceTypeC.ROOK] = pos.attacks_from_ROOK(this.ksq);
#endif
            this.checkSq[PieceTypeC.QUEEN] = this.checkSq[PieceTypeC.BISHOP] | this.checkSq[PieceTypeC.ROOK];
            this.checkSq[PieceTypeC.KING] = 0;
        }
    };

    /// The StateInfo struct stores information we need to restore a Position
    /// object to its previous state when we retract a move. Whenever a move
    /// is made on the board (by calling do_move), an StateInfo object
    /// must be passed as a parameter.
    internal sealed class StateInfo
    {
        internal int capturedType;

        internal int castleRights, rule50, pliesFromNull;

        internal ulong checkersBB;

        internal int epSquare;

        internal ulong key;

        internal int npMaterialWHITE, npMaterialBLACK;

        internal ulong pawnKey, materialKey;

        internal StateInfo previous;

        internal int psqScore;

        internal void Clear()
        {
            this.pawnKey = 0;
            this.materialKey = 0;
            this.npMaterialWHITE = 0;
            this.npMaterialBLACK = 0;
            this.castleRights = 0;
            this.rule50 = 0;
            this.pliesFromNull = 0;
            this.psqScore = 0;
            this.epSquare = 0;

            this.key = 0;
            this.checkersBB = 0;
            this.capturedType = 0;
            this.previous = null;
        }

        //    newSI.pliesFromNull = oldSI.pliesFromNull;
        //    newSI.rule50 = oldSI.rule50;
        //    newSI.castleRights = oldSI.castleRights;
        //    newSI.npMaterialBLACK = oldSI.npMaterialBLACK;
        //    newSI.npMaterialWHITE = oldSI.npMaterialWHITE;
        //    newSI.materialKey = oldSI.materialKey;
        //    newSI.pawnKey = oldSI.pawnKey;
        //{
        //internal static void CopyReducedStateInfo(StateInfo newSI, StateInfo oldSI)

        // ALL CALLS INLINED
        //    newSI.psqScore = oldSI.psqScore;
        //    newSI.epSquare = oldSI.epSquare;
        //}
    };

    /// The position data structure. A position consists of the following data:
    /// 
    /// * For each piece type, a bitboard representing the squares occupied
    /// by pieces of that type.
    /// * For each color, a bitboard representing the squares occupied by
    /// pieces of that color.
    /// * A bitboard of all occupied squares.
    /// * A bitboard of all checking pieces.
    /// * A 64-entry array of pieces, indexed by the squares of the board.
    /// * The current side to move.
    /// * Information about the castling rights for both sides.
    /// * The initial files of the kings and both pairs of rooks. This is
    /// used to implement the Chess960 castling rules.
    /// * The en passant square (which is SQ_NONE if no en passant capture is
    /// possible).
    /// * The squares of the kings for both sides.
    /// * Hash keys for the position itself, the current pawn structure, and
    /// the current material situation.
    /// * Hash keys for all previous positions in the game for detecting
    /// repetition draws.
    /// * A counter for detecting 50 move rule draws.
    internal sealed class Position
    {
        // To convert a Piece to and from a FEN char
        private const string PieceToChar = " PNBRQK  pnbrqk  .";

        internal static readonly int[][] PieceValue =
            {
                new[]{ ValueC.VALUE_ZERO, Constants.PawnValueMidgame, Constants.KnightValueMidgame, Constants.BishopValueMidgame, Constants.RookValueMidgame, Constants.QueenValueMidgame, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
                new[]{ValueC. VALUE_ZERO, Constants.PawnValueEndgame, Constants.KnightValueEndgame, Constants.BishopValueEndgame, Constants.RookValueEndgame, Constants.QueenValueEndgame, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 }
            };

        // Static variables
        
        internal readonly int[] board = new int[SquareC.SQUARE_NB]; // [square]

        // Bitboards
        internal readonly ulong[] byTypeBB = new ulong[PieceTypeC.PIECE_TYPE_NB]; // [pieceType]

        internal readonly ulong[] byColorBB = new ulong[ColorC.COLOR_NB]; // [color]

        internal ulong occupied_squares; // byTypeBB[ALL_PIECES];

        // Piece counts
        internal readonly int[][] pieceCount = new int[ColorC.COLOR_NB][]; // [color][pieceType] 2, 8

        // Piece lists
        internal readonly int[][][] pieceList = new int[ColorC.COLOR_NB][][]; // [color][pieceType][index] 2, 8, 16

        internal readonly int[] index = new int[SquareC.SQUARE_NB]; // [square]

        // Other info
        internal readonly int[] castleRightsMask = new int[SquareC.SQUARE_NB]; // [square]

        internal readonly int[][] castleRookSquare = new int[ColorC.COLOR_NB][]; // [color][side] 2,2

        internal readonly ulong[][] castlePath = new ulong[ColorC.COLOR_NB][]; // [color][side] 2,2

        internal StateInfo startState = new StateInfo();

        internal long nodes;

        internal int startPosPly;

        internal int sideToMove;

        internal Thread thisThread;

        internal StateInfo st;

        internal bool chess960;

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int piece_on(int s)
        {
            return this.board[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int piece_moved(int m)
        {
            return this.board[((m >> 6) & 0x3F)];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool is_empty(int s)
        {
            return this.board[s] == PieceC.NO_PIECE;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong pieces_C(int c)
        {
            return this.byColorBB[c];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong pieces_PT(int pt)
        {
            return this.byTypeBB[pt];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong pieces_PTC(int pt, int c)
        {
            return this.byTypeBB[pt] & this.byColorBB[c];
        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal Bitboard pieces_PTPT(PieceType pt1, PieceType pt2)
        //        {
        //            return byTypeBB[pt1] | byTypeBB[pt2];
        //        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong pieces(int pt1, int pt2, int c)
        {
            return (this.byTypeBB[pt1] | this.byTypeBB[pt2]) & this.byColorBB[c];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int piece_count(int c, int pt)
        {
            return this.pieceCount[c][pt];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int king_square(int c)
        {
            return this.pieceList[c][PieceTypeC.KING][0];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int can_castle_CR(int f)
        {
            return this.st.castleRights & f;
        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal int can_castle_CR_bit(CastleRight f)
        //        {
        //            return (st.castleRights & f) != 0 ? 1 : 0;
        //        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int can_castle_C(int c)
        {
            return this.st.castleRights & (CastleRightC.WHITE_ANY << (c << 1));
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool castle_impeded(int c, int s)
        {
            return (this.occupied_squares & this.castlePath[c][s]) != 0;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int castle_rook_square(int c, int s)
        {
            return this.castleRookSquare[c][s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong attacks_from_PAWN(int s, int c)
        {
            return Utils.StepAttacksBB[((c << 3) | PieceTypeC.PAWN)][s];
        }

        // Knight and King and white pawns
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong attacks_from_PTS(int pieceType, int s)
        {
            if (pieceType == PieceTypeC.BISHOP)
            {
                return Utils.bishop_attacks_bb(s, this.occupied_squares);
            }
            if (pieceType == PieceTypeC.ROOK)
            {
                return Utils.rook_attacks_bb(s, this.occupied_squares);
            }
            if (pieceType == PieceTypeC.QUEEN)
            {
                return Utils.bishop_attacks_bb(s, this.occupied_squares)
                       | Utils.rook_attacks_bb(s, this.occupied_squares);
            }
            return Utils.StepAttacksBB[pieceType][s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong attacks_from_BISHOP(int s)
        {
#if X64
            return Utils.BAttacks[s][(((occupied_squares & Utils.BMasks[s]) * Utils.BMagics[s]) >> Utils.BShifts[s])];
#else
            var lo = (uint)(this.occupied_squares) & (uint)(Utils.BMasks[s]);
            var hi = (uint)(this.occupied_squares >> 32) & (uint)(Utils.BMasks[s] >> 32);
            return
                Utils.BAttacks[s][
                    (lo * (uint)(Utils.BMagics[s]) ^ hi * (uint)(Utils.BMagics[s] >> 32)) >> Utils.BShifts[s]];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong attacks_from_ROOK(int s)
        {
#if X64
            return Utils.RAttacks[s][(((occupied_squares & Utils.RMasks[s]) * Utils.RMagics[s]) >> Utils.RShifts[s])];
#else
            var lo = (uint)(this.occupied_squares) & (uint)(Utils.RMasks[s]);
            var hi = (uint)(this.occupied_squares >> 32) & (uint)(Utils.RMasks[s] >> 32);
            return
                Utils.RAttacks[s][
                    (lo * (uint)(Utils.RMagics[s]) ^ hi * (uint)(Utils.RMagics[s] >> 32)) >> Utils.RShifts[s]];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong attacks_from_QUEEN(int s)
        {
#if X64
            return Utils.BAttacks[s][(((occupied_squares & Utils.BMasks[s]) * Utils.BMagics[s]) >> Utils.BShifts[s])] | Utils.RAttacks[s][(((occupied_squares & Utils.RMasks[s]) * Utils.RMagics[s]) >> Utils.RShifts[s])];
#else
            var lor = (uint)(this.occupied_squares) & (uint)(Utils.RMasks[s]);
            var hir = (uint)(this.occupied_squares >> 32) & (uint)(Utils.RMasks[s] >> 32);
            var lob = (uint)(this.occupied_squares) & (uint)(Utils.BMasks[s]);
            var hib = (uint)(this.occupied_squares >> 32) & (uint)(Utils.BMasks[s] >> 32);
            return
                Utils.BAttacks[s][
                    (lob * (uint)(Utils.BMagics[s]) ^ hib * (uint)(Utils.BMagics[s] >> 32)) >> Utils.BShifts[s]]
                | Utils.RAttacks[s][
                    (lor * (uint)(Utils.RMagics[s]) ^ hir * (uint)(Utils.RMagics[s] >> 32)) >> Utils.RShifts[s]];
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong attacks_from_KING(int s)
        {
            return Utils.StepAttacksBB_KING[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static ulong attacks_from_KNIGHT(int s)
        {
            return Utils.StepAttacksBB_KNIGHT[s];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong attacks_from_PS(int p, int s)
        {
            return attacks_from(p, s, this.occupied_squares);
        }

        /// attacks_from() computes a bitboard of all attacks of a given piece
        /// put in a given square. Slider attacks use occ bitboard as occupancy.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal static ulong attacks_from(int p, int s, ulong occ)
        {
            var pieceType = Utils.type_of(p);
            if (pieceType == PieceTypeC.BISHOP)
            {
                return Utils.bishop_attacks_bb(s, occ);
            }
            if (pieceType == PieceTypeC.ROOK)
            {
                return Utils.rook_attacks_bb(s, occ);
            }
            if (pieceType == PieceTypeC.QUEEN)
            {
                return Utils.bishop_attacks_bb(s, occ) | Utils.rook_attacks_bb(s, occ);
            }
            return Utils.StepAttacksBB[p][s];
        }

        internal ulong attackers_to(int s)
        {
#if X64
            return ((Utils.StepAttacksBB[((ColorC.BLACK << 3) | PieceTypeC.PAWN)][s]) & (byTypeBB[PieceTypeC.PAWN] & byColorBB[ColorC.WHITE]))
                  | ((Utils.StepAttacksBB[((ColorC.WHITE << 3) | PieceTypeC.PAWN)][s]) & (byTypeBB[PieceTypeC.PAWN] & byColorBB[ColorC.BLACK]))
                  | (Utils.StepAttacksBB[PieceTypeC.KNIGHT][s] & byTypeBB[PieceTypeC.KNIGHT])
                  | ((Utils.RAttacks[s][(((occupied_squares & Utils.RMasks[s]) * Utils.RMagics[s]) >> Utils.RShifts[s])]) & (byTypeBB[PieceTypeC.ROOK] | byTypeBB[PieceTypeC.QUEEN]))
                  | ((Utils.BAttacks[s][(((occupied_squares & Utils.BMasks[s]) * Utils.BMagics[s]) >> Utils.BShifts[s])]) & (byTypeBB[PieceTypeC.BISHOP] | byTypeBB[PieceTypeC.QUEEN]))
                  | (Utils.StepAttacksBB[PieceTypeC.KING][s] & byTypeBB[PieceTypeC.KING]);
#else
            return ((Utils.StepAttacksBB[((ColorC.BLACK << 3) | PieceTypeC.PAWN)][s])
                    & (this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[ColorC.WHITE]))
                   | ((Utils.StepAttacksBB[((ColorC.WHITE << 3) | PieceTypeC.PAWN)][s])
                      & (this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[ColorC.BLACK]))
                   | (Utils.StepAttacksBB[PieceTypeC.KNIGHT][s] & this.byTypeBB[PieceTypeC.KNIGHT])
                   | (Utils.rook_attacks_bb(s, this.occupied_squares)
                      & (this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN]))
                   | (Utils.bishop_attacks_bb(s, this.occupied_squares)
                      & (this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN]))
                   | (Utils.StepAttacksBB[PieceTypeC.KING][s] & this.byTypeBB[PieceTypeC.KING]);
#endif
        }

        /// attackers_to() computes a bitboard of all pieces which attack a
        /// given square. Slider attacks use occ bitboard as occupancy.
        internal ulong attackers_to(int s, ulong occ)
        {
#if X64
            return ((Utils.StepAttacksBB[((ColorC.BLACK << 3) | PieceTypeC.PAWN)][s]) & (byTypeBB[PieceTypeC.PAWN] & byColorBB[ColorC.WHITE]))
                  | ((Utils.StepAttacksBB[((ColorC.WHITE << 3) | PieceTypeC.PAWN)][s]) & (byTypeBB[PieceTypeC.PAWN] & byColorBB[ColorC.BLACK]))
                  | (Utils.StepAttacksBB[PieceTypeC.KNIGHT][s] & byTypeBB[PieceTypeC.KNIGHT])
                  | ((Utils.RAttacks[s][(((occ & Utils.RMasks[s]) * Utils.RMagics[s]) >> Utils.RShifts[s])]) & (byTypeBB[PieceTypeC.ROOK] | byTypeBB[PieceTypeC.QUEEN]))
                  | ((Utils.BAttacks[s][(((occ & Utils.BMasks[s]) * Utils.BMagics[s]) >> Utils.BShifts[s])]) & (byTypeBB[PieceTypeC.BISHOP] | byTypeBB[PieceTypeC.QUEEN]))
                  | (Utils.StepAttacksBB[PieceTypeC.KING][s] & byTypeBB[PieceTypeC.KING]);
#else
            return ((Utils.StepAttacksBB[((ColorC.BLACK << 3) | PieceTypeC.PAWN)][s])
                    & (this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[ColorC.WHITE]))
                   | ((Utils.StepAttacksBB[((ColorC.WHITE << 3) | PieceTypeC.PAWN)][s])
                      & (this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[ColorC.BLACK]))
                   | (Utils.StepAttacksBB[PieceTypeC.KNIGHT][s] & this.byTypeBB[PieceTypeC.KNIGHT])
                   | (Utils.rook_attacks_bb(s, occ) & (this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN]))
                   | (Utils.bishop_attacks_bb(s, occ)
                      & (this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN]))
                   | (Utils.StepAttacksBB[PieceTypeC.KING][s] & this.byTypeBB[PieceTypeC.KING]);
#endif
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool in_check()
        {
            return this.st.checkersBB != 0;
        }

        internal ulong discovered_check_candidates()
        {
            // Pinned pieces protect our king, dicovery checks attack the enemy king
            ulong b, result = 0;
            var ksq = this.pieceList[this.sideToMove ^ 1][PieceTypeC.KING][0];

            // Pinners are sliders, that give check when candidate pinned is removed
            var pinners = this.byColorBB[this.sideToMove]
                          & (((this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN])
                              & Utils.PseudoAttacks_ROOK[ksq])
                             | ((this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN])
                                & Utils.PseudoAttacks_BISHOP[ksq]));

            while (pinners != 0)
            {
#if X64
                Bitboard bb = pinners;
                pinners &= (pinners - 1);
                b = (Utils.BetweenBB[ksq][(Utils.BSFTable[((bb & (0xffffffffffffffff - bb + 1)) * DeBruijn_64) >> 58])]) & occupied_squares;
#else
                b = (Utils.BetweenBB[ksq][Utils.pop_lsb(ref pinners)]) & this.occupied_squares;
#endif
                // Only one bit set and is an our piece?
                if ((b != 0) && ((b & (b - 1)) == 0) && ((b & this.byColorBB[this.sideToMove]) != 0))
                {
                    result |= b;
                }
            }
            return result;
        }

        internal ulong pinned_pieces()
        {
            // Pinned pieces protect our king, dicovery checks attack the enemy king
            ulong b, result = 0;
            var ksq = this.pieceList[this.sideToMove][PieceTypeC.KING][0];

            // Pinners are sliders, that give check when candidate pinned is removed
            var pinners = this.byColorBB[this.sideToMove ^ 1]
                          & (((this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN])
                              & Utils.PseudoAttacks_ROOK[ksq])
                             | ((this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN])
                                & Utils.PseudoAttacks_BISHOP[ksq]));

            while (pinners != 0)
            {
#if X64
                Bitboard bb = pinners;
                pinners &= (pinners - 1);
                b = (Utils.BetweenBB[ksq][(Utils.BSFTable[((bb & (0xffffffffffffffff - bb + 1)) * DeBruijn_64) >> 58])]) & occupied_squares;
#else
                b = (Utils.BetweenBB[ksq][Utils.pop_lsb(ref pinners)]) & this.occupied_squares;
#endif
                // Only one bit set and is an our piece?
                if ((b != 0) && ((b & (b - 1)) == 0) && ((b & this.byColorBB[this.sideToMove]) != 0))
                {
                    result |= b;
                }
            }
            return result;
        }

        /// Position:hidden_checkers
        /// <
        /// >
        /// () returns a bitboard of all pinned (against the
        /// king) pieces for the given color. Or, when template parameter FindPinned is
        /// false, the function return the pieces of the given color candidate for a
        /// discovery check against the enemy king.
        /*Bitboard hidden_checkers_original_unused(bool FindPinned)
        {

            // Pinned pieces protect our king, dicovery checks attack the enemy king
            Bitboard b, result = 0;
            Bitboard pinners = pieces_C(FindPinned ? Utils.flip_C(sideToMove) : sideToMove);
            Square ksq = king_square(FindPinned ? sideToMove : Utils.flip_C(sideToMove));

            // Pinners are sliders, that give check when candidate pinned is removed
            pinners &= (pieces_PTPT(PieceTypeC.ROOK, PieceTypeC.QUEEN) & Utils.PseudoAttacks[PieceTypeC.ROOK][ksq])
                      | (pieces_PTPT(PieceTypeC.BISHOP, PieceTypeC.QUEEN) & Utils.PseudoAttacks[PieceTypeC.BISHOP][ksq]);

            while (pinners != 0)
            {
                b = Utils.squares_between(ksq, Utils.pop_lsb(ref pinners)) & occupied_squares;

                // Only one bit set and is an our piece?
                if ((b != 0) && Utils.single_bit(b) && ((b & pieces_C(sideToMove)) != 0))
                {
                    result |= b;
                }
            }
            return result;
        }*/
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal bool pawn_is_passed(int c, int s)
        {
            return ((this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[c ^ 1]) & Utils.PassedPawnMask[c][s]) == 0;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong key()
        {
            return this.st.key;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong exclusion_key()
        {
            return this.st.key ^ Zobrist.exclusion;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong pawn_key()
        {
            return this.st.pawnKey;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong material_key()
        {
            return this.st.materialKey;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal static int psq_delta(int piece, int from, int to)
        {
            return Zobrist.PieceSquareTable[piece][to] - Zobrist.PieceSquareTable[piece][from];
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int psq_score()
        {
            return this.st.psqScore;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int non_pawn_material(int c)
        {
            return c == 0 ? this.st.npMaterialWHITE : this.st.npMaterialBLACK;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool is_passed_pawn_push(int m)
        {
            return (this.board[((m >> 6) & 0x3F)] & 7) == PieceTypeC.PAWN
                   && (((this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[this.sideToMove ^ 1])
                        & Utils.PassedPawnMask[this.sideToMove][m & 0x3F]) == 0);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int startpos_ply_counter()
        {
            return this.startPosPly + this.st.pliesFromNull; // HACK
        }

        // ALL CALLS INLINED
        //#if AGGR_INLINE
        //        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        //#endif
        //        internal bool opposite_bishops()
        //        {
        //            return pieceCount[ColorC.WHITE][PieceTypeC.BISHOP] == 1
        //                && pieceCount[ColorC.BLACK][PieceTypeC.BISHOP] == 1
        //                && Utils.opposite_colors(pieceList[ColorC.WHITE][PieceTypeC.BISHOP][0], pieceList[ColorC.BLACK][PieceTypeC.BISHOP][0]);
        //        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool bishop_pair(int c)
        {
            // Assumes that there are only two bishops
            return this.pieceCount[c][PieceTypeC.BISHOP] >= 2
                   && Utils.opposite_colors(
                       this.pieceList[c][PieceTypeC.BISHOP][0],
                       this.pieceList[c][PieceTypeC.BISHOP][1]);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool pawn_on_7th(int c)
        {
            return (this.pieces_PTC(PieceTypeC.PAWN, c) & Utils.rank_bb_R(Utils.relative_rank_CR(c, RankC.RANK_7))) != 0;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool is_capture_or_promotion(int m)
        {
            return Utils.type_of_move(m) != MoveTypeC.NORMAL ? Utils.type_of_move(m) != MoveTypeC.CASTLING : (this.board[m & 0x3F] != PieceC.NO_PIECE);
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal bool is_capture(int m)
        {
            // Note that castle is coded as "king captures the rook"
            return ((this.board[m & 0x3F] != PieceC.NO_PIECE) && Utils.type_of_move(m) != MoveTypeC.CASTLING) || Utils.type_of_move(m) == MoveTypeC.ENPASSANT;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal int captured_piece_type()
        {
            return this.st.capturedType;
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal Thread this_thread()
        {
            return this.thisThread;
        }

        /// Position c'tors. Here we always create a copy of the original position
        /// or the FEN string, we want the new born Position object do not depend
        /// on any external data so we detach state pointer from the source one.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal void copy(Position pos)
        {
            // MEMCPY
            Array.Copy(pos.board, this.board, 64);
            Array.Copy(pos.byTypeBB, this.byTypeBB, 8);
            Array.Copy(pos.byColorBB, this.byColorBB, 2);
            this.occupied_squares = pos.occupied_squares;

            for (var i = 0; i < 2; i++)
            {
                Array.Copy(pos.castleRookSquare[i], this.castleRookSquare[i], 2);
                Array.Copy(pos.castlePath[i], this.castlePath[i], 2);
                Array.Copy(pos.pieceCount[i], this.pieceCount[i], 8);
                for (var j = 0; j < 8; j++)
                {
                    Array.Copy(pos.pieceList[i][j], this.pieceList[i][j], 16);
                }
            }

            Array.Copy(pos.index, this.index, 64);
            Array.Copy(pos.castleRightsMask, this.castleRightsMask, 64);

            this.startState = pos.startState;
            this.startPosPly = pos.startPosPly;
            this.sideToMove = pos.sideToMove;

            this.st = pos.st;
            this.chess960 = pos.chess960;
            this.thisThread = pos.thisThread;

            this.startState = this.st;
            this.st = this.startState;
            this.nodes = 0;

            Debug.Assert(this.pos_is_ok());
        }

        internal void copy(Position pos, Thread t)
        {
            this.copy(pos);
            this.thisThread = t;
        }

        public Position()
        {
            for (var i = 0; i < 2; i++)
            {
                this.castleRookSquare[i] = new int[CastlingSideC.CASTLING_SIDE_NB];
                this.castlePath[i] = new ulong[CastlingSideC.CASTLING_SIDE_NB];
                this.pieceCount[i] = new int[PieceTypeC.PIECE_TYPE_NB];
                this.pieceList[i] = new int[PieceTypeC.PIECE_TYPE_NB][];
                for (var j = 0; j < 8; j++)
                {
                    this.pieceList[i][j] = new int[16];
                }
            }
        }

        internal Position(Position pos)
            : this()
        {
            this.copy(pos);
        }

        internal Position(string f, bool c960, Thread t)
            : this()
        {
            this.from_fen(f, c960, t);
        }

        /// Position::from_fen() initializes the position object with the given FEN
        /// string. This function is not very robust - make sure that input FENs are
        /// correct (this is assumed to be the responsibility of the GUI).
        internal void from_fen(string fenStr, bool isChess960, Thread th)
        {
            /*
               A FEN string defines a particular position using only the ASCII character set.

               A FEN string contains six fields separated by a space. The fields are:

               1) Piece placement (from white's perspective). Each rank is described, starting
                  with rank 8 and ending with rank 1; within each rank, the contents of each
                  square are described from file A through file H. Following the Standard
                  Algebraic Notation (SAN), each piece is identified by a single letter taken
                  from the standard English names. White pieces are designated using upper-case
                  letters ("PNBRQK") while Black take lowercase ("pnbrqk"). Blank squares are
                  noted using digits 1 through 8 (the number of blank squares), and "/"
                  separates ranks.

               2) Active color. "w" means white moves next, "b" means black.

               3) Castling availability. If neither side can castle, this is "-". Otherwise,
                  this has one or more letters: "K" (White can castle kingside), "Q" (White
                  can castle queenside), "k" (Black can castle kingside), and/or "q" (Black
                  can castle queenside).

               4) En passant target square (in algebraic notation). If there's no en passant
                  target square, this is "-". If a pawn has just made a 2-square move, this
                  is the position "behind" the pawn. This is recorded regardless of whether
                  there is a pawn in position to make an en passant capture.

               5) Halfmove clock. This is the number of halfmoves since the last pawn advance
                  or capture. This is used to determine if a draw can be claimed under the
                  fifty-move rule.

               6) Fullmove number. The number of the full move. It starts at 1, and is
                  incremented after Black's move.
            */

            char col, row, token;
            int p;
            var sq = SquareC.SQ_A8;

            var fen = fenStr.ToCharArray();
            var fenPos = 0;
            this.clear();

            // 1. Piece placement
            while ((token = fen[fenPos++]) != ' ')
            {
                if (Utils.isdigit(token))
                {
                    sq += (token - '0'); // Advance the given number of files
                }
                else if (token == '/')
                {
                    sq -= 16;
                }
                else
                {
                    p = PieceToChar.IndexOf(token);
                    if (p > -1)
                    {
                        this.put_piece(p, sq);
                        sq++;
                    }
                }
            }

            // 2. Active color
            token = fen[fenPos++];
            this.sideToMove = (token == 'w' ? ColorC.WHITE : ColorC.BLACK);
            token = fen[fenPos++];

            // 3. Castling availability. Compatible with 3 standards: Normal FEN standard,
            // Shredder-FEN that uses the letters of the columns on which the rooks began
            // the game instead of KQkq and also X-FEN standard that, in case of Chess960,
            // if an inner rook is associated with the castling right, the castling tag is
            // replaced by the file letter of the involved rook, as for the Shredder-FEN.
            while ((token = fen[fenPos++]) != ' ')
            {
                int rsq;
                var c = Utils.islower(token) ? ColorC.BLACK : ColorC.WHITE;
                token = Utils.toupper(token);

                if (token == 'K')
                {
                    for (rsq = Utils.relative_square(c, SquareC.SQ_H1);
                         Utils.type_of(this.piece_on(rsq)) != PieceTypeC.ROOK;
                         rsq--)
                    {
                    }
                }
                else if (token == 'Q')
                {
                    for (rsq = Utils.relative_square(c, SquareC.SQ_A1);
                         Utils.type_of(this.piece_on(rsq)) != PieceTypeC.ROOK;
                         rsq++)
                    {
                    }
                }
                else if (token >= 'A' && token <= 'H')
                {
                    
                    rsq = (token - 'A') | Utils.relative_rank_CR(c, RankC.RANK_1);
                }
                else
                {
                    continue;
                }

                this.set_castle_right(c, rsq);
            }

            if (fenPos < fenStr.Length)
            {
                col = fen[fenPos++];
                if (fenPos < fenStr.Length)
                {
                    row = fen[fenPos++];

                    // 4. En passant square. Ignore if no pawn capture is possible
                    if (((col >= 'a' && col <= 'h')) && ((row == '3' || row == '6')))
                    {
                        this.st.epSquare = Utils.make_square((col - 'a'), (row - '1'));

                        if ((this.attackers_to(this.st.epSquare) & this.pieces_PTC(PieceTypeC.PAWN, this.sideToMove))
                            == 0)
                        {
                            this.st.epSquare = SquareC.SQ_NONE;
                        }
                    }
                }
            }

            // 5-6. Halfmove clock and fullmove number
            var tokens = Utils.CreateStack(fenStr.Substring(fenPos));
            if (tokens.Count > 0)
            {
                this.st.rule50 = int.Parse(tokens.Pop());
            }
            if (tokens.Count > 0)
            {
                this.startPosPly = int.Parse(tokens.Pop());
            }

            // Convert from fullmove starting from 1 to ply starting from 0,
            // handle also common incorrect FEN with fullmove = 0.
            this.startPosPly = Math.Max(2 * (this.startPosPly - 1), 0) + ((this.sideToMove == ColorC.BLACK) ? 1 : 0);

            this.st.key = this.compute_key();
            this.st.pawnKey = this.compute_pawn_key();
            this.st.materialKey = this.compute_material_key();
            this.st.psqScore = this.compute_psq_score();
            this.st.npMaterialWHITE = this.compute_non_pawn_material(ColorC.WHITE);
            this.st.npMaterialBLACK = this.compute_non_pawn_material(ColorC.BLACK);
            this.st.checkersBB = this.attackers_to(this.king_square(this.sideToMove))
                                 & this.pieces_C(Utils.flip_C(this.sideToMove));
            this.chess960 = isChess960;
            this.thisThread = th;

            Debug.Assert(this.pos_is_ok());
        }

        /// Position::to_fen() returns a FEN representation of the position. In case
        /// of Chess960 the Shredder-FEN notation is used. Mainly a debugging function.
        internal string to_fen()
        {
            //std::ostringstream fen;
            var fen = new StringBuilder();
            
            for (var rank = RankC.RANK_8; rank >= RankC.RANK_1; rank--)
            {
                for (var file = FileC.FILE_A; file <= FileC.FILE_H; file++)
                {
                    Square sq = Utils.make_square(file, rank);

                    if (this.is_empty(sq))
                    {
                        int emptyCnt = 1;

                        for (; file < FileC.FILE_H && is_empty(sq++); file++)
                        {
                            emptyCnt++;
                        }

                        fen.Append(emptyCnt.ToString());
                    }
                    else
                    {
                        fen.Append(PieceToChar[this.piece_on(sq)]);
                    }
                }

                if (rank > RankC.RANK_1)
                {
                    fen.Append('/');
                }
            }

            fen.Append(this.sideToMove == ColorC.WHITE ? " w " : " b ");

            if (this.can_castle_CR(CastleRightC.WHITE_OO) != 0)
            {
                fen.Append(
                    this.chess960
                        ? Utils.file_to_char(
                                Utils.file_of(this.castle_rook_square(ColorC.WHITE, CastlingSideC.KING_SIDE)), false)
                        : 'K');
            }

            if (this.can_castle_CR(CastleRightC.WHITE_OOO) != 0)
            {
                fen.Append(
                    this.chess960
                        ? Utils.file_to_char(
                                Utils.file_of(this.castle_rook_square(ColorC.WHITE, CastlingSideC.QUEEN_SIDE)), false)
                        : 'Q');
            }

            if (this.can_castle_CR(CastleRightC.BLACK_OO) != 0)
            {
                fen.Append(
                    this.chess960
                        ? Utils.file_to_char(
                            Utils.file_of(this.castle_rook_square(ColorC.BLACK, CastlingSideC.KING_SIDE)))
                        : 'k');
            }

            if (this.can_castle_CR(CastleRightC.BLACK_OOO) != 0)
            {
                fen.Append(
                    this.chess960
                        ? Utils.file_to_char(
                            Utils.file_of(this.castle_rook_square(ColorC.BLACK, CastlingSideC.QUEEN_SIDE)))
                        : 'q');
            }

            if (this.st.castleRights == CastleRightC.CASTLES_NONE)
            {
                fen.Append('-');
            }

            fen.Append(
                this.st.epSquare == SquareC.SQ_NONE ? " - " : " " + Utils.square_to_string(this.st.epSquare) + " ");
            fen.Append(this.st.rule50)
                .Append(" ")
                .Append(1 + (this.startPosPly - (this.sideToMove == ColorC.BLACK ? 1 : 0)) / 2);

            return fen.ToString();
        }

        private const string dottedLine = "\n+---+---+---+---+---+---+---+---+\n";

        /// Position::print() prints an ASCII representation of the position to
        /// the standard output. If a move is given then also the san is printed.
        internal void print(int move)
        {
            if (move != 0)
            {
                var p = new Position(this);
                Plug.Write("\nMove is: ");
                Plug.Write((this.sideToMove == ColorC.BLACK ? ".." : ""));
                Plug.Write(Utils.move_to_san(p, move));
            }

            for (var rank = RankC.RANK_8; rank >= RankC.RANK_1; rank--)
            {
                Plug.Write(dottedLine);
                Plug.Write("|");
                for (var file = FileC.FILE_A; file <= FileC.FILE_H; file++)
                {
                    var sq = Utils.make_square(file, rank);
                    var piece = this.piece_on(sq);
                    var c = (Utils.color_of(piece) == ColorC.BLACK ? '=' : ' ');

                    if (piece == PieceC.NO_PIECE && !Utils.opposite_colors(sq, SquareC.SQ_A1))
                    {
                        piece++; // Index the dot
                    }

                    Plug.Write(c.ToString());
                    Plug.Write(PieceToChar[piece].ToString());
                    Plug.Write(c.ToString());
                    Plug.Write("|");
                }
            }
            Plug.Write(dottedLine);
            Plug.Write("Fen is: ");
            Plug.Write(this.to_fen());
            Plug.Write("\nKey is: ");
            Plug.Write(this.st.key.ToString());
            Plug.Write(Constants.endl);
        }

        /// Position::set_castle_right() is an helper function used to set castling
        /// rights given the corresponding color and the rook starting square.
        internal void set_castle_right(int c, int rfrom)
        {
            var kfrom = this.king_square(c);
            var cs = kfrom < rfrom ? CastlingSideC.KING_SIDE : CastlingSideC.QUEEN_SIDE;
            var cr = Utils.make_castle_right(c, cs);

            this.st.castleRights |= cr;
            this.castleRightsMask[kfrom] |= cr;
            this.castleRightsMask[rfrom] |= cr;
            this.castleRookSquare[c][cs] = rfrom;

            var kto = Utils.relative_square(c, cs == CastlingSideC.KING_SIDE ? SquareC.SQ_G1 : SquareC.SQ_C1);
            var rto = Utils.relative_square(c, cs == CastlingSideC.KING_SIDE ? SquareC.SQ_F1 : SquareC.SQ_D1);

            for (var s = Math.Min(rfrom, rto); s <= Math.Max(rfrom, rto); s++)
            {
                if (s != kfrom && s != rfrom)
                {
                    Utils.set_bit(ref this.castlePath[c][cs], s);
                }
            }

            for (var s = Math.Min(kfrom, kto); s <= Math.Max(kfrom, kto); s++)
            {
                if (s != kfrom && s != rfrom)
                {
                    Utils.set_bit(ref this.castlePath[c][cs], s);
                }
            }
        }

        /// pl_move_is_legal() tests whether a pseudo-legal move is legal
        internal bool pl_move_is_legal(int m, ulong pinned)
        {
            Debug.Assert(Utils.is_ok_M(m));
            Debug.Assert(pinned == this.pinned_pieces());

            var us = this.sideToMove;
            var from = ((m >> 6) & 0x3F);

            Debug.Assert(Utils.color_of(this.piece_moved(m)) == us);
            Debug.Assert(this.piece_on(this.king_square(us)) == Utils.make_piece(us, PieceTypeC.KING));

            // En passant captures are a tricky special case. Because they are rather
            // uncommon, we do it simply by testing whether the king is attacked after
            // the move is made.
            if (Utils.type_of_move(m) == MoveTypeC.ENPASSANT)
            {
                var them = us ^ 1;
                var to = (m & 0x3F);
                var capsq = to + (them == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S);
                var ksq = this.pieceList[us][PieceTypeC.KING][0];
                var b = (this.occupied_squares ^ Utils.SquareBB[from] ^ Utils.SquareBB[capsq]) | Utils.SquareBB[to];

                Debug.Assert(to == this.st.epSquare);
                Debug.Assert(this.piece_moved(m) == Utils.make_piece(us, PieceTypeC.PAWN));
                Debug.Assert(this.piece_on(capsq) == Utils.make_piece(them, PieceTypeC.PAWN));
                Debug.Assert(this.piece_on(to) == PieceC.NO_PIECE);

                return ((Utils.rook_attacks_bb(ksq, b)
                         & ((this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN]) & this.byColorBB[them]))
                        == 0)
                       && ((Utils.bishop_attacks_bb(ksq, b)
                            & ((this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN])
                               & this.byColorBB[them])) == 0);
            }

            // If the moving piece is a king, check whether the destination
            // square is attacked by the opponent. Castling moves are checked
            // for legality during move generation.
            if ((this.board[from] & 7) == PieceTypeC.KING)
            {
                return ((Utils.type_of_move(m) == MoveTypeC.CASTLING) || ((this.attackers_to((m & 0x3F)) & this.byColorBB[us ^ 1]) == 0));
            }

            // A non-king move is legal if and only if it is not pinned or it
            // is moving along the ray towards or away from the king.
            return (pinned == 0) || ((pinned & Utils.SquareBB[from]) == 0)
                   || Utils.squares_aligned(from, (m & 0x3F), this.pieceList[us][PieceTypeC.KING][0]);
        }

        /// move_is_legal() takes a random move and tests whether the move
        /// is legal. This version is not very fast and should be used only in non
        /// time-critical paths.
        internal bool move_is_legal(int m)
        {
            var mlist = MListBroker.GetObject();
            mlist.pos = 0;
            Movegen.generate_legal(this, mlist.moves, ref mlist.pos);
            for (var i = 0; i < mlist.pos; i++)
            {
                if (mlist.moves[i].move == m)
                {
                    MListBroker.Free();
                    return true;
                }
            }
            MListBroker.Free();
            return false;
        }

        /// is_pseudo_legal() takes a random move and tests whether the move
        /// is pseudo legal. It is used to validate moves from TT that can be corrupted
        /// due to SMP concurrent access or hash position key aliasing.
        internal bool is_pseudo_legal(int m)
        {
            var us = this.sideToMove;
            Square from = Utils.from_sq(m);
            Square to = Utils.to_sq(m);
            Piece pc = piece_moved(m);

            // Use a slower but simpler function for uncommon cases
            if (Utils.type_of_move(m) != MoveTypeC.NORMAL)
            {
                return this.move_is_legal(m);
            }

            // Is not a promotion, so promotion piece must be empty
            if (((m >> 12) & 3) != PieceTypeC.NO_PIECE_TYPE)
            {
                return false;
            }

            // If the from square is not occupied by a piece belonging to the side to
            // move, the move is obviously not legal.
            if (pc == PieceC.NO_PIECE || (pc >> 3) != us)
            {
                return false;
            }

            // The destination square cannot be occupied by a friendly piece
            if (piece_on(to) != PieceC.NO_PIECE && Utils.color_of(piece_on(to)) == us)
            {
                return false;
            }

            // Handle the special case of a pawn move
            if ((pc & 7) == PieceTypeC.PAWN)
            {
                // Move direction must be compatible with pawn color
                var direction = to - from;
                if ((us == ColorC.WHITE) != (direction > 0))
                {
                    return false;
                }

                // We have already handled promotion moves, so destination
                // cannot be on the 8/1th rank.
                if ((to >> 3) == RankC.RANK_8 || (to >> 3) == RankC.RANK_1)
                {
                    return false;
                }

                // Proceed according to the square delta between the origin and
                // destination squares.
                switch (direction)
                {
                    case SquareC.DELTA_NW:
                    case SquareC.DELTA_NE:
                    case SquareC.DELTA_SW:
                    case SquareC.DELTA_SE:
                        // Capture. The destination square must be occupied by an enemy
                        // piece (en passant captures was handled earlier).
                        if (piece_on(to) == PieceC.NO_PIECE || Utils.color_of(piece_on(to)) == us)
                        {
                            return false;
                        }

                        // From and to files must be one file apart, avoids a7h5
                        if (Math.Abs(Utils.file_of(from) - Utils.file_of(to)) != 1)
                        {
                            return false;
                        }
                        break;

                    case SquareC.DELTA_N:
                    case SquareC.DELTA_S:
                        // Pawn push. The destination square must be empty.
                        if (this.board[to] != PieceC.NO_PIECE)
                        {
                            return false;
                        }
                        break;

                    case SquareC.DELTA_NN:
                        // Double white pawn push. The destination square must be on the fourth
                        // rank, and both the destination square and the square between the
                        // source and destination squares must be empty.
                        if ((to >> 3) != RankC.RANK_4 || (this.board[to] != PieceC.NO_PIECE)
                            || (this.board[from + SquareC.DELTA_N] != PieceC.NO_PIECE))
                        {
                            return false;
                        }
                        break;

                    case SquareC.DELTA_SS:
                        // Double black pawn push. The destination square must be on the fifth
                        // rank, and both the destination square and the square between the
                        // source and destination squares must be empty.
                        if ((to >> 3) != RankC.RANK_5 || (this.board[to] != PieceC.NO_PIECE)
                            || (this.board[from + SquareC.DELTA_S] != PieceC.NO_PIECE))
                        {
                            return false;
                        }
                        break;

                    default:
                        return false;
                }
            }
            else if ((attacks_from(pc, from, this.occupied_squares) & Utils.SquareBB[to]) == 0)
            {
                return false;
            }

            // Evasions generator already takes care to avoid some kind of illegal moves
            // and pl_move_is_legal() relies on this. So we have to take care that the
            // same kind of moves are filtered out here.
            if (this.st.checkersBB != 0)
            {
                if (Utils.type_of(pc) != PieceTypeC.KING)
                {
                    var b = this.st.checkersBB;
                    var checksq = Utils.pop_lsb(ref b);

                    if (b != 0) // double check ? In this case a king move is required
                    {
                        return false;
                    }
                    
                    // Our move must be a blocking evasion or a capture of the checking piece
                    if (((Utils.BetweenBB[checksq][this.pieceList[us][PieceTypeC.KING][0]] | this.st.checkersBB)
                         & Utils.SquareBB[to]) == 0)
                    {
                        return false;
                    }
                }
                // In case of king moves under check we have to remove king so to catch
                // as invalid moves like b1a1 when opposite queen is on c1.
                else if ((this.attackers_to(to, (this.occupied_squares ^ Utils.SquareBB[from])) & this.byColorBB[us ^ 1])
                         != 0)
                {
                    return false;
                }
            }

            return true;
        }

        /// move_gives_check() tests whether a pseudo-legal move gives a check
        internal bool move_gives_check(int m, CheckInfo ci)
        {
            Debug.Assert(Utils.is_ok_M(m));
            Debug.Assert(ci.dcCandidates == this.discovered_check_candidates());
            Debug.Assert(Utils.color_of(this.piece_moved(m)) == this.sideToMove);

            var from = (m >> 6) & 0x3F;
            var to = m & 0x3F;
            var pt = this.board[from] & 7;

            // Direct check ?
            if ((ci.checkSq[pt] & Utils.SquareBB[to]) != 0)
            {
                return true;
            }

            // Discovery check ?
            if ((ci.dcCandidates != 0) && ((ci.dcCandidates & Utils.SquareBB[from]) != 0))
            {
                // For pawn and king moves we need to verify also direction
                if ((pt != PieceTypeC.PAWN && pt != PieceTypeC.KING)
                    || (((Utils.BetweenBB[from][to]
                          | Utils.BetweenBB[from][this.pieceList[this.sideToMove ^ 1][PieceTypeC.KING][0]]
                          | Utils.BetweenBB[to][this.pieceList[this.sideToMove ^ 1][PieceTypeC.KING][0]])
                         & (Utils.SquareBB[from] | Utils.SquareBB[to]
                            | Utils.SquareBB[this.pieceList[this.sideToMove ^ 1][PieceTypeC.KING][0]])) == 0))
                {
                    return true;
                }
            }

            // Can we skip the ugly special cases ?
            if (Utils.type_of_move(m) == MoveTypeC.NORMAL)
            {
                return false;
            }

            var us = this.sideToMove;
            var ksq = this.pieceList[this.sideToMove ^ 1][PieceTypeC.KING][0];

            switch (Utils.type_of_move(m))
            {
                case MoveTypeC.PROMOTION:
                    return (attacks_from((((m >> 12) & 3) + 2), to, this.occupied_squares ^ Utils.SquareBB[from]) & Utils.SquareBB[ksq]) != 0;
                
                    // En passant capture with check ? We have already handled the case
                // of direct checks and ordinary discovered check, the only case we
                // need to handle is the unusual case of a discovered check through
                // the captured pawn.
                case MoveTypeC.ENPASSANT:
                    {
                        var capsq = (((from >> 3) << 3) | (to & 7));
                        var b = (this.occupied_squares ^ Utils.SquareBB[from] ^ Utils.SquareBB[capsq]) | Utils.SquareBB[to];
                        return ((Utils.rook_attacks_bb(ksq, b)
                                 & ((this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN]) & this.byColorBB[us]))
                                != 0)
                               || ((Utils.bishop_attacks_bb(ksq, b)
                                    & ((this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN]) & this.byColorBB[us]))
                                   != 0);
                    }
                case MoveTypeC.CASTLING:
                    if (Utils.type_of_move(m) == MoveTypeC.CASTLING)
                    {
                        var kfrom = from;
                        var rfrom = to; // 'King captures the rook' notation
                        var kto = ((rfrom > kfrom ? SquareC.SQ_G1 : SquareC.SQ_C1) ^ (us * 56));
                        var rto = ((rfrom > kfrom ? SquareC.SQ_F1 : SquareC.SQ_D1) ^ (us * 56));
                        var b = (this.occupied_squares ^ Utils.SquareBB[kfrom] ^ Utils.SquareBB[rfrom]) | Utils.SquareBB[rto]
                                | Utils.SquareBB[kto];
                        return (Utils.rook_attacks_bb(rto, b) & Utils.SquareBB[ksq]) != 0;
                    }

                    Debug.Assert(false);
                    return false;
                default:
                    Debug.Assert(false);
                    return false;
            }
        }

        /// do_move() makes a move, and saves all information necessary
        /// to a StateInfo object. The move is assumed to be legal. Pseudo-legal
        /// moves should be filtered out before this function is called.
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif
        internal void do_move(int m, StateInfo newSt)
        {
            var ci = CheckInfoBroker.GetObject();
            ci.CreateCheckInfo(this);
            this.do_move(m, newSt, ci, this.move_gives_check(m, ci));
            CheckInfoBroker.Free();
        }

        internal void do_move(int m, StateInfo newSt, CheckInfo ci, bool moveIsCheck)
        {
            Debug.Assert(Utils.is_ok_M(m));
            Debug.Assert(newSt != this.st);

            this.nodes++;
            var k = this.st.key;

            newSt.pawnKey = this.st.pawnKey;
            newSt.materialKey = this.st.materialKey;
            newSt.npMaterialWHITE = this.st.npMaterialWHITE;
            newSt.npMaterialBLACK = this.st.npMaterialBLACK;
            newSt.castleRights = this.st.castleRights;
            newSt.rule50 = this.st.rule50;
            newSt.pliesFromNull = this.st.pliesFromNull;
            newSt.psqScore = this.st.psqScore;
            newSt.epSquare = this.st.epSquare;

            newSt.previous = this.st;
            this.st = newSt;

            // Update side to move
            k ^= Zobrist.side;

            // Increment the 50 moves rule draw counter. Resetting it to zero in the
            // case of a capture or a pawn move is taken care of later.
            this.st.rule50++;
            this.st.pliesFromNull++;

            var us = this.sideToMove;
            var them = us ^ 1;
            Square from = Utils.from_sq(m);
            Square to = Utils.to_sq(m);
            Piece piece = piece_on(from);
            PieceType pt = Utils.type_of(piece);
            PieceType capture = Utils.type_of_move(m) == MoveTypeC.ENPASSANT ? PieceTypeC.PAWN : Utils.type_of(piece_on(to));

            Debug.Assert(Utils.color_of(piece) == us);
            Debug.Assert(piece_on(to) == PieceC.NO_PIECE || Utils.color_of(piece_on(to)) == them || Utils.type_of_move(m) == MoveTypeC.CASTLING);
            Debug.Assert(capture != PieceTypeC.KING);

            if (Utils.type_of_move(m) == MoveTypeC.CASTLING)
            {
                Debug.Assert(piece == Utils.make_piece(us, PieceTypeC.KING));
                
                bool kingSide = to > from;
                Square rfrom = to; // Castle is encoded as "king captures friendly rook"
                Square rto = Utils.relative_square(us, kingSide ? SquareC.SQ_F1 : SquareC.SQ_D1);
                to = Utils.relative_square(us, kingSide ? SquareC.SQ_G1 : SquareC.SQ_C1);
                capture = PieceTypeC.NO_PIECE_TYPE;
                
                do_castle(from, to, rfrom, rto);
                
                st.psqScore += psq_delta(Utils.make_piece(us, PieceTypeC.ROOK), rfrom, rto);
                k ^= Zobrist.psq[us][PieceTypeC.ROOK][rfrom] ^ Zobrist.psq[us][PieceTypeC.ROOK][rto];
            }

            if (capture != 0)
            {
                var capsq = to;

                // If the captured piece is a pawn, update pawn hash key, otherwise
                // update non-pawn material.
                if (capture == PieceTypeC.PAWN)
                {
                    if (Utils.type_of_move(m) == MoveTypeC.ENPASSANT)
                    {
                        capsq += (them == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S);

                        Debug.Assert(pt == PieceTypeC.PAWN);
                        Debug.Assert(to == this.st.epSquare);
                        Debug.Assert(Utils.relative_rank_CS(us, to) == RankC.RANK_6);
                        Debug.Assert(this.piece_on(to) == PieceC.NO_PIECE);
                        Debug.Assert(this.piece_on(capsq) == Utils.make_piece(them, PieceTypeC.PAWN));

                        this.board[capsq] = PieceC.NO_PIECE;
                    }

                    this.st.pawnKey ^= Zobrist.psq[them][PieceTypeC.PAWN][capsq];
                }
                else
                {
                    if (them == 0)
                    {
                        this.st.npMaterialWHITE -= PieceValue[PhaseC.MG][capture];
                    }
                    else
                    {
                        this.st.npMaterialBLACK -= PieceValue[PhaseC.MG][capture];
                    }
                }

                // Remove the captured piece
                var capPieceMask = Utils.SquareBB[capsq];
                this.occupied_squares ^= capPieceMask;
                this.byTypeBB[capture] ^= capPieceMask;
                this.byColorBB[them] ^= capPieceMask;

                // Update piece list, move the last piece at index[capsq] position and
                // shrink the list.
                //
                // WARNING: This is a not revresible operation. When we will reinsert the
                // captured piece in undo_move() we will put it at the end of the list and
                // not in its original place, it means index[] and pieceList[] are not
                // guaranteed to be invariant to a do_move() + undo_move() sequence.
                var plThemCapture = this.pieceList[them][capture];
                var pcThemCapture = --this.pieceCount[them][capture];
                var lastSquare = plThemCapture[pcThemCapture];
                this.index[lastSquare] = this.index[capsq];
                plThemCapture[this.index[lastSquare]] = lastSquare;
                plThemCapture[pcThemCapture] = SquareC.SQ_NONE;

                // Update hash keys
                k ^= Zobrist.psq[them][capture][capsq];
                this.st.materialKey ^= Zobrist.psq[them][capture][pcThemCapture];

                // Update incremental scores
                this.st.psqScore -= Zobrist.PieceSquareTable[((them << 3) | capture)][capsq];

                // Reset rule 50 counter
                this.st.rule50 = 0;
            }

            // Update hash key
            k ^= Zobrist.psq[us][pt][from] ^ Zobrist.psq[us][pt][to];

            // Reset en passant square
            if (this.st.epSquare != SquareC.SQ_NONE)
            {
                k ^= Zobrist.enpassant[this.st.epSquare & 7];
                this.st.epSquare = SquareC.SQ_NONE;
            }

            // Update castle rights if needed
            if ((this.st.castleRights != 0) && ((this.castleRightsMask[from] | this.castleRightsMask[to]) != 0))
            {
                var cr = this.castleRightsMask[from] | this.castleRightsMask[to];
                k ^= Zobrist.castle[this.st.castleRights & cr];
                this.st.castleRights &= ~cr;
            }

            // Move the piece. The tricky Chess960 castle is handled earlier
            if (Utils.type_of_move(m) != MoveTypeC.CASTLING)
            {
                Bitboard from_to_bb = Utils.SquareBB[from] ^ Utils.SquareBB[to];
                occupied_squares ^= from_to_bb;
                byTypeBB[pt] ^= from_to_bb;
                byColorBB[us] ^= from_to_bb;
                
                board[from] = PieceC.NO_PIECE;
                board[to] = piece;
                
                      // Update piece lists, index[from] is not updated and becomes stale. This
                      // works as long as index[] is accessed just by known occupied squares.
                index[to] = index[from];
                pieceList[us][pt][index[to]] = to;
            }

            // If the moving piece is a pawn do some special extra work
            if (pt == PieceTypeC.PAWN)
            {
                // Set en-passant square, only if moved pawn can be captured
                if ((to ^ from) == 16
                    && ((((Utils.StepAttacksBB[((us << 3) | PieceTypeC.PAWN)][
                        from + (us == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S)])
                          & (this.byTypeBB[PieceTypeC.PAWN] & this.byColorBB[them]))) != 0))
                {
                    this.st.epSquare = ((from + to) / 2);
                    k ^= Zobrist.enpassant[this.st.epSquare & 7];
                }

                if (Utils.type_of_move(m) == MoveTypeC.PROMOTION)
                {
                    var promotion = (((m >> 12) & 3) + 2);

                    Debug.Assert(Utils.relative_rank_CS(us, to) == RankC.RANK_8);
                    Debug.Assert(promotion >= PieceTypeC.KNIGHT && promotion <= PieceTypeC.QUEEN);

                    // Replace the pawn with the promoted piece
                    this.byTypeBB[PieceTypeC.PAWN] ^= Utils.SquareBB[to];
                    this.byTypeBB[promotion] |= Utils.SquareBB[to];
                    this.board[to] = ((us << 3) | promotion);

                    // Update piece lists, move the last pawn at index[to] position
                    // and shrink the list. Add a new promotion piece to the list.
                    var plUsPawn = this.pieceList[us][PieceTypeC.PAWN];
                    var lastSquare = plUsPawn[--this.pieceCount[us][PieceTypeC.PAWN]];
                    this.index[lastSquare] = this.index[to];
                    plUsPawn[this.index[lastSquare]] = lastSquare;
                    plUsPawn[this.pieceCount[us][PieceTypeC.PAWN]] = SquareC.SQ_NONE;
                    this.index[to] = this.pieceCount[us][promotion];
                    this.pieceList[us][promotion][this.index[to]] = to;

                    // Update hash keys
                    k ^= Zobrist.psq[us][PieceTypeC.PAWN][to] ^ Zobrist.psq[us][promotion][to];
                    this.st.pawnKey ^= Zobrist.psq[us][PieceTypeC.PAWN][to];
                    this.st.materialKey ^= Zobrist.psq[us][promotion][this.pieceCount[us][promotion]++]
                                           ^ Zobrist.psq[us][PieceTypeC.PAWN][this.pieceCount[us][PieceTypeC.PAWN]];

                    // Update incremental score
                    this.st.psqScore += Zobrist.PieceSquareTable[((us << 3) | promotion)][to]
                                        - Zobrist.PieceSquareTable[((us << 3) | PieceTypeC.PAWN)][to];

                    // Update material
                    if (us == 0)
                    {
                        this.st.npMaterialWHITE += PieceValue[PhaseC.MG][promotion];
                    }
                    else
                    {
                        this.st.npMaterialBLACK += PieceValue[PhaseC.MG][promotion];
                    }
                }

                // Update pawn hash key
                this.st.pawnKey ^= Zobrist.psq[us][PieceTypeC.PAWN][from] ^ Zobrist.psq[us][PieceTypeC.PAWN][to];

                // Reset rule 50 draw counter
                this.st.rule50 = 0;
            }

            // Update incremental scores
            this.st.psqScore += (Zobrist.PieceSquareTable[piece][to] - Zobrist.PieceSquareTable[piece][from]);

            // Set capture piece
            this.st.capturedType = capture;

            // Update the key with the final value
            this.st.key = k;

            // Update checkers bitboard, piece must be already moved
            this.st.checkersBB = 0;

            if (moveIsCheck)
            {
                if (Utils.type_of_move(m) != MoveTypeC.NORMAL)
                {
                    this.st.checkersBB = this.attackers_to(this.pieceList[them][PieceTypeC.KING][0])
                                         & this.byColorBB[us];
                }
                else
                {
                    // Direct checks
                    if ((ci.checkSq[pt] & Utils.SquareBB[to]) != 0)
                    {
                        this.st.checkersBB |= Utils.SquareBB[to];
                    }

                    // Discovery checks
                    if ((ci.dcCandidates != 0) && ((ci.dcCandidates & Utils.SquareBB[from]) != 0))
                    {
                        if (pt != PieceTypeC.ROOK)
                        {
                            this.st.checkersBB |= this.attacks_from_ROOK(this.pieceList[them][PieceTypeC.KING][0])
                                                  & ((this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN])
                                                     & this.byColorBB[us]);
                        }

                        if (pt != PieceTypeC.BISHOP)
                        {
                            this.st.checkersBB |= this.attacks_from_BISHOP(this.pieceList[them][PieceTypeC.KING][0])
                                                  & ((this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN])
                                                     & this.byColorBB[us]);
                        }
                    }
                }
            }

            // Finish
            this.sideToMove = this.sideToMove ^ 1;

            Debug.Assert(this.pos_is_ok());
        }

        /// undo_move() unmakes a move. When it returns, the position should
        /// be restored to exactly the same state as before the move was made.
        internal void undo_move(int m)
        {
            Debug.Assert(Utils.is_ok_M(m));

            this.sideToMove = this.sideToMove ^ 1;

            var us = this.sideToMove;
            var them = us ^ 1;
            var from = ((m >> 6) & 0x3F);
            var to = (m & 0x3F);
            var pt = Utils.type_of(piece_on(to));
            var capture = this.st.capturedType;

            Debug.Assert(this.is_empty(from) || Utils.type_of_move(m) == MoveTypeC.CASTLING);
            Debug.Assert(capture != PieceTypeC.KING);

            if (Utils.type_of_move(m) == MoveTypeC.PROMOTION)
            {
                var promotion = (((m >> 12) & 3) + 2);

                Debug.Assert(promotion == pt);
                Debug.Assert(Utils.relative_rank_CS(us, to) == RankC.RANK_8);
                Debug.Assert(promotion >= PieceTypeC.KNIGHT && promotion <= PieceTypeC.QUEEN);

                // Replace the promoted piece with the pawn
                this.byTypeBB[promotion] ^= Utils.SquareBB[to];
                this.byTypeBB[PieceTypeC.PAWN] |= Utils.SquareBB[to];
                this.board[to] = ((us << 3) | PieceTypeC.PAWN);

                // Update piece lists, move the last promoted piece at index[to] position
                // and shrink the list. Add a new pawn to the list.
                var lastSquare = this.pieceList[us][promotion][--this.pieceCount[us][promotion]];
                this.index[lastSquare] = this.index[to];
                var plUsPromotion = this.pieceList[us][promotion];
                plUsPromotion[this.index[lastSquare]] = lastSquare;
                plUsPromotion[this.pieceCount[us][promotion]] = SquareC.SQ_NONE;
                this.index[to] = this.pieceCount[us][PieceTypeC.PAWN]++;
                this.pieceList[us][PieceTypeC.PAWN][this.index[to]] = to;

                pt = PieceTypeC.PAWN;
            }

            if (Utils.type_of_move(m) == MoveTypeC.CASTLING)
            {
                bool kingSide = to > from;
                Square rfrom = to; // Castle is encoded as "king captures friendly rook"
                Square rto = Utils.relative_square(us, kingSide ? SquareC.SQ_F1 : SquareC.SQ_D1);
                to = Utils.relative_square(us, kingSide ? SquareC.SQ_G1 : SquareC.SQ_C1);
                capture = PieceTypeC.NO_PIECE_TYPE;
                pt = PieceTypeC.KING;
                do_castle(to, from, rto, rfrom);
            }
            else
            {
                // Put the piece back at the source square
                Bitboard from_to_bb = Utils.SquareBB[from] ^ Utils.SquareBB[to];
                occupied_squares ^= from_to_bb;
                byTypeBB[pt] ^= from_to_bb;
                byColorBB[us] ^= from_to_bb;
                
                board[to] = PieceC.NO_PIECE;
                board[from] = Utils.make_piece(us, pt);
                
                // Update piece lists, index[to] is not updated and becomes stale. This
                // works as long as index[] is accessed just by known occupied squares.
                index[from] = index[to];
                pieceList[us][pt][index[from]] = from;
            }

            if (capture != 0)
            {
                var capsq = to;

                if (Utils.type_of_move(m) == MoveTypeC.ENPASSANT)
                {
                    capsq -= (us == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S);

                    Debug.Assert(pt == PieceTypeC.PAWN);
                    Debug.Assert(to == this.st.previous.epSquare);
                    Debug.Assert(Utils.relative_rank_CS(us, to) == RankC.RANK_6);
                    Debug.Assert(this.piece_on(capsq) == PieceC.NO_PIECE);
                }

                // Restore the captured piece
                var capSqMask = Utils.SquareBB[capsq];
                this.occupied_squares |= capSqMask;
                this.byTypeBB[capture] |= capSqMask;
                this.byColorBB[them] |= capSqMask;
                this.board[capsq] = ((them << 3) | capture);

                // Update piece list, add a new captured piece in capsq square
                this.index[capsq] = this.pieceCount[them][capture]++;
                this.pieceList[them][capture][this.index[capsq]] = capsq;
            }

            // Finally point our state pointer back to the previous state
            this.st = this.st.previous;

            Debug.Assert(this.pos_is_ok());
        }

        // Position::do_castle() is a helper used to do/undo a castling move. This
        // is a bit tricky, especially in Chess960.
        internal void do_castle(Square kfrom, Square kto, Square rfrom, Square rto)
        {
            var us = this.sideToMove;
            
            var k_from_to_bb = Utils.SquareBB[kfrom] ^ Utils.SquareBB[kto];
            var r_from_to_bb = Utils.SquareBB[rfrom] ^ Utils.SquareBB[rto];
            this.byTypeBB[PieceTypeC.KING] ^= k_from_to_bb;
            this.byTypeBB[PieceTypeC.ROOK] ^= r_from_to_bb;
            this.occupied_squares ^= k_from_to_bb ^ r_from_to_bb;
            this.byColorBB[us] ^= k_from_to_bb ^ r_from_to_bb;

            // Could be from == to, so first set NO_PIECE then KING and ROOK
            this.board[kfrom] = this.board[rfrom] = PieceC.NO_PIECE;
            this.board[kto] = Utils.make_piece(us, PieceTypeC.KING);
            this.board[rto] = Utils.make_piece(us, PieceTypeC.ROOK);

            // Could be kfrom == rto, so use a 'tmp' variable
            int tmp = index[kfrom];
            index[rto] = index[rfrom];
            index[kto] = tmp;
            pieceList[us][PieceTypeC.KING][index[kto]] = kto;
            pieceList[us][PieceTypeC.ROOK][index[rto]] = rto;
        }

        internal void undo_null_move(StateInfo backupSt)
        {
            Debug.Assert(!this.in_check());
            
            st = st.previous;
            this.sideToMove = Utils.flip_C(this.sideToMove);
        }

        // Position::do(undo)_null_move() is used to do(undo) a "null move": It flips
        // the side to move without executing any move on the board.
        internal void do_null_move(StateInfo newSt)
        {
            Debug.Assert(!this.in_check());

            newSt.capturedType = st.capturedType;
            newSt.castleRights = st.castleRights;
            newSt.rule50 = st.rule50;
            newSt.pliesFromNull = st.pliesFromNull;
            newSt.checkersBB = st.checkersBB;
            newSt.epSquare = st.epSquare;
            newSt.key = st.key;
            newSt.npMaterialWHITE = st.npMaterialWHITE;
            newSt.npMaterialBLACK = st.npMaterialBLACK;
            newSt.pawnKey = st.pawnKey;
            newSt.materialKey = st.materialKey;
            newSt.psqScore = st.psqScore;
            
            newSt.previous = st;
            st = newSt;

            if (this.st.epSquare != SquareC.SQ_NONE)
            {
                this.st.key ^= Zobrist.enpassant[Utils.file_of(this.st.epSquare)];
                this.st.epSquare = SquareC.SQ_NONE;
            }

            st.key ^= Zobrist.side;
            st.rule50++;
            st.pliesFromNull = 0;

            this.sideToMove = Utils.flip_C(this.sideToMove);

            Debug.Assert(this.pos_is_ok());
        }

        /// see() is a static exchange evaluator: It tries to estimate the
        /// material gain or loss resulting from a move. There are three versions of
        /// this function: One which takes a destination square as input, one takes a
        /// move, and one which takes a 'from' and a 'to' square. The function does
        /// not yet understand promotions captures.
        internal int see(int m, bool with_sign)
        {
            if ((with_sign)
                && (PieceValue[PhaseC.MG][this.board[m & 0x3F]] >= PieceValue[PhaseC.MG][this.board[((m >> 6) & 0x3F)]]))
            {
                return 1;
            }

            int from, to;
            ulong occupied, attackers, stmAttackers;

            var slIndex = 1;
            int captured;
            int stm;

            Debug.Assert(Utils.is_ok_M(m));

            from = Utils.from_sq(m);
            to = Utils.to_sq(m);
            captured = this.board[to] & 7;
            occupied = this.occupied_squares;

            // Handle en passant moves
            if (Utils.type_of_move(m) == MoveTypeC.ENPASSANT)
            {
                var capQq = to - (this.sideToMove == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S);

                Debug.Assert(captured == 0);
                Debug.Assert(Utils.type_of(this.piece_on(capQq)) == PieceTypeC.PAWN);

                // Remove the captured pawn
                occupied ^= Utils.SquareBB[capQq];
                captured = PieceTypeC.PAWN;
            }
            else if (Utils.type_of_move(m) == MoveTypeC.CASTLING)
            {
                // Castle moves are implemented as king capturing the rook so cannot be
                // handled correctly. Simply return 0 that is always the correct value
                // unless the rook is ends up under attack.
                return 0;
            }

            // Find all attackers to the destination square, with the moving piece
            // removed, but possibly an X-ray attacker added behind it.
            attackers = this.attackers_to(to, occupied);

            // If the opponent has no attackers we are finished
            stm = ((this.board[from] >> 3) ^ 1);
            stmAttackers = attackers & this.byColorBB[stm];
            if (stmAttackers == 0)
            {
                return PieceValue[PhaseC.MG][captured];
            }

            // The destination square is defended, which makes things rather more
            // difficult to compute. We proceed by building up a "swap list" containing
            // the material gain or loss at each stop in a sequence of captures to the
            // destination square, where the sides alternately capture, and always
            // capture with the least valuable piece. After each capture, we look for
            // new X-ray attacks from behind the capturing piece.
            var swap = SwapListBroker.GetObject();
            var swapList = swap.list;

            swapList[0] = PieceValue[PhaseC.MG][captured];
            captured = this.board[from] & 7;

            do
            {
                Debug.Assert(slIndex < 32);
                
                // Add the new entry to the swap list
                swapList[slIndex] = -swapList[slIndex - 1] + PieceValue[PhaseC.MG][captured];
                slIndex++;

                // Locate and remove from 'occupied' the next least valuable attacker
                captured = this.next_attacker(PieceTypeC.PAWN, to, stmAttackers, ref occupied, ref attackers);

                attackers &= occupied; // Remove the just found attacker

                stm = stm ^ 1;
                stmAttackers = attackers & this.byColorBB[stm];

                // Stop before processing a king capture
                if (captured == PieceTypeC.KING && (stmAttackers != 0))
                {
                    Debug.Assert(slIndex < 32);
                    swapList[slIndex++] = Constants.QueenValueMidgame * 10;
                    break;
                }
            }
            while (stmAttackers != 0);

            // Having built the swap list, we negamax through it to find the best
            // achievable score from the point of view of the side to move.
            while ((--slIndex) != 0)
            {
                swapList[slIndex - 1] = Math.Min(-swapList[slIndex], swapList[slIndex - 1]);
            }

            var retval = swapList[0];
            SwapListBroker.Free();

            return retval;
        }

        internal PieceType next_attacker(PieceType Pt, Square to, ulong stmAttackers, ref ulong occupied, ref ulong attackers)
        {
            if (Pt == PieceTypeC.KING)
            {
                return PieceTypeC.KING;
            }

            if ((stmAttackers & this.byTypeBB[Pt]) != 0)
            {
                Bitboard b = stmAttackers & this.byTypeBB[Pt];
                occupied ^= b & ~(b - 1);

                if (Pt == PieceTypeC.PAWN || Pt == PieceTypeC.BISHOP || Pt == PieceTypeC.QUEEN)
                    attackers |= Utils.bishop_attacks_bb(to, occupied) & (this.byTypeBB[PieceTypeC.BISHOP] | this.byTypeBB[PieceTypeC.QUEEN]);

                if (Pt == PieceTypeC.ROOK || Pt == PieceTypeC.QUEEN)
                    attackers |= Utils.rook_attacks_bb(to, occupied) & (this.byTypeBB[PieceTypeC.ROOK] | this.byTypeBB[PieceTypeC.QUEEN]);

                return Pt;
            }

            return this.next_attacker(Pt + 1, to, stmAttackers, ref occupied, ref attackers);
        }

        /// clear() erases the position object to a pristine state, with an
        /// empty board, white to move, and no castling rights.
        internal void clear()
        {
            Array.Clear(this.byColorBB, 0, 2);
            Array.Clear(this.byTypeBB, 0, 8);
            Array.Clear(this.pieceCount[0], 0, 8);
            Array.Clear(this.pieceCount[1], 0, 8);
            Array.Clear(this.index, 0, 64);
            Array.Clear(this.castleRightsMask, 0, 64);
            Array.Clear(this.castleRookSquare[0], 0, 2);
            Array.Clear(this.castleRookSquare[1], 0, 2);
            Array.Clear(this.castlePath[0], 0, 2);
            Array.Clear(this.castlePath[1], 0, 2);

            this.startState.Clear();
            this.occupied_squares = 0;
            this.nodes = 0;
            this.startPosPly = 0;
            this.sideToMove = 0;
            this.thisThread = null;
            this.chess960 = false;

            this.startState.epSquare = SquareC.SQ_NONE;
            this.st = this.startState;

            for (var i = 0; i < 8; i++)
            {
                for (var j = 0; j < 16; j++)
                {
                    this.pieceList[0][i][j] = this.pieceList[1][i][j] = SquareC.SQ_NONE;
                }
            }

            for (var sq = SquareC.SQ_A1; sq <= SquareC.SQ_H8; sq++)
            {
                this.board[sq] = PieceC.NO_PIECE;
            }
        }

        /// put_piece() puts a piece on the given square of the board,
        /// updating the board array, pieces list, bitboards, and piece counts.
        internal void put_piece(int p, int s)
        {
            var c = Utils.color_of(p);
            var pt = Utils.type_of(p);

            this.board[s] = p;
            this.index[s] = this.pieceCount[c][pt]++;
            this.pieceList[c][pt][this.index[s]] = s;

            Utils.set_bit(ref this.occupied_squares, s);
            Utils.set_bit(ref this.byTypeBB[pt], s);
            Utils.set_bit(ref this.byColorBB[c], s);
        }

        /// compute_key() computes the hash key of the position. The hash
        /// key is usually updated incrementally as moves are made and unmade, the
        /// compute_key() function is only used when a new position is set up, and
        /// to verify the correctness of the hash key when running in debug mode.
        internal ulong compute_key()
        {
            var k = Zobrist.castle[this.st.castleRights];

            for (var b = this.occupied_squares; b != 0;)
            {
                var s = Utils.pop_lsb(ref b);
                k ^= Zobrist.psq[Utils.color_of(this.piece_on(s))][Utils.type_of(this.piece_on(s))][s];
            }

            if (this.st.epSquare != SquareC.SQ_NONE)
            {
                k ^= Zobrist.enpassant[Utils.file_of(this.st.epSquare)];
            }

            if (this.sideToMove == ColorC.BLACK)
            {
                k ^= Zobrist.side;
            }

            return k;
        }

        /// compute_pawn_key() computes the hash key of the position. The
        /// hash key is usually updated incrementally as moves are made and unmade,
        /// the compute_pawn_key() function is only used when a new position is set
        /// up, and to verify the correctness of the pawn hash key when running in
        /// debug mode.
        internal ulong compute_pawn_key()
        {
            ulong k = 0;

            for (var b = this.pieces_PT(PieceTypeC.PAWN); b != 0;)
            {
                var s = Utils.pop_lsb(ref b);
                k ^= Zobrist.psq[Utils.color_of(this.piece_on(s))][PieceTypeC.PAWN][s];
            }

            return k;
        }

        /// compute_material_key() computes the hash key of the position.
        /// The hash key is usually updated incrementally as moves are made and unmade,
        /// the compute_material_key() function is only used when a new position is set
        /// up, and to verify the correctness of the material hash key when running in
        /// debug mode.
        internal ulong compute_material_key()
        {
            ulong k = 0;

            for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
            {
                for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.QUEEN; pt++)
                {
                    for (var cnt = 0; cnt < this.piece_count(c, pt); cnt++)
                    {
                        k ^= Zobrist.psq[c][pt][cnt];
                    }
                }
            }

            return k;
        }

        /// Position::compute_psq_score() computes the incremental scores for the middle
        /// game and the endgame. These functions are used to initialize the incremental
        /// scores when a new position is set up, and to verify that the scores are correctly
        /// updated by do_move and undo_move when the program is running in debug mode.
        internal int compute_psq_score()
        {
            var score = ScoreC.SCORE_ZERO;

            for (var b = this.occupied_squares; b != 0;)
            {
                var s = Utils.pop_lsb(ref b);
                score += Zobrist.PieceSquareTable[this.piece_on(s)][s];
            }

            return score;
        }

        /// compute_non_pawn_material() computes the total non-pawn middle
        /// game material value for the given side. Material values are updated
        /// incrementally during the search, this function is only used while
        /// initializing a new Position object.
        internal int compute_non_pawn_material(int c)
        {
            var value = ValueC.VALUE_ZERO;

            for (var pt = PieceTypeC.KNIGHT; pt <= PieceTypeC.QUEEN; pt++)
            {
                value += this.piece_count(c, pt) * PieceValue[PhaseC.MG][pt];
            }

            return value;
        }

        /// is_draw() tests whether the position is drawn by material,
        /// repetition, or the 50 moves rule. It does not detect stalemates, this
        /// must be done by the search.
        internal bool is_draw(bool SkipRepetition)
        {
            // Draw by material?
            if ((this.byTypeBB[PieceTypeC.PAWN] == 0)
                && ((this.st.npMaterialWHITE + this.st.npMaterialBLACK) <= Constants.BishopValueMidgame))
            {
                return true;
            }

            // Draw by the 50 moves rule?
            if (this.st.rule50 > 99)
            {
                if (this.st.checkersBB == 0)
                {
                    return true;
                }
                var mlist = MListBroker.GetObject();
                mlist.pos = 0;
                var pos2 = this;
                Movegen.generate_legal(pos2, mlist.moves, ref mlist.pos);
                var any = mlist.pos > 0;
                MListBroker.Free();
                if (any)
                {
                    return true;
                }
            }

            // Draw by repetition?
            if (!SkipRepetition)
            {
                int i = 4, e = Math.Min(this.st.rule50, this.st.pliesFromNull);
                if (i <= e)
                {
                    var stp = this.st.previous.previous;
                    do
                    {
                        stp = stp.previous.previous;

                        if (stp.key == st.key)
                            return true;

                        i += 2;

                    } while (i <= e);
                }
            }
            return false;
        }
        
        /// flip() flips position with the white and black sides reversed. This
        /// is only useful for debugging especially for finding evaluation symmetry bugs.
        internal void flip()
        {
            // Make a copy of current position before to start changing
            var pos = new Position(this);
            this.clear();

            this.sideToMove = pos.sideToMove ^ 1;
            this.thisThread = pos.this_thread();
            this.nodes = pos.nodes;
            this.chess960 = pos.chess960;
            this.startPosPly = pos.startpos_ply_counter();

            for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
            {
                if (!pos.is_empty(s))
                {
                    this.put_piece((pos.piece_on(s) ^ 8), Utils.flip_S(s));
                }
            }

            if (pos.can_castle_CR(CastleRightC.WHITE_OO) != 0)
            {
                this.set_castle_right(
                    ColorC.BLACK,
                    Utils.flip_S(pos.castle_rook_square(ColorC.WHITE, CastlingSideC.KING_SIDE)));
            }
            if (pos.can_castle_CR(CastleRightC.WHITE_OOO) != 0)
            {
                this.set_castle_right(
                    ColorC.BLACK,
                    Utils.flip_S(pos.castle_rook_square(ColorC.WHITE, CastlingSideC.QUEEN_SIDE)));
            }
            if (pos.can_castle_CR(CastleRightC.BLACK_OO) != 0)
            {
                this.set_castle_right(
                    ColorC.WHITE,
                    Utils.flip_S(pos.castle_rook_square(ColorC.BLACK, CastlingSideC.KING_SIDE)));
            }
            if (pos.can_castle_CR(CastleRightC.BLACK_OOO) != 0)
            {
                this.set_castle_right(
                    ColorC.WHITE,
                    Utils.flip_S(pos.castle_rook_square(ColorC.BLACK, CastlingSideC.QUEEN_SIDE)));
            }

            if (pos.st.epSquare != SquareC.SQ_NONE)
            {
                this.st.epSquare = Utils.flip_S(pos.st.epSquare);
            }

            // Checkers
            this.st.checkersBB = this.attackers_to(this.king_square(this.sideToMove))
                                 & this.pieces_C(Utils.flip_C(this.sideToMove));

            // Hash keys
            this.st.key = this.compute_key();
            this.st.pawnKey = this.compute_pawn_key();
            this.st.materialKey = this.compute_material_key();

            // Incremental scores
            this.st.psqScore = this.compute_psq_score();

            // Material
            this.st.npMaterialWHITE = this.compute_non_pawn_material(ColorC.WHITE);
            this.st.npMaterialBLACK = this.compute_non_pawn_material(ColorC.BLACK);

            Debug.Assert(this.pos_is_ok());
        }

        /// pos_is_ok() performs some consitency checks for the position object.
        /// This is meant to be helpful when debugging.
        internal bool pos_is_ok()
        {
            var junk = 0;
            return this.pos_is_ok(ref junk);
        }

        private bool pos_is_ok(ref int step) //failedStep)
        {
            //int dummy, *step = failedStep ? failedStep : &dummy;

            // What features of the position should be verified?
            var all = false;

            var debugBitboards = all || false;
            var debugKingCount = all || false;
            var debugKingCapture = false; // all || false; // TODO: fixthis
            var debugCheckerCount = all || false;
            var debugKey = all || false;
            var debugMaterialKey = all || false;
            var debugPawnKey = all || false;
            var debugIncrementalEval = all || false;
            var debugNonPawnMaterial = all || false;
            var debugPieceCounts = all || false;
            var debugPieceList = all || false;
            var debugCastleSquares = all || false;

            step = 1;
            if (this.sideToMove != ColorC.WHITE && this.sideToMove != ColorC.BLACK)
            {
                return false;
            }

            step++;
            if (this.piece_on(this.king_square(ColorC.WHITE)) != PieceC.W_KING)
            {
                return false;
            }

            step++;
            if (this.piece_on(this.king_square(ColorC.BLACK)) != PieceC.B_KING)
            {
                return false;
            }

            step++;
            if (debugKingCount)
            {
                var kingCount = new int[ColorC.COLOR_NB];

                for (var s = SquareC.SQ_A1; s <= SquareC.SQ_H8; s++)
                {
                    if (Utils.type_of(this.piece_on(s)) == PieceTypeC.KING)
                    {
                        kingCount[Utils.color_of(this.piece_on(s))]++;
                    }
                }

                if (kingCount[0] != 1 || kingCount[1] != 1)
                {
                    return false;
                }
            }

            step++;
            if (debugKingCapture)
            {
                if ((this.attackers_to(this.king_square(this.sideToMove ^ 1)) & this.pieces_C(this.sideToMove)) != 0)
                {
                    return false;
                }
            }

            step++;
            if (debugCheckerCount && Bitcount.popcount_1s_Full(this.st.checkersBB) > 2)
            {
                return false;
            }

            step++;
            if (debugBitboards)
            {
                // The intersection of the white and black pieces must be empty
                if ((this.pieces_C(ColorC.WHITE) & this.pieces_C(ColorC.BLACK)) != 0)
                {
                    return false;
                }

                // The union of the white and black pieces must be equal to all
                // occupied squares
                if ((this.pieces_C(ColorC.WHITE) | this.pieces_C(ColorC.BLACK)) != this.occupied_squares)
                {
                    return false;
                }

                // Separate piece type bitboards must have empty intersections
                for (var p1 = PieceTypeC.PAWN; p1 <= PieceTypeC.KING; p1++)
                {
                    for (var p2 = PieceTypeC.PAWN; p2 <= PieceTypeC.KING; p2++)
                    {
                        if (p1 != p2 && ((this.pieces_PT(p1) & this.pieces_PT(p2)) != 0))
                        {
                            return false;
                        }
                    }
                }
            }

            step++;
            if (this.st.epSquare != SquareC.SQ_NONE
                && Utils.relative_rank_CS(this.sideToMove, this.st.epSquare) != RankC.RANK_6)
            {
                return false;
            }

            step++;
            if (debugKey && this.st.key != this.compute_key())
            {
                return false;
            }

            step++;
            if (debugPawnKey && this.st.pawnKey != this.compute_pawn_key())
            {
                return false;
            }

            step++;
            if (debugMaterialKey && this.st.materialKey != this.compute_material_key())
            {
                return false;
            }

            step++;
            if (debugIncrementalEval && this.st.psqScore != this.compute_psq_score())
            {
                return false;
            }

            step++;
            if (debugNonPawnMaterial)
            {
                if (this.st.npMaterialWHITE != this.compute_non_pawn_material(ColorC.WHITE)
                    || this.st.npMaterialBLACK != this.compute_non_pawn_material(ColorC.BLACK))
                {
                    return false;
                }
            }

            step++;
            if (debugPieceCounts)
            {
                for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
                {
                    for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
                    {
                        if (this.pieceCount[c][pt] != Bitcount.popcount_1s_Full(this.pieces_PTC(pt, c)))
                        {
                            return false;
                        }
                    }
                }
            }

            step++;
            if (debugPieceList)
            {
                for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
                {
                    for (var pt = PieceTypeC.PAWN; pt <= PieceTypeC.KING; pt++)
                    {
                        for (var i = 0; i < this.pieceCount[c][pt]; i++)
                        {
                            if (this.piece_on(this.pieceList[c][pt][i]) != Utils.make_piece(c, pt))
                            {
                                return false;
                            }

                            if (this.index[this.pieceList[c][pt][i]] != i)
                            {
                                return false;
                            }
                        }
                    }
                }
            }

            step++;
            if (debugCastleSquares)
            {
                for (var c = ColorC.WHITE; c <= ColorC.BLACK; c++)
                {
                    for (var s = CastlingSideC.KING_SIDE; s <= CastlingSideC.QUEEN_SIDE; s = (s + 1))
                    {
                        var cr = Utils.make_castle_right(c, s);

                        if (this.can_castle_CR(cr) == 0)
                        {
                            continue;
                        }

                        if ((this.castleRightsMask[this.king_square(c)] & cr) != cr)
                        {
                            return false;
                        }

                        if (this.piece_on(this.castleRookSquare[c][s]) != Utils.make_piece(c, PieceTypeC.ROOK)
                            || this.castleRightsMask[this.castleRookSquare[c][s]] != cr)
                        {
                            return false;
                        }
                    }
                }
            }

            step = 0;
            return true;
        }
    }
}