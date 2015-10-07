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

    internal enum GenType
    {
        CAPTURES,

        QUIETS,

        QUIET_CHECKS,

        EVASIONS,

        NON_EVASIONS,

        LEGAL
    };

    internal static class Movegen
    {
#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        private static ulong move_pawns(int Delta, ulong p)
        {
            return Delta == SquareC.DELTA_N
                       ? p << 8
                       : Delta == SquareC.DELTA_S
                             ? p >> 8
                             : Delta == SquareC.DELTA_NE
                                   ? (p & ~Constants.FileHBB) << 9
                                   : Delta == SquareC.DELTA_SE
                                         ? (p & ~Constants.FileHBB) >> 7
                                         : Delta == SquareC.DELTA_NW
                                               ? (p & ~Constants.FileABB) << 7
                                               : Delta == SquareC.DELTA_SW ? (p & ~Constants.FileABB) >> 9 : 0;
        }

        private static void generate_castle(
            int Side,
            bool Checks,
            Position pos,
            MoveStack[] ms,
            ref int mpos,
            int us)
        {
            if (pos.castle_impeded(us, Side) || (pos.can_castle_CR(Utils.make_castle_right(us, Side)) == 0))
            {
                return;
            }

            // After castling, the rook and king final positions are the same in Chess960
            // as they would be in standard chess.
            var kfrom = pos.king_square(us);
            var rfrom = pos.castle_rook_square(us, Side);
            var kto = Utils.relative_square(us, Side == CastlingSideC.KING_SIDE ? SquareC.SQ_G1 : SquareC.SQ_C1);

            var enemies = pos.pieces_C(us ^ 1);

            Debug.Assert(!pos.in_check());

            int K = pos.chess960 ? kto > kfrom ? -1 : 1 : Side == CastlingSideC.KING_SIDE ? -1 : 1;
            
            for (Square s = kto; s != kfrom; s += (Square)K)
            {
                if ((pos.attackers_to(s) & enemies) != 0)
                {
                    return;
                }
            }

            // Because we generate only legal castling moves we need to verify that
            // when moving the castling rook we do not discover some hidden checker.
            // For instance an enemy queen in SQ_A1 when castling rook is in SQ_B1.
            if (pos.chess960 && ((pos.attackers_to(kto, Utils.xor_bit(pos.occupied_squares, rfrom)) & enemies) != 0))
            {
                return;
            }

            var m = Utils.make(kfrom, rfrom, MoveTypeC.CASTLING);

            if (Checks)
            {
                var ci = CheckInfoBroker.GetObject();
                ci.CreateCheckInfo(pos);
                var givesCheck = pos.move_gives_check(m, ci);
                CheckInfoBroker.Free();
                if (!givesCheck)
                {
                    return;
                }
            }

            ms[mpos++].move = m;
        }

        private static void generate_promotions(
            GenType Type,
            int Delta,
            MoveStack[] ms,
            ref int mpos,
            ulong pawnsOn7,
            ulong target,
            CheckInfo ci)
        {
            var b = move_pawns(Delta, pawnsOn7) & target;
            while (b != 0)
            {
                var to = Utils.pop_lsb(ref b);

                if (Type == GenType.CAPTURES || Type == GenType.EVASIONS || Type == GenType.NON_EVASIONS)
                {
                    ms[mpos++].move = Utils.make(to - Delta, to, MoveTypeC.PROMOTION, PieceTypeC.QUEEN);
                }

                if (Type == GenType.QUIETS || Type == GenType.EVASIONS || Type == GenType.NON_EVASIONS)
                {
                    ms[mpos++].move = Utils.make(to - Delta, to, MoveTypeC.PROMOTION, PieceTypeC.ROOK);
                    ms[mpos++].move = Utils.make(to - Delta, to, MoveTypeC.PROMOTION, PieceTypeC.BISHOP);
                    ms[mpos++].move = Utils.make(to - Delta, to, MoveTypeC.PROMOTION, PieceTypeC.KNIGHT);
                }

                // Knight-promotion is the only one that can give a direct check not
                // already included in the queen-promotion.
                if (Type == GenType.QUIET_CHECKS
                    && (Utils.bit_is_set(Utils.StepAttacksBB[PieceC.W_KNIGHT][to], ci.ksq) != 0))
                {
                    ms[mpos++].move = Utils.make(to - Delta, to, MoveTypeC.PROMOTION, PieceTypeC.KNIGHT);
                }
            }
        }

        private static void generate_pawn_moves(
            int Us,
            GenType Type,
            Position pos,
            MoveStack[] ms,
            ref int mpos,
            ulong target,
            CheckInfo ci)
        {
            // Compute our parametrized parameters at compile time, named according to
            // the point of view of white side.
            var Them = (Us == ColorC.WHITE ? ColorC.BLACK : ColorC.WHITE);
            var TRank8BB = (Us == ColorC.WHITE ? Constants.Rank8BB : Constants.Rank1BB);
            var TRank7BB = (Us == ColorC.WHITE ? Constants.Rank7BB : Constants.Rank2BB);
            var TRank3BB = (Us == ColorC.WHITE ? Constants.Rank3BB : Constants.Rank6BB);
            var UP = (Us == ColorC.WHITE ? SquareC.DELTA_N : SquareC.DELTA_S);
            var RIGHT = (Us == ColorC.WHITE ? SquareC.DELTA_NE : SquareC.DELTA_SW);
            var LEFT = (Us == ColorC.WHITE ? SquareC.DELTA_NW : SquareC.DELTA_SE);

            ulong b1, b2, dc1, dc2, emptySquares = 0;
            int to;

            var pawnsOn7 = (pos.byTypeBB[PieceTypeC.PAWN] & pos.byColorBB[Us]) & TRank7BB;
            var pawnsNotOn7 = (pos.byTypeBB[PieceTypeC.PAWN] & pos.byColorBB[Us]) & ~TRank7BB;

            var enemies = (Type == GenType.EVASIONS
                               ? pos.byColorBB[Them] & target
                               : Type == GenType.CAPTURES ? target : pos.byColorBB[Them]);

            // Single and double pawn pushes, no promotions
            if (Type != GenType.CAPTURES)
            {
                emptySquares = (Type == GenType.QUIETS ? target : ~pos.occupied_squares);

                b1 = move_pawns(UP, pawnsNotOn7) & emptySquares;
                b2 = move_pawns(UP, b1 & TRank3BB) & emptySquares;

                if (Type == GenType.EVASIONS) // Consider only blocking squares
                {
                    b1 &= target;
                    b2 &= target;
                }

                if (Type == GenType.QUIET_CHECKS)
                {
                    b1 &= Utils.StepAttacksBB[((Them << 3) | PieceTypeC.PAWN)][ci.ksq];
                    b2 &= Utils.StepAttacksBB[((Them << 3) | PieceTypeC.PAWN)][ci.ksq];

                    // Add pawn pushes which give discovered check. This is possible only
                    // if the pawn is not on the same file as the enemy king, because we
                    // don't generate captures. Note that a possible discovery check
                    // promotion has been already generated among captures.
                    if ((pawnsNotOn7 & ci.dcCandidates) != 0) // Target is dc bitboard
                    {
                        dc1 = move_pawns(UP, pawnsNotOn7 & ci.dcCandidates) & emptySquares & ~(Utils.FileBB[ci.ksq & 7]);
                        dc2 = move_pawns(UP, dc1 & TRank3BB) & emptySquares;

                        b1 |= dc1;
                        b2 |= dc2;
                    }
                }

                while (b1 != 0)
                {
                    to = Utils.pop_lsb(ref b1);
                    ms[mpos++].move = (to | ((to - UP) << 6));
                }
                while (b2 != 0)
                {
                    to = Utils.pop_lsb(ref b2);
                    ms[mpos++].move = (to | ((to - UP - UP) << 6));
                }
            }

            // Promotions and underpromotions
            if ((pawnsOn7 != 0) && (Type != GenType.EVASIONS || ((target & TRank8BB) != 0)))
            {
                if (Type == GenType.CAPTURES)
                {
                    emptySquares = ~pos.occupied_squares;
                }

                if (Type == GenType.EVASIONS)
                {
                    emptySquares &= target;
                }

                generate_promotions(Type, RIGHT, ms, ref mpos, pawnsOn7, enemies, ci);
                generate_promotions(Type, LEFT, ms, ref mpos, pawnsOn7, enemies, ci);
                generate_promotions(Type, UP, ms, ref mpos, pawnsOn7, emptySquares, ci);
            }

            // Standard and en-passant captures
            if (Type == GenType.CAPTURES || Type == GenType.EVASIONS || Type == GenType.NON_EVASIONS)
            {
                b1 = move_pawns(RIGHT, pawnsNotOn7) & enemies;
                b2 = move_pawns(LEFT, pawnsNotOn7) & enemies;

                while (b1 != 0)
                {
                    to = Utils.pop_lsb(ref b1);
                    ms[mpos++].move = (to | ((to - RIGHT) << 6));
                }
                while (b2 != 0)
                {
                    to = Utils.pop_lsb(ref b2);
                    ms[mpos++].move = (to | ((to - LEFT) << 6));
                }

                if (pos.st.epSquare != SquareC.SQ_NONE)
                {
                    Debug.Assert(Utils.rank_of(pos.st.epSquare) == Utils.relative_rank_CR(Us, RankC.RANK_6));

                    // An en passant capture can be an evasion only if the checking piece
                    // is the double pushed pawn and so is in the target. Otherwise this
                    // is a discovery check and we are forced to do otherwise.
                    if (Type == GenType.EVASIONS && (((target & Utils.SquareBB[pos.st.epSquare - UP]) == 0)))
                    {
                        return;
                    }

                    b1 = pawnsNotOn7 & Utils.StepAttacksBB[((Them << 3) | PieceTypeC.PAWN)][pos.st.epSquare];

                    Debug.Assert(b1 != 0);

                    while (b1 != 0)
                    {
                        ms[mpos++].move = Utils.make(Utils.pop_lsb(ref b1), pos.st.epSquare, MoveTypeC.ENPASSANT);
                    }
                }
            }
        }

        private static void generate_all(
            GenType type,
            Position pos,
            MoveStack[] mlist,
            ref int mpos,
            int us,
            ulong target,
            CheckInfo ci)
        {
            var Checks = type == GenType.QUIET_CHECKS;

            generate_pawn_moves(us, type, pos, mlist, ref mpos, target, ci);

            generate_moves(PieceTypeC.KNIGHT, Checks, pos, mlist, ref mpos, us, target, ci);
            generate_moves(PieceTypeC.BISHOP, Checks, pos, mlist, ref mpos, us, target, ci);
            generate_moves(PieceTypeC.ROOK, Checks, pos, mlist, ref mpos, us, target, ci);
            generate_moves(PieceTypeC.QUEEN, Checks, pos, mlist, ref mpos, us, target, ci);

            if (!Checks && type != GenType.EVASIONS)
            {
                Square from = pos.king_square(us);
                Bitboard b = Position.attacks_from_KING(from) & target;
                // SERIALIZE(b);
                while (b != 0)
                {
#if X64
                    Bitboard bb = b;
                    b &= (b - 1);
                 mlist[mpos++].move = ((Utils.BSFTable[((bb & (0xffffffffffffffff - bb + 1)) * DeBruijn_64) >> 58]) | (from << 6));
#else
                    mlist[mpos++].move = Utils.make_move(from, Utils.pop_lsb(ref b));
#endif
                }
            }
            
            if (type != GenType.CAPTURES && type != GenType.EVASIONS && pos.can_castle_C(us) != 0)
            {
                generate_castle(CastlingSideC.KING_SIDE, Checks, pos, mlist, ref mpos, us);
                generate_castle(CastlingSideC.QUEEN_SIDE, Checks, pos, mlist, ref mpos, us);
            }
        }

        private static void generate_moves(
            int Pt,
            bool Checks,
            Position pos,
            MoveStack[] mlist,
            ref int mpos,
            int us,
            ulong target,
            CheckInfo ci)
        {
            Debug.Assert(Pt != PieceTypeC.KING && Pt != PieceTypeC.PAWN);

            var pl = pos.pieceList[us][Pt];
            var plPos = 0;
            
            for (var from = pl[plPos]; from != SquareC.SQ_NONE; from = pl[++plPos])
            {
                if (Checks)
                {
                    if ((Pt == PieceTypeC.BISHOP || Pt == PieceTypeC.ROOK || Pt == PieceTypeC.QUEEN)
                        && ((Utils.PseudoAttacks[Pt][from] & (ulong)target & ci.checkSq[Pt]) == 0))
                    {
                        continue;
                    }

                    if ((ci.dcCandidates != 0) && (Utils.bit_is_set(ci.dcCandidates, from) != 0))
                    {
                        continue;
                    }
                }

                var b = pos.attacks_from_PTS(Pt, from) & (ulong)target;

                if (Checks)
                {
                    b &= ci.checkSq[Pt];
                }

                // SERIALIZE(b);
                while (b != 0)
                {
                    mlist[mpos++].move = Utils.make_move(from, Utils.pop_lsb(ref b));
                }
            }
        }

        internal static void generate_legal(Position pos, MoveStack[] ms, ref int mpos)
        {
            /// generate<LEGAL> generates all the legal moves in the given position
            var pinned = pos.pinned_pieces();
            Square ksq = pos.king_square(pos.sideToMove);

            if (pos.in_check())
            {
                generate_evasion(pos, ms, ref mpos);
            }
            else
            {
                generate_non_evasion(pos, ms, ref mpos);
            }

            var last = mpos;
            var cur = 0;
            while (cur != last)
            {
                var curMove = ms[cur].move;
                //if (!pos.pl_move_is_legal(ms[cur].move, pinned))
                if ((pinned != 0 || Utils.from_sq(curMove) == ksq || Utils.type_of_move(curMove) == MoveTypeC.ENPASSANT) && !pos.pl_move_is_legal(curMove, pinned))
                {
                    ms[cur].move = ms[--last].move;
                }
                else
                {
                    cur++;
                }
            }
            mpos = last;
        }

        internal static void generate_evasion(Position pos, MoveStack[] ms, ref int mpos)
        {
            /// generate<EVASIONS> generates all pseudo-legal check evasions when the side
            /// to move is in check. Returns a pointer to the end of the move list.
            Debug.Assert(pos.in_check());

            ulong b;
            int from, checksq;
            var checkersCnt = 0;
            var us = pos.sideToMove;
            var ksq = pos.king_square(us);
            ulong sliderAttacks = 0;
            var checkers = pos.st.checkersBB;

            Debug.Assert(checkers != 0);

            // Find squares attacked by slider checkers, we will remove them from the king
            // evasions so to skip known illegal moves avoiding useless legality check later.
            b = checkers;
            do
            {
                checkersCnt++;
                checksq = Utils.pop_lsb(ref b);

                Debug.Assert(Utils.color_of(pos.piece_on(checksq)) == Utils.flip_C(us));

                switch (Utils.type_of(pos.piece_on(checksq)))
                {
                    case PieceTypeC.BISHOP:
                        sliderAttacks |= Utils.PseudoAttacks[PieceTypeC.BISHOP][checksq];
                        break;
                    case PieceTypeC.ROOK:
                        sliderAttacks |= Utils.PseudoAttacks[PieceTypeC.ROOK][checksq];
                        break;
                    case PieceTypeC.QUEEN:
                        // If queen and king are far or not on a diagonal line we can safely
                        // remove all the squares attacked in the other direction becuase are
                        // not reachable by the king anyway.
                        if ((Utils.between_bb(ksq, checksq) != 0)
                            || ((Utils.bit_is_set(Utils.PseudoAttacks[PieceTypeC.BISHOP][checksq], ksq)) == 0))
                        {
                            sliderAttacks |= Utils.PseudoAttacks[PieceTypeC.QUEEN][checksq];
                        }

                        // Otherwise we need to use real rook attacks to check if king is safe
                        // to move in the other direction. For example: king in B2, queen in A1
                        // a knight in B1, and we can safely move to C1.
                        else
                        {
                            sliderAttacks |= Utils.PseudoAttacks[PieceTypeC.BISHOP][checksq]
                                             | pos.attacks_from_ROOK(checksq);
                        }
                        break;
                    default:
                        break;
                }
            }
            while (b != 0);

            // Generate evasions for king, capture and non capture moves
            b = Position.attacks_from_KING(ksq) & ~pos.pieces_C(us) & ~sliderAttacks;
            from = ksq;
            while (b != 0)
            {
                ms[mpos++].move = Utils.make_move(from, Utils.pop_lsb(ref b));
            }

            // Generate evasions for other pieces only if not under a double check
            if (checkersCnt > 1)
            {
                return;
            }

            // Blocking evasions or captures of the checking piece
            var target = Utils.between_bb(checksq, ksq) | checkers;

            generate_all(GenType.EVASIONS, pos, ms, ref mpos, us, target, null);
        }

        internal static void generate_quiet_check(Position pos, MoveStack[] ms, ref int mpos)
        {
            /// generate<MV_NON_CAPTURE_CHECK> generates all pseudo-legal non-captures and knight
            /// underpromotions that give check. Returns a pointer to the end of the move list.
            Debug.Assert(!pos.in_check());

            var ci = CheckInfoBroker.GetObject();
            ci.CreateCheckInfo(pos);
            var target = ~pos.occupied_squares;
            var dc = ci.dcCandidates;

            while (dc != 0)
            {
                var from = Utils.pop_lsb(ref dc);
                var pt = Utils.type_of(pos.piece_on(from));

                if (pt == PieceTypeC.PAWN)
                {
                    continue; // Will be generated together with direct checks
                }

                var b = pos.attacks_from_PTS(pt, from) & ~pos.occupied_squares;

                if (pt == PieceTypeC.KING)
                {
                    b &= ~Utils.PseudoAttacks[PieceTypeC.QUEEN][ci.ksq];
                }

                while (b != 0)
                {
                    ms[mpos++].move = Utils.make_move(from, Utils.pop_lsb(ref b));
                }
            }

            generate_all(GenType.QUIET_CHECKS, pos, ms, ref mpos, pos.sideToMove, target, ci);

            CheckInfoBroker.Free();
        }

        internal static void generate_quiet(Position pos, MoveStack[] ms, ref int mpos)
        {
            Debug.Assert(!pos.in_check());

            var us = pos.sideToMove;
            var target = ~pos.occupied_squares;

            generate_all(GenType.QUIETS, pos, ms, ref mpos, us, target, null);
        }

        internal static void generate_non_evasion(Position pos, MoveStack[] ms, ref int mpos)
        {
            Debug.Assert(!pos.in_check());

            var us = pos.sideToMove;
            var target = ~(pos.byColorBB[us]);

            generate_all(GenType.NON_EVASIONS, pos, ms, ref mpos, us, target, null);
        }

        internal static void generate_capture(Position pos, MoveStack[] ms, ref int mpos)
        {
            Debug.Assert(!pos.in_check());

            var us = pos.sideToMove;
            var target = pos.byColorBB[us ^ 1];

            generate_all(GenType.CAPTURES, pos, ms, ref mpos, us, target, null);
        }
    }
}