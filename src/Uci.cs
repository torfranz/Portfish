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

namespace Portfish
{
    using System.Collections.Generic;

    internal static class Uci
    {
        // FEN string of the initial position, normal chess
        internal const string StartFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

        // Keep track of position keys along the setup moves (from start position to the
        // position just before to start searching). This is needed by draw detection
        // where, due to 50 moves rule, we need to check at most 100 plies back.
        internal static readonly StateInfo[] StateRingBuf = new StateInfo[102];

        internal static int SetupStatePos; // *SetupState = StateRingBuf;

        /// Wait for a command from the user, parse this text string as an UCI command,
        /// and call the appropriate functions. Also intercepts EOF from stdin to ensure
        /// that we exit gracefully if the GUI dies unexpectedly. In addition to the UCI
        /// commands, the function also supports a few debug commands.
        internal static void loop(string args)
        {
            for (var i = 0; i < 102; i++)
            {
                StateRingBuf[i] = new StateInfo();
            }

            var pos = new Position(StartFEN, false, Threads.main_thread()); // The root position
            string cmd, token = string.Empty;

            while (token != "quit")
            {
                if (args.Length > 0)
                {
                    cmd = args;
                }
                else if (string.IsNullOrEmpty(cmd = Plug.ReadLine())) // Block here waiting for input
                {
                    cmd = "quit";
                }
                var stack = Utils.CreateStack(cmd);

                token = stack.Pop();

                if (token == "quit" || token == "stop")
                {
                    Search.SignalsStop = true;
                    Threads.wait_for_search_finished(); // Cannot quit while threads are running
                }
                else if (token == "ponderhit")
                {
                    // The opponent has played the expected move. GUI sends "ponderhit" if
                    // we were told to ponder on the same move the opponent has played. We
                    // should continue searching but switching from pondering to normal search.
                    Search.Limits.ponder = false;

                    if (Search.SignalsStopOnPonderhit)
                    {
                        Search.SignalsStop = true;
                        Threads.main_thread().wake_up(); // Could be sleeping
                    }
                }
                else if (token == "go")
                {
                    go(pos, stack);
                }
                else if (token == "ucinewgame")
                {
                    TT.clear();
                }
                else if (token == "isready")
                {
                    Plug.Write("readyok");
                    Plug.Write(Constants.endl);
                }
                else if (token == "position")
                {
                    set_position(pos, stack);
                }
                else if (token == "setoption")
                {
                    set_option(stack);
                }
                else if (token == "validmoves")
                {
                    Search.validmoves(pos, stack);
                }
                else if (token == "d")
                {
                    pos.print(0);
                }
                else if (token == "flip")
                {
                    pos.flip();
                }
                else if (token == "eval")
                {
                    Plug.Write(Evaluate.trace(pos));
                    Plug.Write(Constants.endl);
                }
                else if (token == "bench")
                {
                    Benchmark.benchmark(pos, stack);
                }
                else if (token == "key")
                {
                    Plug.Write("key: ");
                    Plug.Write(string.Format("{0:X}", pos.key()));
                    Plug.Write("\nmaterial key: ");
                    Plug.Write(pos.material_key().ToString());
                    Plug.Write("\npawn key: ");
                    Plug.Write(pos.pawn_key().ToString());
                    Plug.Write(Constants.endl);
                }
                else if (token == "uci")
                {
                    Plug.Write("id name ");
                    Plug.Write(Utils.engine_info(true));
                    Plug.Write("\n");
                    Plug.Write(OptionMap.Instance.ToString());
                    Plug.Write("\nuciok");
                    Plug.Write(Constants.endl);
                }
                else if (token == "perft")
                {
                    token = stack.Pop(); // Read depth
                    var ss =
                        Utils.CreateStack(
                            string.Format(
                                "{0} {1} {2} current perft",
                                OptionMap.Instance["Hash"].v,
                                OptionMap.Instance["Threads"].v,
                                token));
                    Benchmark.benchmark(pos, ss);
                }
                else
                {
                    Plug.Write("Unknown command: ");
                    Plug.Write(cmd);
                    Plug.Write(Constants.endl);
                }

                if (args.Length > 0) // Command line arguments have one-shot behaviour
                {
                    Threads.wait_for_search_finished();
                    break;
                }
            }
        }

        // set_position() is called when engine receives the "position" UCI
        // command. The function sets up the position described in the given
        // fen string ("fen") or the starting position ("startpos") and then
        // makes the moves given in the following move list ("moves").
        internal static void set_position(Position pos, Stack<string> stack)
        {
            int m;
            string token, fen = string.Empty;

            token = stack.Pop();

            if (token == "startpos")
            {
                fen = StartFEN;
                if (stack.Count > 0)
                {
                    token = stack.Pop();
                } // Consume "moves" token if any
            }
            else if (token == "fen")
            {
                while ((stack.Count > 0) && (token = stack.Pop()) != "moves")
                {
                    fen += token + " ";
                }
            }
            else
            {
                return;
            }

            pos.from_fen(fen, bool.Parse(OptionMap.Instance["UCI_Chess960"].v), Threads.main_thread());

            // Parse move list (if any)
            while ((stack.Count > 0) && (m = Utils.move_from_uci(pos, token = stack.Pop())) != MoveC.MOVE_NONE)
            {
                pos.do_move(m, StateRingBuf[SetupStatePos]);

                // Increment pointer to StateRingBuf circular buffer
                SetupStatePos = (SetupStatePos + 1) % 102;
            }
        }

        // set_option() is called when engine receives the "setoption" UCI command. The
        // function updates the UCI option ("name") to the given value ("value").
        // setoption name Ponder value false
        internal static void set_option(Stack<string> stack)
        {
            string token, name = null, value = null;

            // Consume "name" token
            stack.Pop();

            // Read option name (can contain spaces)
            while ((stack.Count > 0) && ((token = stack.Pop()) != "value"))
            {
                name += (name == null ? string.Empty : " ") + token;
            }

            // Read option value (can contain spaces)
            while ((stack.Count > 0) && ((token = stack.Pop()) != "value"))
            {
                value += (value == null ? string.Empty : " ") + token;
            }

            if (OptionMap.Instance.Contains(name))
            {
                OptionMap.Instance[name].v = value;
            }
            else
            {
                Plug.Write("No such option: ");
                Plug.Write(name);
                Plug.Write(Constants.endl);
            }
        }

        // go() is called when engine receives the "go" UCI command. The function sets
        // the thinking time and other parameters from the input string, and then starts
        // the main searching thread.
        internal static void go(Position pos, Stack<string> stack)
        {
            var token = string.Empty;
            var limits = new LimitsType();
            var searchMoves = new List<int>();

            while (stack.Count > 0)
            {
                token = stack.Pop();

                if (token == "wtime")
                {
                    limits.time[ColorC.WHITE] = int.Parse(stack.Pop());
                }
                else if (token == "btime")
                {
                    limits.time[ColorC.BLACK] = int.Parse(stack.Pop());
                }
                else if (token == "winc")
                {
                    limits.inc[ColorC.WHITE] = int.Parse(stack.Pop());
                }
                else if (token == "binc")
                {
                    limits.inc[ColorC.BLACK] = int.Parse(stack.Pop());
                }
                else if (token == "movestogo")
                {
                    limits.movesToGo = int.Parse(stack.Pop());
                }
                else if (token == "depth")
                {
                    limits.depth = int.Parse(stack.Pop());
                }
                else if (token == "nodes")
                {
                    limits.nodes = int.Parse(stack.Pop());
                }
                else if (token == "movetime")
                {
                    limits.movetime = int.Parse(stack.Pop());
                }
                else if (token == "mate")
                {
                    limits.mate = int.Parse(stack.Pop());
                }
                else if (token == "infinite")
                {
                    limits.infinite = 1;
                }
                else if (token == "ponder")
                {
                    limits.ponder = true;
                }
                else if (token == "searchmoves")
                {
                    while ((token = stack.Pop()) != null)
                    {
                        searchMoves.Add(Utils.move_from_uci(pos, token));
                    }
                }
            }

            Threads.start_searching(pos, limits, searchMoves);
        }
    }
}