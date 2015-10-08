namespace Portfish
{
    using System.Text;

    public sealed class Engine
    {
        public void Run(object arguments)
        {
            var args = (string[])arguments;

            Plug.Write(Utils.engine_info());
            Plug.Write(Constants.endl);

            CheckInfoBroker.init();
            EvalInfoBroker.init();
            SwapListBroker.init();
            MovesSearchedBroker.init();
            PositionBroker.init();
            StateInfoArrayBroker.init();

            MListBroker.init();
            LoopStackBroker.init();
            MovePickerBroker.init();
            StateInfoBroker.init();

            Utils.init();
            PolyglotBook.init();
            Zobrist.init();
            KPKPosition.init_kpk();
            Endgame.init();
            Search.init();
            Evaluate.init();

            Threads.init();

            // .Net warmup sequence
            Plug.IsWarmup = true;
            var pos = new Position(Uci.StartFEN, false, Threads.main_thread());
            var stack = Utils.CreateStack("go depth 7");
            Uci.go(pos, stack);
            Threads.wait_for_think_finished();
            Plug.IsWarmup = false;

            var sb = new StringBuilder();
            for (var i = 1; i < args.Length; i++)
            {
                sb.Append(args[i]).Append(" ");
            }

            Uci.loop(sb.ToString());

            Threads.exit();
        }
    }
}