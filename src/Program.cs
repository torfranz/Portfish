namespace Portfish
{
    using System;
    using System.IO;
    using System.Text;

    internal sealed class ConsolePlug : IPlug
    {
        public void Write(string message)
        {
            Console.Write(message);
        }

        public string ReadLine()
        {
            return Console.ReadLine();
        }
    }

    internal class Program
    {
        private static readonly byte[] inputBuffer = new byte[8192];

        private static void Main(string[] args)
        {
            // Setup an 8k inputBuffer because really long UCI strings were getting truncated
            var inputStream = Console.OpenStandardInput(inputBuffer.Length);
            Console.SetIn(new StreamReader(inputStream, Encoding.ASCII, false, inputBuffer.Length));

            IPlug cp = new ConsolePlug();
            Plug.Init(cp);

            var e = new Engine();
            var t = new System.Threading.Thread(e.Run);
            t.Start(args);
        }
    }
}