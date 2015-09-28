namespace Portfish
{
    public interface IPlug
    {
        void Write(string message);

        string ReadLine();
    }

    public static class Plug
    {
        internal static IPlug Interface;

        internal static bool IsWarmup = false;

        public static void Init(IPlug iFace)
        {
            Interface = iFace;
        }

        public static void Write(string message)
        {
            if (!IsWarmup)
            {
                Interface.Write(message);
            }
        }

        public static string ReadLine()
        {
            return Interface.ReadLine();
        }
    }
}