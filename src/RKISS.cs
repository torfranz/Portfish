namespace Portfish
{
    using System.Runtime.CompilerServices;

    /// RKISS is our pseudo random number generator (PRNG) used to compute hash keys.
    /// George Marsaglia invented the RNG-Kiss-family in the early 90's. This is a
    /// specific version that Heinz van Saanen derived from some internal domain code
    /// by Bob Jenkins. Following the feature list, as tested by Heinz.
    /// 
    /// - Quite platform independent
    /// - Passes ALL dieharder tests! Here *nix sys-rand() e.g. fails miserably:-)
    /// - ~12 times faster than my *nix sys-rand()
    /// - ~4 times faster than SSE2-version of Mersenne twister
    /// - Average cycle length: ~2^126
    /// - 64 bit seed
    /// - Return doubles with a full 53 bit mantissa
    /// - Thread safe
    internal sealed class RKISS
    {
        private ulong a, b, c, d, e;

        // Init seed and scramble a few rounds
        internal RKISS(long seed = 73)
        {
            this.a = 0xf1ea5eed;
            this.b = this.c = this.d = 0xd4e12c77;
            for (var i = 0; i < seed; i++) // Scramble a few rounds
            {
                this.rand();
            }
        }

#if AGGR_INLINE
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
#endif

        internal ulong rand()
        {
            this.e = this.a - ((this.b << 7) | (this.b >> 57));
            this.a = this.b ^ ((this.c << 13) | (this.c >> 51));
            this.b = this.c + ((this.d << 37) | (this.d >> 27));
            this.c = this.d + this.e;
            return this.d = this.e + this.a;
        }
    }
}