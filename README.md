Portfish
========

Portfish - .Net port of Stockfish (UCI chess engine)

Now with Windows RT support!

Latest versions could be downloaded from: https://github.com/downloads/bpfliegel/Portfish/PortfishLatestRelease.zip

0. Introduction
---------------

Portfish is the .Net port of Stockfish, functionally equal to Stockfish 2012 may 27, commit 0412f4a1ee.
All features of the original version are present, except search and debug logging is stripped.
The fastest builds are 4.0 x64 and 4.5 x64.


1. Motivation
---------------

Portfish was created to serve as a free and powerful chess engine for platforms that does not support
or have only limited support for C engines - e.g. Silverlight and Windows Phone.
It is/was also motivated to see what performance ratio is achievable after conversion, it is obviously
a lot slower than the original Stockfish. Ratio should be around 1:2,7 when running on a Windows desktop
and more when running on Linux with Mono support.
Portfish is not meant to be a clone or competitor of Stockfish in any way.


2. Files
--------

This distribution of Portfish consists of the following files:

  * Readme.md, the file you are currently reading.

  * src/Copying.txt, a text file containing the GNU General Public
    License.

  * src/, a subdirectory containing the full source code.
    For further information about how to compile Portfish yourself
    read section 4 below.


3. Opening books
----------------

The Stockfish opening book by Salvo Spitaleri is included in order to compile
Portfish for portable platforms (Silverlight, Windows Phone).


4. Compiling it yourself
------------------------

Portfish was compiled with Visual Studio version 10 and 11 beta for the following framework versions:
- .Net FW 2.0 (x86,x64)
- .Net FW 4.0 (x86,x64)
- .Net FW 4.5 (x86,x64)
- Visual Studio 2012 portable project

It was not yet compiled and tested with Mono, would be happy to receive feedback on that.

When compiling yourself, one should be careful about the following:

a) Compilation directives:
- X64 - bitboard operations are optimized for x64
- PORTABLE - no file system operations
- WINDOWS_RT - Windows RT compatible mode (no book support for now)
- AGGR_INLINE - Visual Studio 11 beta feature to mark methods for aggressive inlining.
To compile for Windows RT, please use both PORTABLE and WINDOWS_RT (besides NETFX_CORE) and other directives if you wish.

b) Constants defined in Constants.cs
- MaterialTableSize and PawnTableSize - make it small if compiling for e.g. mobile
- NumberOfCPUs - when compiling a portable edition, there is no way to count the number of CPUs,
this constant should be set for the correct number of CPUs.
(not needed for Windows RT)

c) Hash size
- Default or adjusted size of the transposition table (UciOptions.cs)

d) 'Optimize code' checkbox...

e) For mobile platforms and Windows RT one should create an IPlug implementation to be able
to communicate through UCI and launch the engine in the same way as presented
in Program.cs.
The WindowsRT solution has such a sample implementation.


5. Terms of use
---------------

Portfish is free, and distributed under the GNU General Public License
(GPL). Essentially, this means that you are free to do almost exactly
what you want with the program, including distributing it among your
friends, making it available for download from your web site, selling
it (either by itself or as part of some bigger software package), or
using it as the starting point for a software project of your own.

The only real limitation is that whenever you distribute Portfish in
some way, you must always include the full source code, or a pointer
to where the source code can be found. If you make any changes to the
source code, these changes must also be made available under the GPL.

For full details, read the copy of the GPL found in the file named
Copying.txt.