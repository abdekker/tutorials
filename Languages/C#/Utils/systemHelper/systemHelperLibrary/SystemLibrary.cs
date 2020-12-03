using System;

namespace systemHelperLibrary
{
    // This library project was original adapted from:
    // https://docs.microsoft.com/en-us/dotnet/core/tutorials/library-with-visual-studio
    public static class SystemLibrary
    {
        public static bool Is64Bit()
        {
            // Is this process 64-bit or 32-bit? There are two good methods in C#.
            // Method 1: Use Environment.Is64BitProcess
            // Method 2: Check the size of IntPtr
            return (IntPtr.Size == 8);
        }
    }
}
