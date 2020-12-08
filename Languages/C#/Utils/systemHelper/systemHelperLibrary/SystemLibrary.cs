using System;

namespace systemHelperLibrary
{
    // This library project was original adapted from:
    // https://docs.microsoft.com/en-us/dotnet/core/tutorials/library-with-visual-studio

    /* To create a C# Class Library (DLL)
        - Create new project
        - Set language to "C#"
        - Enter "solution" in search
        - Select "Blank solution"
        - Right-click solution in Solution Explorer
        - Add > New Project
        - Enter "library" in search
        - Select "Class Library (.NET Framework)" (Windows only)
    */
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
