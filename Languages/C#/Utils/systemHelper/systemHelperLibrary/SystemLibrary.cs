using System;
using System.Runtime.InteropServices;

namespace systemHelperLibrary
{
    // This library project was original adapted from:
    // https://docs.microsoft.com/en-us/dotnet/core/tutorials/library-with-visual-studio
    // Note: To run "unsafe" code, enable "Build > Allow unsafe code" on the project properties

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

    /* To use this library in another project:
        - In Solution Explorer, right-click References and select "Add Reference..."
        - Select Browse then click "Browse..."
        - Navigate to this library
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

        public static int GetObjectSize(object obj)
        {
            // Return the size of the object (eg. 1 for "System.Byte")
            int objSize = -1;
            try
            {
                // This fails for types such as "System.String"...
                objSize = Marshal.SizeOf(obj);
            }
            catch { }
            return objSize;
        }
    }
}