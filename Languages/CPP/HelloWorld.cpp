#include <stdio.h>

/* Sample program which can be compiled from the command-line.

    - Windows (Visual Studio): cl /EHsc /MD / O2 HelloWorld.cpp /link /out:hw.exe
        - More generally: cl [options] FILE1 FILE2 ... /link /out:OUTPUT
        - Flags:
            - /EHsc => Standard C++ stack unwinding, assume functions marked as extern "C" never throw a C++ exception
            - /MD => Dynamic-linking with the release version of the Windows runtime library
                - Use "/MT" for static-linking with the release runtime
                - Use "/MDd" for dynamic-linking with the debug runtime (and define _DEBUG)
                - Use "/MTd" for static-linking with the debug runtime (and define _DEBUG)
            - /O2 => Maximise optimization for code speed

    - Linux Ubuntu (gcc): gcc -O3 -g0 HelloWorld.cpp -o hw.out
        - More generally "gcc [options] FILE1 FILE2 ... -o OUTPUT"
        - Flags:
            - O3 => Maximise optimization for code size and speed
            - g0 => No debug information
*/
int main(int argc, char* args[])
{
    printf("Hello, world!\n");
    return 0;
}
