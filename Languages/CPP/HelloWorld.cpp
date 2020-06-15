#include <stdio.h>

/* Sample program which can be compiled from the command-line.

    - Windows (Visual Studio): cl /EHsc HelloWorld.cpp /link /out:hw.exe
		- Alternative: cl /O2 HelloWorld.cpp /link /out:hw.exe
		- More generally: cl [options] FILE1 FILE2 ... /link /out:OUTPUT
    - Linux Ubuntu (gcc): gcc HelloWorld.cpp -o hw.out
        - More generally "gcc [options] FILE1 FILE2 ... -o OUTPUT"
		- Optimization: gcc -O3 -g0 HelloWorld.cpp -o hw.out
			- O3 => Maximise optimization for code size and speed
			- g0 => No debug information
*/
int main(int argc, char* args[])
{
    printf("Hello, world!\n");
    return 0;
}
