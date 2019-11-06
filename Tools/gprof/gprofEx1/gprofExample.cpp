#include <stdio.h>

#include "gprofExample.hpp"
#include "gprofExampleNew.hpp"

using namespace std;
int main() {
    // This tutorial is about using the gprof command-line profiler in Linux
    // Based on: https://www.thegeekstuff.com/2012/08/gprof-tutorial/

    // We add "-gp" to gcc to get profiling information included in the output binary
    //      gcc -Wall -pg FILE1 FILE2 -o OUTPUT.o

    // When test_gprof.o is executed, "gmon.out" is generated. Now generate the analysis with:
    //      gprof OUTPUT.o gmon.out > analysis.txt

    // gprof example:
    //      gprof OUTPUT.o gmon.out > analysis.txt (Standard output with "Flat profile" and "Call graph" sections)
    //      gprof -a OUTPUT.o gmon.out > analysis.txt (Exclude static function)
    //      gprof -b OUTPUT.o gmon.out > analysis.txt (Remove verbose blurbs)

    //      gprof -p -b OUTPUT.o gmon.out > analysis.txt (Output only the "Flat profile" section)
    //      gprof -pFUNC -b OUTPUT.o gmon.out > analysis.txt (Output only information related to FUNC)
    //      gprof -P -b OUTPUT.o gmon.out > analysis.txt (Suppress "Flat profile" section and only include "Call graph")

    //      gprof -q -b OUTPUT.o gmon.out > analysis.txt (Output only the "Call graph" section)
    //      gprof -qFUNC -b OUTPUT.o gmon.out > analysis.txt (Output only information related to FUNC)
    //      gprof -Q -b OUTPUT.o gmon.out > analysis.txt (Suppress "Call graph" section and only include "Flat profile")
    printf("\nInside main()\n");

    unsigned int i = 0;
    for(;i<0xffffff;i++);
    func1();
    func2();

    return 0;
}

void func1(void)
{
    printf("\n Inside func1\n");

    unsigned int i = 0;
    for(;i<0xffffffff;i++);
    new_func1();

    return;
}

static void func2(void)
{
    printf("\n Inside func2 (static)\n");

    unsigned int i = 0;
    for(;i<0xffffffaa;i++);

    return;
}
