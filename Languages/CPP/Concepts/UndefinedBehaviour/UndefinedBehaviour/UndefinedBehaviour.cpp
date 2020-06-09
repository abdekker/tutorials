#include <iostream>
#include <conio.h>

using namespace std;

int main()
{
    // This sample console application explore some of the concepts of "undefined behaviour" in C/C++ code
 
    // Shift-left operator
    cout << "### Example 1: Shift-left operator ###\n";
    int myNumber = 1;
    cout << "  MyNumber = " << myNumber << endl;
    cout << "  Now shift left (1 << 32). This should generate a compiler warning.\n";
    myNumber = (1 << 32);   // warning C4293 [MSVC: shift count negative or too big, undefined behavior]
    cout << "  MyNumber = " << myNumber << endl;
 
    // Pause the program
    _getch();
}

