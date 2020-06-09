#include <iostream>
#include <conio.h>
#include <limits.h>

using namespace std;

int main()
{
    // This sample console application explore some of the concepts of "undefined behaviour" in C/C++ code
 
    // Shift-left operator
    cout << "### Example 1: Shift-left operator ###\n";
    cout << "  [MSVC compiler warning C4293]\n";
    int myNumber = 1;
    cout << "  MyNumber = " << myNumber << endl;
    cout << "  Now shift left (X << 32)...this generates a compiler warning.\n";
    // Note: (1 << X) where X is 32 or greater will generate this warning
    myNumber = (1 << 32);           // warning C4293 [MSVC: shift count negative or too big, undefined behavior]
    cout << "  MyNumber = " << myNumber << endl << endl;

    // Largest representable integer
    cout << "### Example 2: Adding 1 to the largest representable integer ###\n";
    cout << "  [MSVC compiler warning C4307]\n";
    cout << "  INT_MAX (the largest representable integer) = " << INT_MAX << endl;
    cout << "  Is (INT_MAX + 1) positive or negative?";
    myNumber = (INT_MAX + 1);       // warning C4307 [MSVC: signed integral constant overflow]
    if (myNumber < 0)
        cout << " Negative (" << myNumber << ")!\n";
    else
        cout << " Non-negative (" << myNumber << ")!\n";

    // Prompt for exit
    std::cout << "\nFinished...press a key to exit\n";
    (void) _getch();
}
