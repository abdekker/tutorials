// SimpleDllClient.cpp : Client app for a simple C++ DLL. This file contains the 'main' function.
// Program execution begins and ends there.
#include <iostream>
#include <conio.h>
#include "SimpleDLL.h"
#include "SimpleDllWithExports.h"

int main()
{
    // Start with a friendly message
    std::cout << "Hello World from a simple DLL client (written in VS 2019)!\n\n";

    std::cout << "This application will consume functions from C++ DLLs:\n";
    std::cout << " 1) Written using the \"Dynamic-Link Library (DLL)\" wizard\n";
    std::cout << "\tContains simple \"extern \"C\"\" functions\n";
    std::cout << " 2) Written using the \"Dynamic-Link Library with exports (DLL)\" wizard\n";
    std::cout << "\tContains data, functions and a C++ class\n\n";

    // ### SimpleDLL ###
    std::cout << "### SimpleDLL ###\n";

    // Test functions
    std::cout << "Test functions from DLL\n";
    std::cout << "  SimpleReturn()\t\t= " << SimpleReturn() << "\n";
    std::cout << "  SimpleSum(4, -11)\t\t= " << SimpleSum(4, -11) << "\n";
    std::cout << "  SimpleMultiply(3.7f, 12)\t= " << SimpleMultiply(3.7f, 12) << "\n\n";

    // Initialize a 64-bit Fibonacci relation sequence
    std::cout << "Fibonacci sequence (64-bit)\n";
    fibonacciInit64(1, 1);

    // Write out the sequence values until overflow
    do {
        std::cout << fibonacciIndex() << ": " << fibonacciCurrent64() << std::endl;
    } while (fibonacciNext64());

    // Report count of values written before overflow
    std::cout << fibonacciIndex() + 1 <<
        " Fibonacci sequence values fit in an unsigned 64-bit integer.\n\n";

    // Repeat with 32-bit
    std::cout << "Fibonacci sequence (32-bit)\n";
    fibonacciInit32(1, 1);
    do {
        std::cout << fibonacciIndex() << ": " << fibonacciCurrent32() << std::endl;
    } while (fibonacciNext32());

    std::cout << fibonacciIndex() + 1 <<
        " Fibonacci sequence values fit in an unsigned 32-bit integer.\n\n";

    // ### SimpleDllWithExports ###
    std::cout << "### SimpleDllWithExports ###\n";
    std::cout << "  nSimpleReturn_Data\t\t= " << nSimpleReturn_Data << "\n";
    std::cout << "  SimpleReturn_NonClass()\t= " << SimpleReturn_NonClass() << "\n\n";

    // Use the exported class
    // Note: VS 2019 Intellisense does not find "SimpleHello_Class" which has been declared inline
    CSimpleDllWithExports exportedClass;
    
    std::cout << "  exportedClass\n";
    std::cout << "    SimpleHello_Class()\t\t\t= " << exportedClass.SimpleHello_Class() << "\n";
    std::cout << "    SimpleReturn_Class()\t\t= " << exportedClass.SimpleReturn_Class() << "\n";
    std::cout << "    SimpleMultiply_Class(3.7f, 12)\t= " <<
        exportedClass.SimpleMultiply_Class(3.7f, 12) << "\n\n";

    // Prompt for exit
    std::cout << "Finished...press a key to exit\n";
    (void) _getch();
}
