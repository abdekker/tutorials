// SimpleDllClient.cpp : Client app for a simple C++ DLL. This file contains the 'main' function.
// Program execution begins and ends there.
#include <iostream>
#include <conio.h>
#include "SimpleDLL.h"

int main()
{
    // Start with a friendly message
    std::cout << "Hello World from a simple DLL client (written in VS 2019)!\n\n";

    // Test functions
    std::cout << "Test functions from DLL:\n";
    std::cout << "SimpleReturn()\t\t\t= " << SimpleReturn() << "\n";
    std::cout << "SimpleSum(4, -11)\t\t= " << SimpleSum(4, -11) << "\n";
    std::cout << "SimpleMultiply(3.7f, 12)\t= " << SimpleMultiply(3.7f, 12) << "\n\n";

    // Initialize a 64-bit Fibonacci relation sequence
    std::cout << "Fibonacci sequence (64-bit)\n";
    fibonacciInit64(1, 1);

    // Write out the sequence values until overflow
    do {
        std::cout << fibonacciIndex64() << ": " << fibonacciCurrent64() << std::endl;
    } while (fibonacciNext64());

    // Report count of values written before overflow
    std::cout << fibonacciIndex64() + 1 <<
        " Fibonacci sequence values fit in an unsigned 64-bit integer.\n\n";

    // Repeat with 32-bit
    std::cout << "Fibonacci sequence (32-bit)\n";
    fibonacciInit32(1, 1);
    do {
        std::cout << fibonacciIndex32() << ": " << fibonacciCurrent32() << std::endl;
    } while (fibonacciNext32());

    std::cout << fibonacciIndex32() + 1 <<
        " Fibonacci sequence values fit in an unsigned 32-bit integer.\n";

    // Prompt for exit
    std::cout << "Finished...press a key to exit\n";
    _getch();
}
