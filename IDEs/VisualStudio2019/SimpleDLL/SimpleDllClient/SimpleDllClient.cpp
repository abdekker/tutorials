// SimpleDllClient.cpp : Client app for a simple C++ DLL. This file contains the 'main' function.
// Program execution begins and ends there.
#include <iostream>
#include "SimpleDLL.h"

int main()
{
    // Start with a friendly message
    std::cout << "Hello World from a simple DLL client!\n";

    // Initialize a Fibonacci relation sequence
    fibonacci_init(1, 1);

    // Write out the sequence values until overflow
    do {
        std::cout << fibonacci_index() << ": " << fibonacci_current() << std::endl;
    } while (fibonacci_next());

    // Report count of values written before overflow
    std::cout << fibonacci_index() + 1 <<
        " Fibonacci sequence values fit in an " <<
        "unsigned 64-bit integer." << std::endl;
}
