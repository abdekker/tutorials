#include <iostream>

// Function overload resolution in C++
double f(__int64 a, double b) { return (a * b); }
double f(unsigned __int64 a, double b) { return (a * b); }
double f(double a, double b) { return (a * b); }

int main()
{
    // Ensure the output buffer is flushed on each insertion operation
    setvbuf(stdout, NULL, _IONBF, 0);
    std::cout << "Function overload resolution in C++\n\n";

    int z = 3;
    //f(z,4);                   // error C2668: 'f': ambiguous call to overloaded function
    f(__int64(z),4);            // OK
    f(unsigned __int64(z),4);   // OK
    f(double(z),4);             // OK
    return 0;
}
