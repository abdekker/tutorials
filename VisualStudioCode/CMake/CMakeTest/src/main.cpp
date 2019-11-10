#include <iostream>
#include "testClass.hpp"

using namespace std;
int main()
{
    // Simple tutorial for a CMake project in Visual Studio Code
    cout << "main::Testing CMake in Visual Studio Code\n";

    // Test whether this is a Debug or Release build
    #ifdef _DEBUG
        cout << "main::Debug build, line 1\n";
        cout << "main::Debug build, line 2\n\n";
    #else
        cout << "main::Release build\n\n";
    #endif

    // Call a method in another class
    TestClass myTest;
    myTest.PrintMessage("Hello from main");

    // Back in main...
    cout << "\nmain::Back in main...\n";

    // Random variables (to play with colour customisations)
    int bob = 3;
    float mary = 3.3f;

    // Exit...
    cout << "Press RETURN to continue...";
    getchar();
}
