    #include        <iostream>
#include "testClass.hpp"

using namespace std;
int main()
{
    // Simple code formatting example
    // Formatting has been deliberately misformed for testing! Get originals from "srcBackup".
  cout << "main::Code formatting in IDEs (eg. CLion, Visual Studio Code, Qt Creator, etc)\n";

    // Test whether this is a Debug or Release build
    #ifdef _DEBUG
            cout << "  main::Debug build!\n\n";
    #else
        cout << "  main::Release build!\n\n";
    #endif

    // for loop
    cout << "main::for loop\n";
    int number = 0;
    for (int number = 0; number < 10; number++)
    {
        // With brackets in the for loop
        if (number % 2)
        {
            cout << "  main::for::number is even\n";
        }
        else  {
            cout   << "  main::for::number is odd\n";
        }
    }
    cout << endl;

    // do-while loop
    cout << "main::do-while loop\n";
    number = 0;
    do {
        // No brackets in the do-while loop
        if(number  %  2)
          cout << "  main::do-while::even number\n";
        else
            cout << "  main::do-while::odd number\n";
    } while (++number < 10);
    cout << endl;

    // switch statement
    cout << "main::switch statement\n";
    switch (number)

    {
        case 1:
            cout << "  main::your number is small\n";
                break;

    case 100:
        cout << "  main::your number is large\n";
        break;

        default:
        {
            int anotherNumber = number;
            anotherNumber++;
            cout << "  main::your number is equal to " << number << endl;
        }
    }
    cout << endl;

    // Call methods in another class
    TestClass::staticPrintMessage("Hello from main (static)");
    TestClass myTest;
    myTest.printMessage  (  "Hello from main (method)"  );

        // Exit...
    cout << "\nmain::Back in main, bye!\n"            ;
}
