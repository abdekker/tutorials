#include <iostream>
#include "testClass.hpp"

TestClass::TestClass()
{
    // Constructor...
    cout << "TestClass constructor...\n";
    szLastMessage = string();
}

TestClass::~TestClass()
{
    // Destructor...clean up?
    cout << "TestClass destructor...\n";
}

void TestClass::PrintMessage(string szMessage)
{
    cout << "TestClass::PrintMessage::" << szMessage << endl;
    #ifdef _DEBUG
        cout << "TestClass::Debug build\n";
    #else
        cout << "TestClass::Release build\n";
    #endif

    szLastMessage = szMessage;
}
