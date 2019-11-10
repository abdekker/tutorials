#include <iostream>
#include "testClass.hpp"

TestClass::TestClass()
{
    // Constructor...nothing to do
}

TestClass::~TestClass()
{
    // Destructor...clean up?
}

void TestClass::PrintMessage(string szMessage)
{
    cout << "TestClass::PrintMessage::" << szMessage << endl;
    #ifdef _DEBUG
        cout << "TestClass::Debug build\n";
    #else
        cout << "TestClass::Release build\n";
    #endif
}
