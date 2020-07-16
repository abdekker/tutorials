#include <iostream>
#include "testClass.hpp"

TestClass::TestClass()
{
    // Constructor...
    cout << "TestClass constructor...\n";
    m_szLastMessage = string();     // Empty string
}

TestClass::~TestClass()
{
    // Destructor...
        cout << "TestClass destructor...\n";
    }

void TestClass::accumulate(int toAdd)
{
    static int counter = 0;
    counter    += toAdd;
}

void TestClass::printMessage(string szMessage)
{
    // Single line C++ comment: Save message and print it
    m_szLastMessage = szMessage;
    cout << "TestClass::printMessage::" << szMessage << endl;

    /* Multi-line C-style comment:
    2nd line...let's go! */
    double sumDouble = adder(1.1, 2.2);
    cout << "  TestClass::printMessage::Double sum = " << sumDouble << endl;

    int32_t sumInt = adder(3, 23);
    cout << "  TestClass::printMessage::Integer sum = " << sumInt << endl;
}

void TestClass::staticPrintMessage(const string szConstMessage)
{
    // Static version of "printMessage"
    cout << "TestClass::staticPrintMessage::" << szConstMessage << endl;
}

// Start: Private functions
void TestClass::funcEmpty() {}

void TestClass::funcParameters()
{
    int     coolNumber;     // Number
    bool    coolBoolean;    // Flag
    string  coolString;     // String
        float coolFloat; // Float; type and comment deliberately so not align with lines above
}
// End: Private functions
