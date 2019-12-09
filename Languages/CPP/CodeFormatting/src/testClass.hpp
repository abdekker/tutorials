#pragma once
#include <string>

using namespace std;
class TestClass
{
public:
    TestClass();
    TestClass(const string szStartMessage) :
        m_szLastMessage(szStartMessage)
    {
        // Constructor with parameter list
    }
    ~TestClass();

    void accumulate(int toAdd);
    void printMessage(string szMessage);
    static void staticPrintMessage(const string szConstMessage);

private:
    string m_szLastMessage;

    void funcEmpty   ();
    void funcParameters();

    template <typename T>
    T adder(T first, T second)
        {
                return (first + second);    // Type T must have operator+ defined...
            }
};
