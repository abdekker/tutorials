//INSERT_TERAKI_COPYRIGHT_MESSAGE_HERE//
#pragma once

#include <cstring>

// Simple helper class for using strings
using namespace std;
class stringHelper
{
public:
    stringHelper()
    {
        // Constructor
    }

    ~stringHelper()
    {
        // Destructor
    }

    // Template functions
    template <typename... Args>
    string formatString(const char *format, Args... args)
    {
        // Helper function to format a string

        // Example:
        //      string myString = "Hello";
        //      int myInt = 123;
        //      double myDouble = 456.789d;
        //      cout << formatString("%s %05d %10.5f", myString.c_str(), myInt, myDouble);
        int length = snprintf(nullptr, 0, format, args...);
        if (length >= 0)
        {
            char *buffer = new char[length + 1];
            snprintf(buffer, length + 1, format, args...);
            string formatted(buffer);
            delete[] buffer;
            return formatted;
        }
        else
            return string(); // Empty std::string
    }

    // Non-template functions
    bool containsSubString(const string &mainString, const string &subString)
    {
        // Example: containsSubString(string("fred"), string("red"));   // true
        return (mainString.find(subString) != string::npos);
    }
};