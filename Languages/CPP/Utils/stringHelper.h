#pragma once

#include <cstring>

// Header-only helper class for using strings
// Note: It is bad practice to include something like "using namespace X" in header files, or
// before "#include" statements. If used, put them in your private implementation (.cpp) files.
class stringHelper
{
public:
    // Constructor / destructor
    stringHelper() {}
    ~stringHelper() {}

    // Variadic template functions (using C++11 or later)
    template <typename... Args>
    std::string formatString(const char *format, Args... args)
    {
        // Helper function to format a string

        // Example:
        //      string myString = "Hello";
        //      int myInt = 123;
        //      double myDouble = 456.789;
        //      cout << formatString("%s %05d %10.5f", myString.c_str(), myInt, myDouble);
        // Result: Hello 00123  456.78900
        int length = snprintf(nullptr, 0, format, args...);
        if (length >= 0)
        {
            char *buffer = new char[length + 1];
            snprintf(buffer, length + 1, format, args...);
            std::string formatted(buffer);
            delete[] buffer;
            return formatted;
        }
        else
            return std::string(); // Empty std::string
    }

    // Non-template functions
    bool containsSubString(const std::string &mainString, const std::string &subString)
    {
        // Example: containsSubString(string("fred"), string("red"));   // true
        return (mainString.find(subString) != std::string::npos);
    }
};
