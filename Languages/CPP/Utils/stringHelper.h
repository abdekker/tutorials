#pragma once

#include <cstring>
#include <algorithm>
#include <random>
#include <vector>

#include "mathHelper.h"
#include "vectorHelper.h"

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
    std::string FormatString(const char *format, Args... args)
    {
        // Helper function to format a string

        // Example:
        //      string myString = "Hello";
        //      int myInt = 123;
        //      double myDouble = 456.789;
        //      cout << formatString("%s %05d %12.5f", myString.c_str(), myInt, myDouble);
        // Result: Hello 00123    456.78900
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
    bool ContainsSubString(const std::string &mainString, const std::string &subString)
    {
        // Example: containsSubString(string("fred"), string("red"));   // true
        return (mainString.find(subString) != std::string::npos);
    }

    std::string GetRandomString(uint8_t length, bool lettersOnly, bool removeIllegal = false)
    {
        // Generate a randomised string
        std::vector<char> output;
        mathHelper mHelper;
        length = mHelper.clamp(length, (uint8_t)1, (uint8_t)100);

        // Assign allowed characters to a temporary array
        uint8_t start, end, character;
        std::vector<char> allowed;
        std::vector<char> forbidden;
        if (lettersOnly)
        {
            // Lowercase letters only
            start = 97;     // a
            end = 122;      // z
        }
        else
        {
            // Any printable ASCII character
            start = 33;     // !
            end = 126;      // ~

            // Remove illegal characters? Use if the random string will be used to create a file/folder in
            // the file system. Forbidden printable ASCII characters are:
            // * Windows:   \ / : * ? " < > |
            // * MacOS:     :
            // * Linux:     /
            if (removeIllegal)
            {
                // Use the Windows superset because it covers MacOS and Linux too
                forbidden.push_back('\\');
                forbidden.push_back('/');
                forbidden.push_back(':');
                forbidden.push_back('*');
                forbidden.push_back('?');
                forbidden.push_back('"');
                forbidden.push_back('<');
                forbidden.push_back('>');
                forbidden.push_back('|');
            }
        }

        if (forbidden.size() > 0)
        {
            vectorHelper vHelper;
            for (character = start; character <= end; character++)
            {
                if (!vHelper.Contains(forbidden, (char)character))
                    allowed.push_back((char)character);
            }
        }
        else
        {
            for (character = start; character <= end; character++)
                allowed.push_back((char)character);
        }

        // Create a random number generator (with optional seed for performance and test)
        std::mt19937 rnd(std::random_device{}());
        std::uniform_int_distribution<> dist(0, INT_MAX);   // Avoid constructing distribution all the time

        // Build randomised string
        while (output.size() < length)
            output.push_back(allowed[dist(rnd) % allowed.size()]);

        return std::string(output.begin(), output.end());
    }
};
