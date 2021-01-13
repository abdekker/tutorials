#pragma once

#include <algorithm>
#include <iomanip>
#include <vector>

// Header-only helper class for using vectors
class vectorHelper
{
public:
    // Constructor / destructor
    vectorHelper() {}
    ~vectorHelper() {}

    template <typename T> 
    const bool Contains(std::vector<T>& input, const T& elem)
    {
        // Does the vector contains the requested value?
        if (std::find(input.begin(), input.end(), elem) != input.end())
            return true;

        return false;
    }

    template <typename T>
    void PrintVector(std::vector<T> input, const char cSeparator = ' ', bool bNewLine = false)
    {
        // Output an array (std::vector) to the console eg. {1,11,21} => 1 11 21.
        // Example: stringHelper::PrintVector<int>(vecOfInts);
        if (input.size() > 0)
        {
            // This does not compile in VS 2019 with the "/permissive-" compiler option:
            //      for (std::vector<T>::iterator it = input.begin(); it != input.end();)
            // because std::vector<T>::iterator is a "dependent name". See full discussion here:
            // https://stackoverflow.com/questions/65647489

            // Resolve by using "auto" (or specify "std::vector<T>::iterator" is a type as in the code):
            //        for (auto it = input.begin(); it != input.end();)
            typename std::vector<T>::iterator it = input.begin();
            for (; it != input.end();)
            {
                std::cout << *it;
                if (++it != input.end())
                    std::cout << cSeparator;
            }

            // Alternatively, use a range-based loop using auto. This has the disadvantage of not
            // being easy to identify specific elements (such as the last):
            /*for (auto a : input)
            {
                std::cout << a << cSeparator;
            }*/

            if (bNewLine)
                std::cout << std::endl;
        }
        //else
        //    std::cout << "(empty)\n";
    }

    template <typename T>
    void PrintVector(std::vector<T> input, const int width, const char cSeparator = ' ', bool bNewLine = false)
    {
        // Version which uses an alignment specifier (std::setw)
        if (input.size() > 0)
        {
            (void) std::clamp(width, 1, 10); // Ensure width is valid
            typename std::vector<T>::iterator it = input.begin();
            for (; it != input.end();)
            {
                std::cout << std::setw(width) << *it;
                if (++it != input.end())
                    std::cout << cSeparator;
            }

            if (bNewLine)
                std::cout << std::endl;
        }
    }
};
