#include <iostream>
#include <conio.h>

#include <algorithm>
#include <iterator>
#include <numeric>
#include <string>
#include <valarray>
#include <vector>

#include "..\Utils\vectorHelper.h"

// Declare a helper to work with vectors
vectorHelper g_helper;

using namespace std;
void basicVector()
{
    // Inserting data in std::vector
    cout << "\n### Basic tutorial for 'std::vector<int>' ###";

    // Construct a simple vector of integers
    vector<int> vecOfInts = {3, 1, 4, 1, 5, 9};

    // Number of items in the map
    cout << "\nVector contains " << vecOfInts.size() << " items\n";

    // Iterate through all elements in std::vector
    cout << "\nIterate through items using 'std::vector::operator[]'\n";
    int pos = 0;
    for (pos = 0; pos < vecOfInts.size(); pos++)
        cout << "  [" << pos << "] :: " << vecOfInts[pos] << endl;

    cout << "\nIterate using 'std::vector::iterator'\n  ";
    vector<int>::iterator it = vecOfInts.begin();
    while (it != vecOfInts.end())
    {
        cout << *it << " ";
        it++;
    }
    cout << endl;

    cout << "\nReverse iterate using 'std::vector::reverse_iterator'\n  ";
    for (vector<int>::reverse_iterator itR = vecOfInts.rbegin(); itR != vecOfInts.rend(); ++itR)
        cout << *itR << " ";

    cout << endl;

    std::cout << "\nIterate using range-based 'for (auto& elem : vector)' (requires C++11)\n  ";
    for (auto& elem : vecOfInts)
        cout << elem << " ";
    
    cout << endl;

    // Clear the vector
    vecOfInts.clear();
    cout << "\nVector cleared, there are now " << vecOfInts.size() << " elements\n";
    cout << "#\n";
}

string firstLastHelper(const int vecSize, const int pos)
{
    // Helper for "firstLastVector"
    if (pos == 0)
        return "first";
    else if (pos == (vecSize - 1))
        return "last";
    
    return "middle";
}

void firstLastVector()
{
    // Detecting the first and last element of std::vector (while looping)
    cout << "\n### Finding first and last elements of a 'std::vector<int>' ###\n";

    // Construct a simple vector of integers
    vector<int> vec = {3, 1, 4, 1};
    cout << "(";
    g_helper.PrintVector<int>(vec);
    cout << ")";

    // As we iterate through the vector, detect when we are on the first or last element
    cout << "\nSimple 'for' loop (good)\n";
    int pos = 0;
    for (pos = 0; pos < vec.size(); pos++)
        cout << "  " << vec[pos] << " " << firstLastHelper(vec.size(), pos) << endl;
    
    cout << "\nUsing 'std::vector::iterator' and 'std::next' (good)\n";
    vector<int>::iterator it = vec.begin();
    for (; it != vec.end(); it++)
    {
        cout << "  " << *it << " ";
        if (it == vec.begin())
            cout << "first";
        else if (next(it) == vec.end()) // Using std::next from C++11
            cout << "last";
        else
            cout << "middle";
        
        cout << endl;
    }

    cout << "\nRange-based 'auto' and knowledge that std::vector elements are stored contiguously (fine)\n";
    for (auto& elem : vec)
    {
        pos = (&elem - &*(vec.begin()));
        cout << "  " << elem << " " << firstLastHelper(vec.size(), pos) << endl;
    }

    cout << "\nLambda with 'std::for_each' (weird)\n";
    int vecSize = vec.size();
    std::for_each(begin(vec), end(vec), [pos = size_t{}, vecSize] (auto elem) mutable
    {
        cout << "  " << elem << " " << firstLastHelper(vecSize, pos) << endl;
        ++pos;
    });

    cout << "#\n";
}

void copyVector()
{
    // Copying std::vector
    cout << "\n### Copy variables of type 'std::vector<T>' ###\n";

    // Construct a simple vector of integers
    vector<int> vec1 = {3, 1, 4, 1, 5, 9};
    cout << "Start   : ";
    g_helper.PrintVector<int>(vec1);

    // Copy the vector
    {
        // Method 1 (using loop)
        cout << "\nCopy 1  : ";
        vector<int> copy1;
        for (int i=0; i<vec1.size(); i++)
            copy1.push_back(vec1[i]);

        g_helper.PrintVector<int>(copy1);
        cout << " (loop over elements)";
    }

    {
        // Method 2 (using iterator)
        cout << "\nCopy 2  : ";
        vector<int> copy2;
        vector<int>::iterator it = vec1.begin();
        while (it != vec1.end())
        {
            copy2.push_back(*it);
            it++;
        }

        g_helper.PrintVector<int>(copy2);
        cout << " (using an iterator)";
    }

    {
        // Method 3 (by direct assignment)
        cout << "\nCopy 3  : ";
        vector<int> copy3 = vec1;
        g_helper.PrintVector<int>(copy3);
        cout << " (by assignment)";
    }

    {
        // Method 4 (passing to constructor)
        cout << "\nCopy 4  : ";
        vector<int> copy4(vec1);
        g_helper.PrintVector<int>(copy4);
        cout << " (constructor)";
    }

    {
        // Method 5a (std::copy)
        cout << "\nCopy 5a : ";
        vector<int> copy5a;
        copy(vec1.begin(), vec1.end(), back_inserter(copy5a)); 
        g_helper.PrintVector<int>(copy5a);
        cout << " (using std::copy)";
    }

    {
        // Method 5b (std::assign)
        cout << "\nCopy 5b : ";
        vector<int> copy5b;
        copy5b.assign(vec1.begin(), vec1.end()); 
        g_helper.PrintVector<int>(copy5b);
        cout << " (using std::assign)";
    }

    cout << "\n#\n";
}

void sumVector()
{
    // Summing the elements in a std::vector
    cout << "\n### Sum elements of 'std::vector<int>' ###\n";

    // Construct a simple vector
    vector<int> vec = {3, 1, 4, 1, 5, 9};
    cout << "(";
    g_helper.PrintVector<int>(vec);
    cout << ")";

    // Sum the elements
    int sum = 0;
    {
        // Method 1 (using loop)
        sum = 0;
        for (int i=0; i<vec.size(); i++)
            sum += vec[i];

        cout << "\nSum 1  = " << sum << " (simple loop)";
    }

    {
        // Method 2 (using iterator)
        sum = 0;
        for (vector<int>::iterator it = vec.begin(); it != vec.end(); ++it)
            sum += *it;

        cout << "\nSum 2  = " << sum << " (loop with iterator)";
    }

    {
        // Method 3a (std::accumulate)
        // Note: The last parameter is the type of accumulation. Change to "0.0f", for example, for floats.
        sum = accumulate(vec.begin(), vec.end(), 0);
        cout << "\nSum 3a = " << sum << " (std::accumulate, C++03)";
    }

    {
        // Method 3b (std::accumulate which tracks changes to underlying tpe of the vector)
        sum = accumulate(vec.begin(), vec.end(), decltype(vec)::value_type(0));
        cout << "\nSum 3b = " << sum << " (std::accumulate with type tracking, C++11)";
    }

     {
        // Method 4 (using std:valarray). Not recommended - use std::accumulate instead.
        valarray<int> vec_add{vec.data(), vec.size()};
        sum = vec_add.sum();
        cout << "\nSum 4  = " << sum << " (std:valarray, c++03)";
    }

    {
        // Method 5a (std::for_each with functor). Not recommended - use std::accumulate instead.
        struct DoSum
        {
            DoSum(int* t) : total(t) { };
            int* total;
            void operator()(int element)
            {
                *total += element;
            }
        };

        sum = 0;
        DoSum doSum(&sum);
        for_each(vec.begin(), vec.end(), doSum);
        cout << "\nSum 5a = " << sum << " (std::for_each with functor, C++03)";
    }

    {
        // Method 5b (std::for_each with lambda, C++11)
        sum = 0;
        for_each(vec.begin(), vec.end(), [&] (int n) { sum += n; });
        cout << "\nSum 5b = " << sum << " (std::for_each with lambda, C++11)";
    }

    {
        // Method 6 (using range-based loop, C++11)
        sum = 0;
        for (auto& elem : vec)
            sum += elem;

        cout << "\nSum 6  = " << sum << " (range-based loop using auto, C++11)";
    }

    cout << "\n#\n";
}

void searchVector()
{
    // Determine whether a particular value is in the vector
    cout << "\n### Searching 'std::vector<int>' ###\n";

    // Construct a simple vector
    vector<int> vec = {3, 1, 4, 1, 5, 9};
    cout << "(";
    g_helper.PrintVector<int>(vec);
    cout << ")";

    // Search for the existence of particular value (we'll search for the numbers 1 through 4)
    {
        // Method 1 (using loop). Simple loops, iterators, range-based loops, etc are all essentially
        // the same. Only a simple loop is demonstrated here.
        cout << "\nSearch 1 - Simple loop\n";
        bool found = false;
        for (int findMe=1; findMe<=4; findMe++)
        {
            found = false;
            cout << "  " << findMe << ": ";
            for (int i=0; i<vec.size(); i++)
            {
                if (vec[i] == findMe)
                {
                    found = true;
                    break;
                }
            }
            
            cout << boolalpha << found << endl;
        }
    }

    {
        // Method 2 (std::find). See vectorHelper.h for implementation details.
        cout << "\nSearch 2 - std::find (recommended)\n";
        for (int findMe=1; findMe<=4; findMe++)
        {
            cout << "  " << findMe << ": ";
            cout << boolalpha << g_helper.Contains(vec, findMe) << endl;
        }
    }

    {
        // Method 3 (std::anyof using a lambda). Not recommended - use std::find instead.
        cout << "\nSearch 3 - std::any_of with a lambda function\n";
        for (int findMe=1; findMe<=4; findMe++)
        {
            cout << "  " << findMe << ": ";
            cout << boolalpha <<
                any_of(vec.begin(), vec.end(), [&](const int& elem) { return elem == findMe; }) << endl;
        }
    }

    {
        // Method 4 (std::count). Not recommended (because of efficiency) - use std::find instead.
        cout << "\nSearch 4 - std::count\n";
        for (int findMe=1; findMe<=4; findMe++)
        {
            cout << "  " << findMe << ": ";
            cout << boolalpha <<
                (count(vec.begin(), vec.end(), findMe) > 0) << endl;
        }
    }

    cout << "#\n";
}

void templateVector()
{
    // Supplying different types to the vector
    cout << "\n### Types and templates with 'std::vector' ###\n";

    // Construct vectors of various types
    cout << "  "; vector<int> vecInt = {3, 1, 4, 1, 5, 9};
    g_helper.PrintVector<int>(vecInt);
    cout << "\t\t[std::vector<int>]\n";

    cout << "  "; vector<double> vecDouble = {3.14159, -3.2};
    g_helper.PrintVector<double>(vecDouble);
    cout << "\t\t[std::vector<double>]\n";

    cout << "  "; vector<char> vecChar = {'h', 'e', 'l', 'p', '?'};
    g_helper.PrintVector<char>(vecChar);
    cout << "\t\t[std::vector<char>]\n";

    cout << "  "; vector<string> vecString = {"hello", "world", "!"};
    g_helper.PrintVector<string>(vecString);
    cout << "\t\t[std::vector<string>]\n";
    cout << "#\n";
}

int main()
{
    // References for the std::map data structure:
    // * https://en.cppreference.com/w/cpp/container/vector

    // Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);

    // Basic vector tests
    basicVector();
    firstLastVector();
    copyVector();
    sumVector();
    searchVector();
    templateVector();

    // Prompt for exit
    cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}
