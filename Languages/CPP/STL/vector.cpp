#include <iostream>
#include <conio.h>
#include <vector>
#include <string>
#include <iterator>

#include "..\Utils\stringHelper.h"

using namespace std;
void basicVector()
{
    // Inserting data in std::vector
    cout << "### Basic tutorial of 'std::vector<int>' ###\n";

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

    std::cout << "\nIterate using 'for (auto& elem : vector)' (requires C++11)\n  ";
    for (auto& elem : vecOfInts)
        cout << elem << " ";
    
    cout << endl;

    // Clear the vector
    vecOfInts.clear();
    cout << "\nVector cleared, there are now " << vecOfInts.size() << " elements\n";
    cout << "#\n";
}

void copyVector()
{
    // Copying std::vector
    cout << "### Copying variables of type 'std::vector<T>' ###\n";

    // Declare a helper to investigate types
    stringHelper helper;

    // Construct a simple vector of integers
    vector<int> vec1 = {3, 1, 4, 1, 5, 9};
    cout << "Start   : ";
    helper.PrintVector<int>(vec1);

    // Copy the vector
    {
        // Method 1 (Looping)
        cout << "\nCopy 1  : ";
        vector<int> copy1;
        for (int i=0; i<vec1.size(); i++)
            copy1.push_back(vec1[i]);

        helper.PrintVector<int>(copy1);
        cout << "(loop over elements)";
    }

    {
        // Method 2 (Iterative)
        cout << "\nCopy 2  : ";
        vector<int> copy2;
        vector<int>::iterator it = vec1.begin();
        while (it != vec1.end())
        {
            copy2.push_back(*it);
            it++;
        }

        helper.PrintVector<int>(copy2);
        cout << "(using an iterator)";
    }

    {
        // Method 3 (Assignment)
        cout << "\nCopy 3  : ";
        vector<int> copy3 = vec1;
        helper.PrintVector<int>(copy3);
        cout << "(by assignment)";
    }

    {
        // Method 4 (Passing to constructor)
        cout << "\nCopy 4  : ";
        vector<int> copy4(vec1);
        helper.PrintVector<int>(copy4);
        cout << "(constructor)";
    }

    {
        // Method 5a (In-built methods - copy)
        cout << "\nCopy 5a : ";
        vector<int> copy5a;
        copy(vec1.begin(), vec1.end(), back_inserter(copy5a)); 
        helper.PrintVector<int>(copy5a);
        cout << "(using std::copy)";
    }

    {
        // Method 5b (In-built methods - assign)
        cout << "\nCopy 5b : ";
        vector<int> copy5b;
        copy5b.assign(vec1.begin(), vec1.end()); 
        helper.PrintVector<int>(copy5b);
        cout << "(using std::assign)";
    }

    cout << "\n#\n";
}

int main()
{
    // References for the std::map data structure:
    // * https://en.cppreference.com/w/cpp/container/vector

    // Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);

    // Basic vector tests
    basicVector();
    copyVector();

    // Prompt for exit
    cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}
