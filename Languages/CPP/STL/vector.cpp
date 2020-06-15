#include <iostream>
#include <conio.h>
#include <vector>
#include <string>
#include <iterator>

using namespace std;

// Forward declare of a function (bottom of file)

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
    std::cout << "\nVector cleared, there are now " << vecOfInts.size() << " elements\n";
    std::cout << "#\n";
}
 
int main()
{
    // References for the std::map data structure:
    // * https://en.cppreference.com/w/cpp/container/vector

    // Basic vector
    basicVector();

    // Prompt for exit
    cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}

// Helper function(s)
