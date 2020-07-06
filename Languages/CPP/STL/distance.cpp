#include <iostream>
#include <iterator>
#include <vector>

// Adapted from: https://en.cppreference.com/w/cpp/iterator/distance
using namespace std;
int main()
{
    // Ensure the output buffer is flushed on each insertion operation
    setvbuf(stdout, NULL, _IONBF, 0);

    // Demonstrates std::distance(a, b). Returns the number of hops from a to b, which can be negative.
    cout << "std::distance(iteratorA, iteratorB) returns the number of hops between A and B (which can be negative)\n";

    // Define a vector
    vector<int> v{ 3, 1, 4, 1, 5, 9 };
    cout << "Vector: ";
    for (auto elem : v)
        cout << elem;

    cout << " (size = " << v.size() << ")\n\n";

    // Return the distance between the start and end of the vector
    cout << "Basic distance\n";
    cout << "  distance(first, last) = " << distance(v.begin(), v.end()) << endl;
    cout << "  distance(last, first) = " << distance(v.end(), v.begin()) << "\n";
    cout << "  distance(first, first) = " << distance(v.begin(), v.begin()) << "\n";
    cout << "  distance(last, last) = " << distance(v.end(), v.end()) << "\n";
    // Note: Before C++11, the above behaviour was undefined

    // Now check the distance, but not from the start
    cout << "\nDistance, but not from start\n";
    vector<int>::iterator it = v.begin();
    it += 2;
    cout << "  distance(3rd [" << *it << "], last) = " << std::distance(it, v.end());
        cout << " (using \"v.begin() + 2\")\n";

    it = v.begin();
    advance(it, 4);
    cout << "  distance(5th [" << *it << "], last) = " << std::distance(it, v.end());
        cout << " (using \"std::advance(v.begin(), 4)\")\n";
    
    // Now loop through the elements
    cout << "\nLoop through elements\n";
    it = v.begin();
    while (it != v.end())
    {
        cout << "  distance([" << *it << "], last) = " << std::distance(it, v.end()) << endl;
        it++;
    }

    cout << "  distance(final, last) = " << std::distance(it++, v.end()) << endl;
    cout << "\nAll done!\n";
}
