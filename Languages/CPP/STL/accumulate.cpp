
#include <conio.h>
#include <iostream>
#include <vector>
#include <numeric>
#include <string>
#include <functional>

// Adapted from: https://en.cppreference.com/w/cpp/algorithm/accumulate

int main()
{
    // Ensure debug output goes immediately to the console
    setvbuf(stdout, NULL, _IONBF, 0);

    std::cout << "std::accumulate can be used to sum, or apply a binary operation, on a range\n";
    std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    std::cout << "Array is: ";
    for (auto el : v) {
        std::cout << el << ' ';
    }
    std::cout << "\n\n";

    // Use operator += for the default implementation
    int sum = std::accumulate(v.begin(), v.end(), 0);
 
    // Apply the given binary operation
    int product = std::accumulate(v.begin(), v.end(), 1, std::multiplies<int>());

    // Local lambda
    auto dash_fold = [](std::string a, int b) {
                         return std::move(a) + '-' + std::to_string(b);
                     };

    // Left fold using forward iterators
    std::string s = std::accumulate(std::next(v.begin()), v.end(),
                                std::to_string(v[0]), // start with first element
                                dash_fold);

    // Right fold using reverse iterators
    std::string rs = accumulate(std::next(v.rbegin()), v.rend(),
                                std::to_string(v.back()), // start with last element
                                dash_fold);

    std::cout << "sum:\t\t\t\t\t" << sum << '\n'
              << "product:\t\t\t\t" << product << '\n'
              << "dash-separated string:\t\t\t" << s << '\n'
              << "dash-separated string (right-folded):\t" << rs << '\n';

    std::cout << "\nAll done...press a key to exit!\n";
    (void)_getch();
}
