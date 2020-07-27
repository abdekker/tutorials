#include <iostream>

// Adapted from: https://en.cppreference.com/w/cpp/language/constexpr
 
// C++11 constexpr functions use recursion rather than iteration
// (C++14 constexpr functions may use local variables and loops)
constexpr int factorial(int n)
{
    return n <= 1 ? 1 : (n * factorial(n - 1));
}
 
// Literal class
class constString
{
    const char* p;
    std::size_t sz;

public:
    template<std::size_t N>
    constexpr constString(const char(&a)[N]): p(a), sz(N - 1) {}

    // constexpr functions signal errors by throwing exceptions, in C++11 they must do so from
    // the conditional operator ?:
    constexpr char operator[](std::size_t n) const
    {
        return n < sz ? p[n] : throw std::out_of_range("");
    }
    constexpr std::size_t size() const { return sz; }
};
 
// C++11 constexpr functions had to put everything in a single return statement
// (C++14 doesn't have that requirement)
constexpr std::size_t countLower(constString s, std::size_t n = 0, std::size_t c = 0)
{
    // "n" is the position in the string, "c" is the count of lowercase letters
    return (n == s.size())
        ? c
        : ('a' <= s[n] && s[n] <= 'z')
            ? countLower(s, n + 1, c + 1)
            : countLower(s, n + 1, c);
}
 
// Output function that requires a compile-time constant, for testing
template<int n>
struct constN
{
    constN() { std::cout << n << '\n'; }
};
 
int main()
{
    // Ensure debug output goes immediately to the console
    setvbuf(stdout, NULL, _IONBF, 0);

    // Compute at compile time
    std::cout << "4! = " ;
    constN<factorial(4)> out1;  // Computed at compile time

    // Disallow optimization using volatile (forces computation at runtime)
    volatile int k = 8;
    std::cout << k << "! = " << factorial(k) << '\n';   // Computed at runtime

    // Count lowercase characters
    std::cout << "Number of lowercase letters in \"Hello, world!\" is ";
    constN<countLower("Hello, world!")> out2;   // Implicitly converted to conststr
    return 0;
}
