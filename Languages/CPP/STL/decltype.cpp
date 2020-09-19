#include <iostream>
#include "..\Utils\typeHelper.h"

// Adapted from:
// * https://en.cppreference.com/w/cpp/language/decltype
// * https://stackoverflow.com/questions/81870

// This program discusses various aspects of type information in C++, including:
// * typeid
// * decltype

// decltype was introduced in C++11. Inspects the declared type of an entity or the type and
// value category of an expression.

// decltype is useful when declaring types that are difficult or impossible to declare using
// standard notation, like lambda-related types or types that depend on template parameters.

// Basic example
struct A { double x; };
const A* a;
decltype(a->x) y;           // type of y is double (declared type)
decltype((a->x)) z = y;     // type of z is const double& (lvalue expression)

// Template example
template<typename T, typename U>
auto add(T t, U u) -> decltype(t + u) // return type depends on template parameters
                                      // return type can be deduced since C++14
{
    return t+u;
}

// Helper function to return the name of a type

/*using namespace std;
int main() 
{
    // Ensure debug output goes immediately to the console
    setvbuf(stdout, NULL, _IONBF, 0);

    // Declare a variable to investigate types
    DataTypeHelper typeHelper;
    cout << "Investigating decltype(...)\n\n";

    cout << "('i1' is a char, and `j1` is defined using `auto`)\n";
    char i1 = 98;
    auto j1 = (i1 + 2);
    cout << "  i1 = " << i1 << ", " << "j1 = " << j1 << '\n';
    cout << "  sizeof: i1 = " << sizeof(i1) << ", " << "j1 = " << sizeof(j1) << '\n';


    //if (typeHelper.isSame(decltype(i1), char))
    //bool test = is_same<decltype(i1), char>::value;
    //if (is_same<decltype(i1), char>::value)
    //    cout << "  same!\n";
    //else
    //    cout << "  not the same\n";

    cout << typeHelper.getTypeName(i1) << ", " << typeHelper.getTypeName(&j1) << endl;

    int i = 33;
    decltype(i) j = i * 2;
    std::cout << "i = " << i << ", " << "j = " << j << '\n';
 
    auto f = [](int a, int b) -> int
    {
        return a * b;
    };
 
    decltype(f) g = f; // the type of a lambda function is unique and unnamed
    i = f(2, 2);
    j = g(3, 3);
 
    std::cout << "i = " << i << ", "
              << "j = " << j << '\n';
    return 0;
}*/

template <typename T>
constexpr std::string_view name__FUNCSIG__(T)
{
    std::string_view name, prefix, suffix;
#ifdef __clang__
    name = __PRETTY_FUNCTION__;
    prefix = "std::string_view type_name() [T = ";
    //suffix = "]";
#elif defined(__GNUC__)
    name = __PRETTY_FUNCTION__;
    prefix = "constexpr std::string_view type_name() [with T = ";
    //suffix = "; std::string_view = std::basic_string_view<char>]";
#elif defined(_MSC_VER)
    name = __FUNCSIG__;
    prefix = "class std::basic_string_view<char,struct std::char_traits<char> > __cdecl ";
    //suffix = ">(void)";
#endif

    name.remove_prefix(prefix.size());
    //name.remove_suffix(suffix.size());
    return name;
}

using namespace std;
int main()
{
    // Ensure debug output goes immediately to the console
    setvbuf(stdout, NULL, _IONBF, 0);

    // Type information in C++
    cout << "Investigating type information in C++\n";
    cout << "__cplusplus = " << __cplusplus << " (C++ version)\n";
#ifdef _MSC_VER
    cout << "_MSC_VER = " << _MSC_VER << " (MSVC compiler version)\n";
#endif  // _MSC_VER

    cout << "This method signature is: " << __FUNCSIG__ << "\n\n";

    // Declare a helper to investigate types
    DataTypeHelper typeHelper;

    // Define some types and see what typeid(T) does...
    cout << "\ntypeid(T)\n";
    char c1 = 98;
    const int i1 = 31;
    int i2 = 32;
    auto i3 = 33;
    double d1 = 1.2;
    cout << "  c1  = " << typeHelper.name_typeid(c1) << endl;
    cout << "  i1  = " << typeHelper.name_typeid(i1) << endl;
    cout << "(i1 was actually declared \"const int i1 = 31\")\n";
    cout << "  i2  = " << typeHelper.name_typeid(i2) << endl;
    cout << "  i3  = " << typeHelper.name_typeid(i3) << endl;
    cout << "(i3 was declared \"auto i3 = 33\")\n";
    cout << "  &i1 = " << typeHelper.name_typeid(&i1) << endl;
    cout << "  &i2 = " << typeHelper.name_typeid(&i2) << endl;
    cout << "  d1  = " << typeHelper.name_typeid(d1) << endl;

    cout << "\ntype_name(T)\n";
    cout << "  c1  = " << name__FUNCSIG__(c1) << endl;
    cout << "  c1  = " << typeHelper.name__FUNCSIG__(c1) << endl;
    cout << "  i1  = " << typeHelper.name__FUNCSIG__(i1) << endl;
    cout << "(i1 was actually declared \"const int i1 = 31\")\n";
    cout << "  i2  = " << typeHelper.name__FUNCSIG__(i2) << endl;
    cout << "  i3  = " << typeHelper.name__FUNCSIG__(i3) << endl;
    cout << "(i3 was declared \"auto i3 = 33\")\n";
    cout << "  &i1  = " << typeHelper.name__FUNCSIG__(&i1) << endl;
    cout << "  &i2 = " << typeHelper.name__FUNCSIG__(&i2) << endl;
    cout << "  d1  = " << typeHelper.name__FUNCSIG__(d1) << endl;

    return 0;
}
