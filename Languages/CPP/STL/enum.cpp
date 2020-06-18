#include <iostream>
#include <algorithm>
using namespace std;

// Adapted from: https://stackoverflow.com/questions/261963/how-can-i-iterate-over-an-enum
namespace MyEnum
{
    enum TypeOne
    {
        a = 100,
        b = 220,
        c = -1
    };
    static const TypeOne All[] = { a, b, c };
}

void fun(const MyEnum::TypeOne e)
{
    cout << e << std::endl;
}

int main()
{
    // Simple program showing some details of enumerations
    cout << "### Enumerations ###\n";

    // All
    cout << "\nAll values using 'for (const auto e : ARRAY)'\n";
    for (const auto e : MyEnum::All)
        fun(e);

    // All
    cout << "\nAll values using 'std::for_each(std::begin(ARRAY), std::end(ARRAY), METHOD)'\n";
    std::for_each(std::begin(MyEnum::All), std::end(MyEnum::All), fun);

    // Some
    cout << "\nSome values using 'for (const auto e : { SPECIFIED_ELEMENTS })'\n";
    for (const auto e : { MyEnum::a, MyEnum::b })
        fun(e);

    cout << "\nAll done, bye!\n";
    return 0;
}
