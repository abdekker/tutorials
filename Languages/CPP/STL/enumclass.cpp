#include <iostream> 
using namespace std; 

// Adapted from:
//  https://www.geeksforgeeks.org/enum-classes-in-c-and-their-advantage-over-enum-datatype/

// The "enum class" feature was added in C++11
using namespace std;
int main() 
{
    // Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);
    cout << "Demonstrating enum class from C++11\n\n";

    // "Old-style" enumerations
    cout << "'Old-style' enumerations are not type-safe\n";
    enum Gender {
        Male,
        Female };
    enum Animal {
        Cat,
        Dog };

    // An enum values cannot be used to name variables
        //int Cat = 10;   // error C2365: 'main::Cat': redefinition

    // Create type variables
    Gender person = Male;
    Animal pet = Cat;
    cout << "  Person is " << person << " (Male)\n";
    cout << "  Pet is " << pet << " (Cat)\n";
    if (person == pet)
        cout << "  Values are equal\n";
    else
        cout << "  Values are not equal\n";

    cout << "    Note: The values are the same, but the variables are different\n\n";

    // "New-style" C++11 enum class
    cout << "'New-style' C++11 enum class enumerations are type-safe\n";
    enum class Color1 {
        Red,
        Green,
        Blue };
    enum class Color2 {
        // With standard enum, this would generates a "redeclaration" compiler error for "Red"
        Red,
        Black,
        White };
    enum class People {
        Good,
        Bad };

    // An enum value can now be used to name variables (this of doubtful value, of course!)
    int Green = 10;
  
    // Instantiating the enum class
    Color1 x1 = Color1::Green;
    Color2 x2 = Color2::Black;
    People p = People::Good;

    // There is no implicit conversion of enum class to int (explicit conversions are fine)
        //int myColour = x1;                            // error C2440: cannot convert from 'main::Color1' to 'int'
        //cout << "  x1 is " << x1 << " (Green)\n";     // error C2679
    cout << "  x1 is " << int(x1) << " (Color1::Green)\n";
    cout << "  x2 is " << int(x2) << " (Color2::Black)\n";
    cout << "  p is " << int(p) << " (People::Good)\n";

    cout << "\nComparison is now completely type-safe\n";
    if (x1 == Color1::Red) 
        cout << "    x1 is Red\n";
    else
        cout << "    x1 is not Red\n";

    if (p == People::Bad) 
        cout << "    p is bad :o(\n";
    else
        cout << "    p is good! :o)\n";

    cout << "\n  The following would give a compiler error:\n";
    cout << "    if (x1 == x2) ...\n";

    cout << "\nAll done!\n";
    return 0; 
} 
