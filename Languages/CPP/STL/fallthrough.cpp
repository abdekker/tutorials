#include <iostream>
// Adapted from: https://en.cppreference.com/w/cpp/language/attributes/fallthrough

/* Notes:
# Introduced in C++17
# Indicates that the fall-through from the previous case label is intentional and should not be diagnosed
    by a compiler that warns on fallthrough
# In older C++ code, comments such as "// Deliberate fall-through" would serve the same purpose

# If the C++17 standard is not specified:
    "-std=c++17" for gcc
    "/std:c++17" for cl
  then the compiler will generate the warning:
    warning C5051: attribute 'fallthrough' requires at least '/std:c++17'; ignored

# If C++17 is specified, the compiler warning for those lines marked as "ill-formed" is:
    warning C4468: 'fallthrough': attribute must be followed by a case label or a default label
*/

void a() {}
void b() {}
void c() {}

void testFallthrough(int n) {
    switch (n) {
        case 1:
        case 2:
            a();
            [[fallthrough]];
        case 3:     // No warning on fallthrough
            b();
        case 4:     // Compiler may warn on fallthrough (cle.exe does not generate a warning
            if (n < 3) {
                c();
                [[fallthrough]]; // OK (cl.exe warning C4468)
            }
            else {
                return;
            }
        case 5:
            while (false) {
                // ill-formed: next statement is not part of the same iteration (cl.exe does not generate a warning)
                [[fallthrough]];
            }
        case 6:
            [[fallthrough]]; // ill-formed, no subsequent case or default label (cl.exe warning C4468)
    }
}

int main()
{
    // Test the C++17 "[[fallthrough]];" attribute
    for (int test=1; test < 10; test++)
        testFallthrough(test);

    return 0;
}
