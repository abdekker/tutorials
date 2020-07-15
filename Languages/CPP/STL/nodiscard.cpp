#include <iostream>
// Adapted from: https://en.cppreference.com/w/cpp/language/attributes/nodiscard

/* Notes:
# Introduced in C++17
# If a function declared nodiscard (or a function returning an enumeration or class declared nodiscard by value) is
    called from a discarded-value expression other than a cast to void, the compiler is encouraged to issue a warning.
# From C++20, a string literal can be provided to explain the rationale for why the result should not be discarded
*/

// Structure marked with "[[nodiscard]]"
struct [[nodiscard]] error_info_nodiscard { };
error_info_nodiscard global_error_nodiscard;
error_info_nodiscard by_value_nodiscard() { return global_error_nodiscard; }
error_info_nodiscard& by_ref_nodiscard() { return global_error_nodiscard; }

// Structure NOT marked with "[[nodiscard]]"
struct error_info_std { };
error_info_std global_error_std;
error_info_std by_value_std() { return global_error_std; }
error_info_std& by_ref_std() { return global_error_std; }

int main()
{
    std::cout << "Demonstrates [[nodiscard]] from C++17\n";
    by_value_nodiscard();   // Compiler may warn: "discarding return value of function with 'nodiscard' attribute"
    by_ref_nodiscard();     // No warning: type is not return by value
    by_value_std();         // No warning: structure is not marked with 'nodiscard'
    by_ref_std();
    return 0;
}
