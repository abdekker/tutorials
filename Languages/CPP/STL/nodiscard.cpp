#include <iostream>
// Adapted from: https://en.cppreference.com/w/cpp/language/attributes/nodiscard

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
