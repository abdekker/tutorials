#include <stdio.h>

// Sample program to demonstrate Doxygen comments. Comments can be added manually or using extensions:

// * Visual Studio Code
//      - Extensions > Doxygen Documentation Generator (since ~2018, these notes based on 0.6.0 from ~March 2020)
//      - Settings
//          - Doxdocgen › C: Comment Prefix = "/// "
//          - Doxdocgen › C: First Line = "" (blank)
//          - Doxdocgen › C: Last Line = "" (blank)
//          - Doxdocgen › C: Trigger Sequence = "///"
//  - To trigger, put the cursor immediately above a function and type "///" (followed by <RETURN>)

// * Visual Studio 2019 Community (since April 2020)
//      - Tools > Options > C/C++ > CodeStyle > General
//      - Set "Generated documentation comments style" to "Doxygen (///)"
//  - To trigger, put the cursor immediately above a function and type "///" (no <RETURN> required)

// Doxygen comments in this code have annotations to identify how they were generated. "blahX" was added manually.
// * (VSC)      = Visual Studio Code
// * (VS2019)   = Visual Studio 2019 (in general more terse than VS Code)
// Each IDE show these comments in a popup for Intellisense when you hold your mouse over the function name. The
// exact formatting differs with each application. Note that comments blocks with std C++ "//" or C "/**/" will
// also appear in the Intellisense popup.

// * There are alternatives for the formatting of the comments (eg. "/**" or "/*!"). See:
//      https://www.doxygen.nl/manual/docblocks.html
// * The sections also have alternatives. For example, "\brief" and "@brief" are equivalent.

// (VSC)
/// @brief blahBrief
///

// (VS2019)
/// @brief 
void VoidNoParams() { }

// (VSC)
/// @brief blahBrief
/// 
/// @param param1 blahParam1
/// @param param2 blahParam2

// (VS2019)
/// @brief blahBrief
/// @param param1 blahParam1
/// @param param2 blahParam2
void VoidTwoParams(bool param1, char* param2) { }

// (VSC)
/// @brief blahBrief
/// 
/// @return int blahReturn

// (VS2019)
/// @brief blahBrief
/// @return blahReturn
int IntNoParams() { return 1; }

// (VSC)
/// @brief blahBrief
/// 
/// @param param1 blahParam1
/// @param param2 blahParam2
/// @return int blahReturn

// (VS2019)
/// @brief blahBrief
/// @param param1 blahParam1
/// @param param2 blahParam2
/// @return blahReturn
int IntTwoParams(bool param1, char* param2) { return 2; }

// (VSC)
/// @brief This is the main function
/// 
/// @param argc Number of arguments
/// @param args Command-line parameters
/// @return int The error code (if any)

// (VS2019)
/// @brief This is the main function
/// @param argc Number of arguments
/// @param args Command-line parameters
/// @return The error code (if any)
int main(int argc, char* args[])
{
    printf("This program demonstrates Doxygen comments and formatting (and does nothing!)\n");
    return 0;
}
