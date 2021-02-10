// Sample C++ console application for Windows, createed in Visual Studio
// Note: Use this in preference to IDE-specific sample console applications in <tutorials>\IDEs
//      For example: IDEs\VisualStudio2019\SampleConsole

#include <iostream>
#include <conio.h>

void ShowCppStandard()
{
    std::cout << "### C++ standard which compiled this application ###\n";
    // 199711 for C++98,
    // 201103 for C++11
    // 201402 for C++14
    // 201703 for C++17
    std::cout << "__cplusplus = " << __cplusplus << ", _MSC_VER = " << _MSC_VER << std::endl;
    std::cout << "  (Note: Will be \"199711\" if the project is not compiled with the \"/Zc:__cplusplus\" flag)\n";
    // To add this flag:
    // * Project Properties > C/C++ > Command-Line
    // * Manually add "/Zc:__cplusplus" to the Additional Options field and click Apply

    // Flag has been added to the DEBUG build only to show the difference. Caution! It is risky relying on
    // this macro to decide whether the compiler has support for a particular feature. The most reliable way
    // to determine whether a compiler has support for a specific feature is to test.
    std::cout << "#\n\n";
}

int main()
{
    printf("C++ sample console application\n\n");
    ShowCppStandard();

    // Prompt for exit (holds application on-screen if launched outside the IDE)
    std::cout << "Finished...press a key to exit\n";
    _getch();
}
