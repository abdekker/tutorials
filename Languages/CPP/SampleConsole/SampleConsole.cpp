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
    std::cout << __cplusplus << std::endl;
    std::cout << "  (Note: Might be 199711 even if compiled for, say, C++17. Reasons unknown.)\n";
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
