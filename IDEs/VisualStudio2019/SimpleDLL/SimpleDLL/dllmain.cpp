// dllmain.cpp : Defines the entry point for the DLL application.
#include "pch.h"

// This sample DLL project was based on this tutorial:
// https://docs.microsoft.com/en-us/cpp/build/walkthrough-creating-and-using-a-dynamic-link-library-cpp?view=vs-2019

/* Basic steps:
    - Create new project > Dynamic-Link Library (DLL)
        - Choose the path and name (eg. %USERPROFILE%\Projects\tutorials\IDEs\VisualStudio2019\SimpleDLL)
        - Let Visual Studio create the project
        - If building x86 binaries only
            - Close Visual Studio
            - Edit the PROJ-DLL.sln and .vcxproj files and remove x64 references
            - Re-open project
        - Add a new C++ header file
        - Project > Add new item > Header File (.h)
        - Give the file the name PROJ-DLL.h (eg. SimpleDLL.h)
        - Contents provided by tutorial
        - ...but set the appropriate name for the PROJECT_EXPORTS and PROJECT_API macros
        - See Project > Properties > C/C++ > Preprocessor
        - Add a new C++ source file
            - Right-click Solution Explorer > Source Files > Add > New Item > C++ File (.cpp)
            - Give the file the name PROJ-DLL.cpp (eg. SimpleDLL.cpp)
            - Contents provided by tutorial
    - Create new project > Console App
        - Choose the path and name (eg. %USERPROFILE%\Projects\tutorials\IDEs\VisualStudio2019\SimpleDllClient)
        - Let Visual Studio create the project
        - If building x86 binaries only, see the instructions above
        - Open Project properties > C/C++ > General > Additional Include Directories
            - Add the path to PROJ-DLL.h (absolute or relative)
                - Example: "..\..\SimpleDLL\SimpleDLL"
        - Contents provided by tutorial
        - Open Project properties
            - Linker > Input and add SimpleDLL.lib
            - Linker > General > Additional Library Directories and add "..\..\PROJ-DLL\$(IntDir)"
                - Example: "..\..\SimpleDLL\$(IntDir)"
        - Note: You can also solve the problem of finding the PROJ-DLL.dll by using a Post-Build Event
*/

BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved )
{
	// Debugging:
	// * With "SimpleDllClient" written in VS2019
	//		- Project Properties > Debugging
	//		- Command = "..\SimpleDllClient.exe" (Working Directory = "$(ProjectDir)")
	// * With "DelphiClient.exe" written in Delphi7
	//		- Project Properties > Debugging
	//		- Command = "..\DelphiClient.exe"

	// Set a breakpoint on the "switch" line and debug (see above). The entry point is here with
    // "ul_reason_for_call" == 1 (DLL_PROCESS_ATTACH). When the client application exits,
    // "ul_reason_for_call" == 0 (DLL_PROCESS_DETACH).
    switch (ul_reason_for_call)
    {
        case DLL_PROCESS_ATTACH:
        case DLL_THREAD_ATTACH:
        case DLL_THREAD_DETACH:
        case DLL_PROCESS_DETACH:
            break;
    }
    return TRUE;
}

