#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>

/* Sample program which can be compiled from the command-line.

    - Windows (Visual Studio): cl /EHsc /MD /O2 HelloWorld.cpp /link /out:hw.exe
        - More generally: cl [options] FILE1 FILE2 ... /link /out:OUTPUT
        - Flags:
            - /EHsc => Standard C++ stack unwinding, assume functions marked as extern "C" never throw a C++ exception
            - /MD => Dynamic-linking with the release version of the Windows runtime library
                - Use "/MT" for static-linking with the release runtime
                - Use "/MDd" for dynamic-linking with the debug runtime (and define _DEBUG)
                - Use "/MTd" for static-linking with the debug runtime (and define _DEBUG)
            - /O2 => Maximise optimization for code speed
    
    - Windows (Visual Studio Code):
        - Ensure the Visual Studio Command Prompt is run before running Visual Studio Code
        - See <PEN_DRIVE>\Work\Utilities\Microsoft\VS_Code\Notes-VS-Code-Builds.txt for details
        - Add following to c_cpp_properties.json
            {
                "configurations": [
                    {
                        "name": "Win32",
                        "includePath": [
                            "${workspaceFolder}/**",
                            "${INCLUDE}"
                        ],
                        "defines": [
                            "_DEBUG",
                            "UNICODE",
                            "_UNICODE"
                        ],
                        "windowsSdkVersion": "10.0.18362.0",
                        "compilerPath": "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/MSVC/14.26.28801/bin/Hostx64/x64/cl.exe"
                    }
                ],
                "version": 4
            }
        - Add following to tasks.json:
            {
                // Using the MSVC compiler cl.exe (eg. Visual Studio 2019) on Windows
                // Note: Run the Visual Studio Command Prompt before launching Visual Studio Code
                "type": "shell",
                "label": "cl.exe: Build active file (DEBUG)",
                "command": "cl.exe",
                "args": [
                    "/Zi",
                    "/EHsc",
                    "/Fe:",
                    "${fileDirname}\\${fileBasenameNoExtension}.exe",
                    "${file}"
                ],
                "options": {
                    "cwd": "${workspaceFolder}"
                },
                "problemMatcher": [
                    "$msCompile"
                ],
                "group": {
                    "kind": "build",
                    "isDefault": true
                }
            },
            {
                // Using the MSVC compiler cl.exe (eg. Visual Studio 2019) on Windows
                // Note: Run the Visual Studio Command Prompt before launching Visual Studio Code
                "type": "shell",
                "label": "cl.exe: Build active file (RELEASE)",
                "command": "cl.exe",
                "args": [
                    "/EHsc",
                    "/MD",
                    "/O2",
                    "${file}",
                    "/link",
                    "/out:${fileDirname}\\${fileBasenameNoExtension}.exe"
                ],
                "options": {
                    "cwd": "${workspaceFolder}"
                },
                "problemMatcher": [
                    "$msCompile"
                ]
            }
        - Add following to launch.json:
            {
                // Debug using cl on Windows
                "name": "cl.exe: Build and debug active file",
                "type": "cppvsdbg",
                "request": "launch",
                "program": "${fileDirname}\\${fileBasenameNoExtension}.exe",
                "args": [],
                "stopAtEntry": false,
                "cwd": "${workspaceFolder}",
                "environment": [],
                "externalConsole": false,
                "preLaunchTask": "cl.exe: Build active file (DEBUG)"
            }
        - Start debugging (F5)

    - Linux Ubuntu (gcc): gcc -O3 -g0 HelloWorld.cpp -o hw.out
        - More generally "gcc [options] FILE1 FILE2 ... -o OUTPUT"
        - Flags:
            - O3 => Maximise optimization for code size and speed
            - g0 => No debug information
*/

using namespace std;
int main(int argc, char* args[])
{
    // Ensure the output buffer is flushed on each insertion operation (either line below achieve this)
    setvbuf(stdout, NULL, _IONBF, 0);
    //std::cout << std::unitbuf;

    // Version 1 (using simple printf)
    printf("Hello World 1! (using printf)\n");

    // Version 2 (using a vector of std::string)
    vector<string> msg {"Hello", "World", "2!", "(using", "a", "vector)"};
    for (const string& word : msg)
    {
        cout << word << " ";
    }
    cout << endl;

    // End of program
    return 0;
}
