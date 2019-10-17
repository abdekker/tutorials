#include <iostream>
#include <vector>
#include "configureTasks.hpp"

using namespace std;
int main(int argc, char **argv)
{
    // This tutorial is about configuring basic tasks in Visual Studio Code
    cout << "Hello from ConfigureTasks" << endl;
    cout << "Number of arguments: " << argc << endl;

    if (argc > 1)
    {
        // At least one argument...
        vector<string> args;
        std::cout << std::endl;
        for (auto arg = 0; arg < argc; arg++)
        {
            args.push_back(argv[arg]);
            std::cout << "Argument " << (arg + 1) << ": " << argv[arg] << std::endl;
        }

        // Check arguments (the launch tasks test different arguments)
        if (args[1].compare("--help") == 0)
            cout << "Help requested" << endl;
        else
            cout << "Unrecognised argument" << endl;
    }

    return 0;
}
