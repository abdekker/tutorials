#include <iostream>
#include <vector>
#include "mainArguments.hpp"

using namespace std;
int main(int argc, char **argv)
{
    // This application shows an example of how to process arguments to "main"
    cout << "Test arguments to main function\n\n";
    cout << "Number of arguments: " << argc << endl;

    if (argc > 1)
    {
        // At least one argument...
        vector<string> args;
        for (auto arg = 0; arg < argc; arg++)
        {
            args.push_back(argv[arg]);
            cout << "Argument " << (arg + 1) << ": " << argv[arg] << endl;
        }

        // Check arguments
        cout << endl;
        cout << "Checking arguments..." << endl;
        if (args[1].compare("--help") == 0)
            ShowHelp();
        else
            cout << "Unrecognised argument(s)" << endl;
    }

    return 0;
}

void ShowHelp()
{
    cout << "Help requested!" << endl;
}
