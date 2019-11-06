#include "configureTasks.hpp"
#include <iostream>
#include <vector>

using namespace std;
int main(int argc, char **argv) {
  // This tutorial is about configuring basic tasks in Visual Studio Code
  cout << "Hello from ConfigureTasks" << endl;
  cout << "Number of arguments: " << argc << endl;

  if (argc > 1) {
    // At least one argument...
    vector<string> args;
    cout << endl;

    // Set up known arguments
    vector<string> argsKnown;
    argsKnown.push_back("--help");
    argsKnown.push_back("--version");
    argsKnown.push_back("--test");
    bool isKnown = false;

    // Process arguments
    for (auto arg = 0; arg < argc; arg++) {
      args.push_back(argv[arg]);
      isKnown = false;
      for (auto known = argsKnown.begin(); known != argsKnown.end(); known++) {
        if (*known == argv[arg]) {
          isKnown = true;
        }
      }

      cout << "Argument " << (arg + 1) << ": " << argv[arg];
      if (isKnown)
        cout << " (known)" << endl;
      else
        cout << " (unknown!)" << endl;
    }

    // Check arguments; see launch.json which tests different arguments
    cout << endl;
    if (args[1].compare("--help") == 0)
      cout << "  Help requested" << endl;
    else
      cout << "  Argument 1 was unrecognised" << endl;
  }

  return 0;
}
