#include <iostream>
#include <conio.h>
#include <stdio.h>
#include <string>

// This dummy file has been added to the Git repo but is intended for quick tests of C/C++ only.
// To keep the file in the index, but tell Git to start ignoring changes, use this command:
//      $ git update-index --assume-unchanged path/to/file
// To start tracking changes again, use this command:
//      $ git update-index --no-assume-unchanged path/to/file

// Adapted from: https://stackoverflow.com/questions/6964297/untrack-files-from-git-temporarily

using namespace std;
int main(int argc, char* args[])
{
    // Quick tests in C++ (do not update in Git)
    int test1 = 0x00000200;
    int test2 = (test1 >> 8);
    cout << "test >> 8 is " << test2 << endl;

    // Prompt for exit
    cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}
