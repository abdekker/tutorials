#include <iostream>
#include <conio.h>
#include <stdio.h>
#include <string>

// This dummy file has been added to the Git repo but is intended for quick tests of C/C++ only.

/* To keep this file in the index, but tell Git to start ignoring changes, there are two commands:
    $ git update-index --assume-unchanged QuickTests.cpp (or)
    $ git update-index --skip-worktree QuickTests.cpp

These two commands work similarly but have different use-cases.
* assume-unchanged is designed for cases where it is expensive to check whether the file has been
    modified. This flag assumes the developer shouldn't change the file, and is intended primarily
    for improving performance (eg. for not-changing folders like SDKs).
* skip-worktree instructs Git not to touch the file and is intended for situations where the
    developers should change, or are expected to change, the file

Generally, "skip-worktree" is the option you should use.

To start tracking changes again, use the appropriate (anti-)command:
    $ git update-index --no-assume-unchanged QuickTests.cpp (or)
    $ git update-index --no-skip-worktree QuickTests.cpp

Adapted from:
- https://stackoverflow.com/questions/6964297/
- https://stackoverflow.com/questions/13630849/
- https://git-scm.com/docs/git-update-index */

using namespace std;
int main(int argc, char* args[])
{
    // Quick tests in C/C++ (do not update in Git)
    int test1 = 0x00000200;
    int test2 = (test1 >> 8);
    cout << "test >> 8 is " << test2 << endl;

    // Prompt for exit
    cout << "\nFinished...press a key to exit\n";
    (void) _getch();
    return 0;
}
