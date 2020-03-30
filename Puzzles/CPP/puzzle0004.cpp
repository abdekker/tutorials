#include "stdafx.h"
#include "puzzlePrototypes.h"

// Print "Hello World" without using a semicolon (;)

// Source: Papyrus coding test

/* Commentary:
Is that anywhere in the program??
It is assumed that this is not allowed:
	void main()
	{
		std::cout << "Hello world";    // Semicolon! Leave it off and you get compile errors
	}
This is somewhat obscure, but the prototype of "printf" is:
	int printf ( const char * format, ... );
From http://www.cplusplus.com/reference/cstdio/printf/
So we could put the statement in an "if" or "while" conditional statement...
*/

void RunPuzzle0004()
{
    if (printf("Hello World"))
    {
        // Deliberately empty so we don't use a semicolon (this could be on one line)
    }
	// This appears to be a cheat...does this satisfy the test?
}
