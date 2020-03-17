// Sample console application for Visual Studio 2019
#include <windows.h>
#include <iostream>
#include <conio.h>
//#include <stdio.h>

// Maximum number of arguments to this console application
#define MAX_ARG 20

void ShowArguments(int argc, char *argv[])
{
	// Output the number of the arguments
    std::cout << "Number of arguments: " << argc << "\n";
	if (argc > 0)
	{
		// One or more arguments to this console application. On Windows, the 1st argument should
		// be the application name (including full path).
		BYTE	byArg = 0;
		size_t	nLength = 0;
		char	aszArguments[MAX_ARG][MAX_PATH];
		for (byArg = 0; byArg < argc; byArg++)
		{
			nLength = strlen(argv[byArg]);
			strncpy_s(aszArguments[byArg], argv[byArg], nLength);
			aszArguments[byArg][nLength] = '\0';
            std::cout << "Arg " << (byArg+1) << " = " << aszArguments[byArg] << "\n";
		}

		std::cout << "\n";
	}
}

int main(int argc, char *argv[])
{
    printf("Visual Studio 2019 sample console application...\n");
	ShowArguments(argc, argv);

	// Prompt for exit (holds application on-screen if launched outside the IDE)
	std::cout << "Finished...press a key to exit\n";
	_getch();
}
