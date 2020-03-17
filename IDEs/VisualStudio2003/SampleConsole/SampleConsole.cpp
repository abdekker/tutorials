// Sample console application for Visual Studio 2003
#include <windows.h>	// Required for "BYTE", "MAX_PATH", "strlen", etc
#include <conio.h>		// Required for "_getch"
#include <stdio.h>		// Required for "printf"

// Maximum number of arguments to this console application
#define MAX_ARG 20

void ShowArguments(int argc, char *argv[])
{
	// Output the number of the arguments
	printf("Number of arguments: %d\n", argc);
	if (argc > 0)
	{
		// One or more arguments to this console application. On Windows, the 1st argument should
		// be the application name (including full path).
		size_t	nLength = 0;
		char	aszArguments[MAX_ARG][MAX_PATH];
		for (BYTE byArg = 0; byArg < argc; byArg++)
		{
			nLength = strlen(argv[byArg]);
			strncpy(aszArguments[byArg], argv[byArg], nLength);
			aszArguments[byArg][nLength] = '\0';
			printf("Arg %d = %s\n", byArg, aszArguments[byArg]);
		}

		printf("\n");
	}
}

int main(int argc, char *argv[])
{
	printf("Visual Studio 2003 sample console application...\n\n");
	ShowArguments(argc, argv);

	// Prompt for exit (holds application on-screen if launched outside the IDE)
	printf("Finished...press any key to exit\n");
	_getch();
}
