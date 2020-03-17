program SampleConsole;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, Dialogs,
  ConsoleUtils in '..\..\..\Languages\Delphi\Utils\ConsoleUtils.pas';

begin
	// Welcome message
	WriteLn('Delphi 7 sample console application...');
	WriteLn('');
	ShowConsoleArguments();

	// Show information about data types
	//cout << "### Integers ###\n";
	//cout << "\t\t\tSize (bytes)\tMin\t\t\tMax\n";
	//{
		// bool
	  //	bool min = false;
	   //	bool max = true;
	   //	cout << "bool\t\t\t" << sizeof(bool) << "\t\t" << min << "\t\t\t" << max << "\n";
		// Or: cout << formatter.formatString("bool\t\t\t%d\t\t%d\t\t\t%d\n", sizeof(bool), min, max);
	//}

	//{
		// char, __int8
	  //	char min = CHAR_MIN;
	  //	char max = CHAR_MAX;
	  //	unsigned char maxU = UCHAR_MAX;
	  //  cout << formatter.formatString("char (__int8)\t\t%d\t\t%d\t\t\t%d\n", sizeof(char), min, max);
	  //  cout << formatter.formatString("unsigned char\t\t%d\t\t0\t\t\t%u\n", sizeof(unsigned char), maxU);
	//}

	WriteLn('Finished...press any key to exit');
	while (not IsConsoleKeyPressed()) do;
end.
