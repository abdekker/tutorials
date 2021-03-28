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

	// Show the command-line arguments. To test parameters inside the Delphi IDE:
	// * Run > Parameters > Local
	// * Type the required arguments in the "Parameters" box
	ShowConsoleArguments();

	// Exit
	WriteLn('Finished...press any key to exit');
	while (not IsConsoleKeyPressed()) do;
end.
