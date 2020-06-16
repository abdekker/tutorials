program MemoryGame;

{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}
{$R 'resources\bitmaps.res' 'resources\bitmaps.rc'}

{ This program uses these C++ DLLs:
* ..\..\..\Languages\CPP\Utils\Imaging
* ..\..\..\Languages\CPP\Utils\JPGTools

These DLLs may need to be copied manually to this folder. Conditional definitions from
CoreOptions.inc alter the way this program functions. }

uses
  Windows,
  Forms,
  Dialogs,

  // Core Delphi tools
  CoreFormClasses in '..\..\..\Languages\Delphi\Utils\CoreFormClasses.pas',
  CoreTypes in '..\..\..\Languages\Delphi\Utils\CoreTypes.pas',
  FormUtils in '..\..\..\Languages\Delphi\Utils\FormUtils.pas',
  SystemUtils in '..\..\..\Languages\Delphi\Utils\SystemUtils.pas',

  // Core C++ tools
  Imaging in '..\..\..\Languages\CPP\Utils\Imaging\source\Imaging.pas',
  JPGTools in '..\..\..\Languages\CPP\Utils\JPGTools\JPGTools.pas',

  // This project...
  Main in 'source\Main.pas' {frmMain},
  MemoryGameTypes in 'source\MemoryGameTypes.pas',
  Disclaimer in 'source\Disclaimer.pas' {frmDisclaimer},
  Machine in 'source\Machine.pas',
  Settings in 'source\Settings.pas' {frmSettings};

{$R *.res}

begin
	// Initialise
	Application.Initialize();

	// Create the main and run the application
	Application.CreateForm(TfrmMain, frmMain);
	Application.Run();
end.
