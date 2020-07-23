program ChocolateBox;

uses
  Windows,
  Dialogs,
  Forms,

  // Core Delphi tools
  CoreFormClasses in '..\..\..\Languages\Delphi\Utils\CoreFormClasses.pas',
  CoreTypes in '..\..\..\Languages\Delphi\Utils\CoreTypes.pas',
  FormUtils in '..\..\..\Languages\Delphi\Utils\FormUtils.pas',
  SystemUtils in '..\..\..\Languages\Delphi\Utils\SystemUtils.pas',

  // This project...
  ChocolateBoxMain in 'source\ChocolateBoxMain.pas' {frmChocolateBox},
  GameAbout  in 'source\GameAbout.pas' {frmGameAbout},
  GameMachine in 'source\GameMachine.pas',
  GameTypes in 'source\GameTypes.pas',
  SettingsAdvanced in 'source\SettingsAdvanced.pas', {frmSettingsAdvanced}
  SettingsMedia in 'source\SettingsMedia.pas', {frmSettingsMedia}
  SettingsUserInterface in 'source\SettingsUserInterface.pas' {frmSettingsUserInterface};

{$R *.res}

var
	hMapping: HWND;

begin
	// Initialise
	Application.Initialize();

	// Set the application title
	Application.Title := 'Chocolate Box';

	// Before we continue, ensure that this application is not already running
	hMapping := CreateFileMapping(HWND($FFFFFFFF), nil, PAGE_READONLY, 0, 32, 'ChocolateBox');
	if (hMapping <> 0) then
		if (GetLastError() = ERROR_ALREADY_EXISTS) then
			begin
			// Running already, so display a message and exit...
			ShowMessage('This application should only run once!');
			Application.Terminate();
			Exit;
			end;

	// Pre-create the main form and run
	Application.CreateForm(TfrmChocolateBox, frmChocolateBox);
	Application.Run();

	// Free pre-created forms
	frmChocolateBox.Free();
end.
