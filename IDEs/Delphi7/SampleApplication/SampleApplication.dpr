program SampleApplication;

uses
  Windows, Dialogs, Forms,
  SampleApplicationForm in 'SampleApplicationForm.pas' {frmSampleApplication},

  CoreFormClasses in '..\..\..\Languages\Delphi\Utils\CoreFormClasses.pas',
  CoreTypes in '..\..\..\Languages\Delphi\Utils\CoreTypes.pas',
  FormUtils in '..\..\..\Languages\Delphi\Utils\FormUtils.pas',
  SystemUtils in '..\..\..\Languages\Delphi\Utils\SystemUtils.pas';

{$R *.res}

var
	hMapping: HWND;

begin
	// Initialise
	Application.Initialize();

	// Set the application title
	Application.Title := 'Sample Application';

	// Before we continue, ensure that this application is not already running
	hMapping := CreateFileMapping(HWND($FFFFFFFF), nil, PAGE_READONLY, 0, 32, 'SampleApp');
	if (hMapping <> 0) then
		if (GetLastError() = ERROR_ALREADY_EXISTS) then
			begin
			// Running already, so display a message and exit...
			ShowMessage('This Delphi sample software can only run once!');
			Application.Terminate();
			Exit;
			end;

	// Pre-create the main form and run
	Application.CreateForm(TfrmSampleApplication, frmSampleApplication);
	Application.Run();

	// Free pre-created forms
	frmSampleApplication.Free();
end.
