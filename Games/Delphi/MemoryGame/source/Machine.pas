unit Machine;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, CoreTypes, StdCtrls, SysUtils, Forms, IniFiles, SystemUtils, Imaging,
  WinSvc, psAPI, Graphics, StrUtils;

type
  // Image source / type
  IMAGE_SOURCE =
  (
	eInternal = 0,
	eNumbers,
	eLetters,
	eFolder
  );

  // Game settings
  PGAME_SETTINGS = ^GAME_SETTINGS;
  GAME_SETTINGS = record
	// Image source
	eSource: IMAGE_SOURCE;
	strSourceFolder: String;

	// Graphics
	clGridColour: TColor;
	nGridWidth: Integer;

	// Grid size
	nRows, nColumns: Integer;
  end;

  TSystem = class
  private
	{ Private declarations }
	m_dwAppStartTicks: DWORD;

	procedure Load();
	procedure Save();
	function ProgramRunningOnCD() : Boolean;

  public
	{ Public declarations }
	// Settings
	GameSettings: GAME_SETTINGS;

	// System status flags
	bySystemExitRequest: BYTE;

	// Initialisation and exiting
	procedure InitSystem();
	procedure PrepareToExit();

	// Imaging Utility functions
	function GetSourceFolder(settings: GAME_SETTINGS) : String;
	procedure SaveBitmapToUSB(strTitle, strFilename: String; bmSaveMe: TBitMap);

	// Timing utility functions
	procedure Rest(dwPeriod: DWORD);
  end;

var
  MemoryGame: TSystem;

implementation

uses
  Main, MemoryGameTypes;

// Private functions: Start [TSystem]
procedure TSystem.Load();
var
	pIniFile: TIniFile;
begin
	// Read settings from the INI file
	pIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'MemoryGame.ini');

	// Image source
	GameSettings.eSource := IMAGE_SOURCE(pIniFile.ReadInteger('Images', 'Location', BYTE(eInternal)));
	GameSettings.strSourceFolder := pIniFile.ReadString('Images', 'SourceFolder',
		(ExtractFilePath(Application.ExeName) + 'Internal sample pics\Food Images'));

	// Graphics
	GameSettings.clGridColour := pIniFile.ReadInteger('Graphics', 'GridColour', clRed);
	GameSettings.nGridWidth := pIniFile.ReadInteger('Graphics', 'GridWidth', 2);

	// Grid size
	GameSettings.nRows := pIniFile.ReadInteger('Game', 'Rows', 4);
	GameSettings.nColumns := pIniFile.ReadInteger('Game', 'Columns', 4);

	// Validate settings
	Validate_BYTE(@GameSettings.eSource, BYTE(eInternal), BYTE(eFolder), BYTE(eInternal));
	Validate_Integer(@GameSettings.nGridWidth, 2, GRID_WIDTH_MAX, 2);
	if (Odd(GameSettings.nGridWidth)) then
		GameSettings.nGridWidth := 2;

	Validate_Integer(@GameSettings.nRows, 2, GRID_WIDTH_MAX, 4);
	Validate_Integer(@GameSettings.nColumns, 2, GRID_WIDTH_MAX, 4);

	// Close INI file
	pIniFile.Free();
end;

procedure TSystem.Save();
var
	pIniFile: TIniFile;
begin
	// Save settings to the INI file
	// Note: This can fail if the INI file is read-only, such as on a CD drive.
	// We could try and remove the read-only attribute, but its just simpler to
	// not bother saving at all. The code below code tries to remove the
	// read-only (and other system flags) attributes:
	// RemoveAttributes(strFile, (faReadOnly or faHidden or faSysFile));

	// If the program is running on a CD, do not try to save)
	if (not ProgramRunningOnCD()) then
		begin
		try
			try
				pIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'MemoryGame.ini');

				// Image source
				pIniFile.WriteInteger('Images', 'Location', BYTE(GameSettings.eSource));
				pIniFile.WriteString('Images', 'SourceFolder', GameSettings.strSourceFolder);

				// Graphics
				pIniFile.WriteInteger('Graphics', 'GridColour', GameSettings.clGridColour);
				pIniFile.WriteInteger('Graphics', 'GridWidth', GameSettings.nGridWidth);

				// Grid size
				pIniFile.WriteInteger('Game', 'Rows', GameSettings.nRows);
				pIniFile.WriteInteger('Game', 'Columns', GameSettings.nColumns);

				// Flush the file
				pIniFile.UpdateFile();
			except
			end;
		finally
		// Close the file
		if (pIniFile <> nil) then
			pIniFile.Free();
		end;
		end;	// if (not ProgramRunningOnCD()) then
end;

function TSystem.ProgramRunningOnCD() : Boolean;
var
	strDrivesCD, strAppDrive: String;
begin
	strDrivesCD := GetSystemDrives(DRIVE_CDROM);
	strAppDrive := AnsiMidStr(ExtractFilePath(Application.ExeName), 1, 2);
	Result := AnsiContainsStr(strDrivesCD, strAppDrive);
end;
// Private functions: Eng [TSystem]

// Public functions: Start [TSystem]
procedure TSystem.InitSystem();
begin
	// General intialisation
	m_dwAppStartTicks := GetTickCount();

	// System status flags
	bySystemExitRequest := 0;

	// Load settings
	Load();

	// Machine object initialisation complete!
end;

procedure TSystem.PrepareToExit();
begin
	// Force the system to exit elegantly
	if (bySystemExitRequest < 2) then
		Inc(bySystemExitRequest);

	if (bySystemExitRequest = 2) then
		Exit;

	// Save settings
	Save();
	Rest(200);
end;

function TSystem.GetSourceFolder(settings: GAME_SETTINGS) : String;
var
	strFolder: String;
begin
	// Return the path to the source files
	if (settings.eSource = eInternal) then
		strFolder := (ExtractFilePath(Application.ExeName) + 'Internal Sample Pics\General Images')
	else if (settings.eSource = eNumbers) then
		strFolder := (ExtractFilePath(Application.ExeName) + 'Internal Sample Pics\Numbers')
	else if (settings.eSource = eLetters) then
		strFolder := (ExtractFilePath(Application.ExeName) + 'Internal Sample Pics\Letters')
	else
		strFolder := settings.strSourceFolder;

	Result := strFolder;
end;

procedure TSystem.SaveBitmapToUSB(strTitle, strFilename: String; bmSaveMe: TBitMap);
var
	strFile: String;
begin
	strFile := GetNextFilename('C:\', strFilename, 'bmp');
	bmSaveMe.SaveToFile(strFile);
end;

procedure TSystem.Rest(dwPeriod: DWORD);
var
	dwTarget: DWORD;
begin
	// We generally use this "Rest" function instead of Sleep(...) in most
	// places in the code, but we should use Sleep(...) in the Threads module.
	dwTarget := (GetTickCount() + dwPeriod);
	repeat
		Sleep(20);
		Application.ProcessMessages();
	until (GetTickCount() > dwTarget);
end;
// Public functions: End [TSystem]

end.
