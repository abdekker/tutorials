unit GameMachine;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Forms, IniFiles, psAPI, SystemUtils, StrUtils, SysUtils, WinSvc,
  CoreTypes;

type
  // Game settings
  PGAME_SETTINGS = ^GAME_SETTINGS;
  GAME_SETTINGS = record
	// Grid size
	nRows, nColumns: Integer;
  end;

  // General cache
  PGAME_CACHE = ^GAME_CACHE;
  GAME_CACHE = record
	// Application path
	szAppPath, szAppINI: String;

	// Screen dimensions
	nScreenWidth, nScreenHeight: Integer;
  end;

  TGameMachine = class
  private
	{ Private declarations }
	m_bInitialised: Boolean;

	procedure Load();
	procedure Save();
	function ProgramRunningOnCD() : Boolean;

  public
	{ Public declarations }
	// Settings and cache
	GameSettings: GAME_SETTINGS;
	GameCache: GAME_CACHE;

	// System status flags
	bySystemExitRequest: BYTE;

	// Initialisation and exiting
	procedure InitSystem();
	procedure PrepareToExit();

	// Timing utility functions
	procedure Rest(dwPeriod: DWORD);
  end;

var
  ChocolateBox: TGameMachine;

implementation

uses
  ChocolateBoxMain, GameTypes;

// Private functions: Start [TGameMachine]
procedure TGameMachine.Load();
var
	pIniFile: TIniFile;
begin
	// Read settings from the INI file
	pIniFile := TIniFile.Create(GameCache.szAppINI);

	// Grid size
	GameSettings.nRows := pIniFile.ReadInteger('Game', 'Rows', GRID_SIZE_DEFAULT);
	GameSettings.nColumns := pIniFile.ReadInteger('Game', 'Columns', GRID_SIZE_DEFAULT);

	// Validate settings
	Validate_Integer(@GameSettings.nRows, 1, GRID_SIZE_MAX, GRID_SIZE_DEFAULT);
	Validate_Integer(@GameSettings.nColumns, 1, GRID_SIZE_MAX, GRID_SIZE_DEFAULT);

	// Close INI file
	pIniFile.Free();
end;

procedure TGameMachine.Save();
var
	pIniFile: TIniFile;
begin
	// Save settings to the INI file. If the program is running on a CD, do not try to save!
	if (not ProgramRunningOnCD()) then
		begin
			try
				// Open INI
				pIniFile := TIniFile.Create(GameCache.szAppINI);

				// Grid size
				pIniFile.WriteInteger('Game', 'Rows', GameSettings.nRows);
				pIniFile.WriteInteger('Game', 'Columns', GameSettings.nColumns);

				// Flush the file
				pIniFile.UpdateFile();
			finally
				// Close the file
				if (pIniFile <> nil) then
					pIniFile.Free();
			end;
		end;	// if (not ProgramRunningOnCD()) then
end;

function TGameMachine.ProgramRunningOnCD() : Boolean;
var
	strDrivesCD, strAppDrive: String;
begin
	strDrivesCD := GetSystemDrives(DRIVE_CDROM);
	strAppDrive := AnsiMidStr(ExtractFilePath(Application.ExeName), 1, 2);
	Result := AnsiContainsStr(strDrivesCD, strAppDrive);
end;
// Private functions: Eng [TGameMachine]

// Public functions: Start [TGameMachine]
procedure TGameMachine.InitSystem();
begin
	// Initialise the machine object available throughout the game
	m_bInitialised := False;

	// Settings
	ZeroMemory(@GameSettings, SizeOf(GAME_SETTINGS));

	// Cache
	ZeroMemory(@GameCache, SizeOf(GAME_CACHE));
	GameCache.szAppPath := ExtractFilePath(Application.ExeName);
	GameCache.szAppINI := (GameCache.szAppPath + 'ChocolateBox.ini');

	GameCache.nScreenWidth := Screen.Width;		// Or use "GetSystemMetrics(SM_CXSCREEN)"
	GameCache.nScreenHeight := Screen.Height;	// Or use "GetSystemMetrics(SM_CYSCREEN)"

	// System status flags
	bySystemExitRequest := 0;

	// Load settings
	Load();

	// Machine object initialisation complete!
	m_bInitialised := True;
end;

procedure TGameMachine.PrepareToExit();
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

procedure TGameMachine.Rest(dwPeriod: DWORD);
var
	dwTarget: DWORD;
begin
	// We generally use this "Rest" function instead of Sleep(...) in most places in the code, but
	// we should use Sleep(...) in threads
	dwTarget := (GetTickCount() + dwPeriod);
	repeat
		Sleep(20);
		Application.ProcessMessages();
	until (GetTickCount() > dwTarget);
end;
// Public functions: End [TGameMachine]

end.
