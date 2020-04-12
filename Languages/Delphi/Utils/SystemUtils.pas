{ General system utilities designed for Windows applications }
unit SystemUtils;

interface

uses
  Windows, Classes, StdCtrls;

const
  // Windows constants for drives
  DRIVE_UNKNOWN			= 0;
  DRIVE_NO_ROOT_DIR		= 1;
  DRIVE_REMOVABLE		= 2;
  DRIVE_FIXED			= 3;
  DRIVE_REMOTE			= 4;
  DRIVE_CDROM			= 5;
  DRIVE_RAMDISK			= 6;
  DRIVE_ALL_TYPES		= 99;	// Non-Windows constant for "All of the above"

type
  PDummyInterfacePointer = ^Integer;

// Public methods

// Windows
function IsWindows64Bit() : Boolean;
function IsWindows10() : Boolean;
procedure SetSystem32Path(var strSys32: String; bRedirect: Boolean);
function ExpandEnvironment(const cstrValue: String): String;
function IsProcessRunning(strProcessName: String) : Boolean;
function GetProcessThreadCount(dwProcessID: DWORD) : Integer;
function GetSystemThreadCount() : Integer;
function IsThreadRunning(dwThreadID: DWORD) : Boolean;
function FindWindowByTitle(hStartHandle: HWND; strWindowTitle: string) : HWND;
procedure GetDiskSpaceGB(const strDrive: String; var fTotalGB: Single; var fTotalFreeGB: Single);
function GetDiskFileSystem(const cstrDrive: String) : String;
function GetSystemDrives(cdwDrives: DWORD = DRIVE_ALL_TYPES) : String;
procedure SaveToClipboard(const cstrText: String);

// Registry
function RegGetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	var pValue: Pointer; var dwValSize: Cardinal): Boolean;
function RegSetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	pValue: Pointer; dwValueSize: Cardinal): Boolean;
function RegKeyExists(hRootKey: HKEY; const cstrName: String): Boolean;
function RegValueExists(hRootKey: HKEY; const cstrName: String): Boolean;
procedure RegEnumSubKeys(hRootKey: HKEY; const cstrName: String; astrKeys: TStringList);
procedure RegEnumSubValues(hRootKey: HKEY; const cstrName: String; astrValues: TStringList);
procedure DumpRegKeyValues(var fp: TextFile; hRootKey: HKEY; const cstrKey: String);
procedure DumpRegValue(var fp: TextFile; hRootKey: HKEY; const cstrName: String; dwValType: Cardinal);
function RegisterOCX(strDLL: String; bSilent: Boolean = False) : Boolean;

// File utilities
function FileHasData(const cstrFile: String) : Boolean;
procedure GetFolderListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
function DeleteFolder(strFolder: String) : Boolean;
function IsFolderWriteable(const cstrPath: String) : Boolean;
procedure CopyFilesBetweenFolders(strSourceFolder, strTargetFolder, strWildcard: String);
function GetSizeOfFile(strFilename: String) : DWORD;
procedure ChangeFilename(strOldPath, strNewPath: String);

// System
function TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;

implementation

uses
  Clipbrd, Registry, StrUtils, SysUtils, TLHelp32;

const
  // Disk sizes / capacities
  KILO_BYTE		= Int64(1024);
  MEGA_BYTE		= Int64(1024 * KILO_BYTE);
  GIGA_BYTE		= Int64(1024 * MEGA_BYTE);
  TERA_BYTE		= Int64(1024 * GIGA_BYTE);

// Start: Public methods
// Windows
function IsWindows64Bit() : Boolean;
type
	TWinIsWow64 = function(hHandle: THandle; var bReturn: Windows.BOOL): Windows.BOOL; stdcall;
var
	funcWinIsWow64: TWinIsWow64;
	bIs64Bit: Windows.BOOL;
begin
	// Determine whether the Windows operating system is 64-bit and therefore this process is
	// running under WOW64. For additional details see the following:
	// * https://msdn.microsoft.com/en-us/library/windows/desktop/ms684139.aspx
	// * https://msdn.microsoft.com/en-us/library/windows/desktop/aa384249.aspx
	// * https://stackoverflow.com/questions/1436185/how-can-i-tell-if-im-running-on-x64

	// Note to developer: Delphi 7 produces 32-bit processes only so calls to the 64-bit "System32"
	// folder are silently redirected to the 32-bit "SysWOW64" folder instead. The ability to
	// produce 64-bit processes was added in Delphi XE2 (introduced in 2011).
	Result := False;
	funcWinIsWow64 := GetProcAddress(GetModuleHandle('kernel32.dll'), 'IsWow64Process');
	if (Assigned(funcWinIsWow64)) then
		begin
		if (funcWinIsWow64(GetCurrentProcess(), bIs64Bit)) then
			Result := bIs64Bit;
		end;
end;

function IsWindows10() : Boolean;
var
	osVersionInfo: TOSVERSIONINFO;
begin
	{Following taken from MSDN (and duplicates removed)
		Windows 10						10.0	***
		Windows Server 2016				10.0	***
		Windows 8.1						6.3		***
		Windows Server 2012 R2			6.3		***
		Windows 8						6.2
		Windows 7						6.1
		Windows Server 2008 R2			6.1
		Windows Server 2008				6.0
		Windows Vista					6.0
		Windows Server 2003				5.2
		Windows XP 64-Bit				5.2
		Windows Embedded Std 2009		5.1 [Not stated explicitly on MSDN, from testing]
		Windows XP						5.1
		Windows 2000					5.0
		Windows NT 4.0					4.0
		Windows Me						4.90
		Windows 98						4.10
		Windows 95						4.0

		*** The application manifest must specifically target Windows 8.1 or 10, otherwise the
		version info for Windows 8 (6.2) will be returned. Since we cannot generate the proper
		manifest in Delphi 7, only 6.2 is returned. }
	ZeroMemory(@osVersionInfo, SizeOf(osVersionInfo));
	osVersionInfo.dwOSVersionInfoSize := SizeOf(osVersionInfo);
	GetVersionEx(osVersionInfo);
	Result := (
		((osVersionInfo.dwMajorVersion = 6) and (osVersionInfo.dwMinorVersion >= 2)) or
		(osVersionInfo.dwMajorVersion = 10));
end;

procedure SetSystem32Path(var strSys32: String; bRedirect: Boolean);
var
	szSystemFolder: array[0..(MAX_PATH+1)] of Char;
begin
	// Set the path to %WinDir%\System32
	// Note to developer: Delphi 7 produces 32-bit application only, so if running on 64-bit
	// Windows, calls to the 64-bit "System32" folder will be subject to redirection under WOW64.
	if (bRedirect) and (IsWindows64Bit()) then
		begin
		// 64-bit Windows (ie. our application is running WOW64)
		// Note: This will usually be "C:\Windows\Sysnative\"
		GetWindowsDirectory(szSystemFolder, MAX_PATH+1);
		strSys32 := (IncludeTrailingPathDelimiter(szSystemFolder) + 'Sysnative' + PathDelim);
		end
	else
		begin
		// 32-bit Windows (or the standard path, ignoring re-direction issues)
		// Note: This will usually be "C:\Windows\System32\"
		GetSystemDirectory(szSystemFolder, MAX_PATH+1);
		strSys32 := IncludeTrailingPathDelimiter(szSystemFolder);
		end;
end;

function ExpandEnvironment(const cstrValue: String): String;
var
	szResult: array[0..1023] of Char;
	dwReturn: DWORD;
begin
	// Use this function as follows:
	// strWindowsDir = ExpandEnvironment('%SystemRoot%');
	// which will return "C:\Windows".

	// You can also get this by calling:
	// var
	//		szSystemFolder: array[0..(MAX_PATH+1)] of Char;
	// ...
	// GetWindowsDirectory(szSystemFolder, MAX_PATH+1);
	// strWindowsDir := IncludeTrailingPathDelimiter(szSystemFolder);
	// which will return "C:\Windows\"
	dwReturn := ExpandEnvironmentStrings(PChar(cstrValue), szResult, 1024);
	if (dwReturn = 0) then
		Result := cstrValue
	else
		Result := Trim(szResult);
end;

function IsProcessRunning(strProcessName: String) : Boolean;
var
	hSnapShot: THandle;
	process: TProcessEntry32;
	bFound: Boolean;
begin
	// Go through the list of running processes and see if the requested process is running

	{ Note to developer: An alternate method is to use the Windows "_wsystem" API from C++:
		// Pass in "std::wstring strProcess" parameter...
		std::wstring cmd_query(
			std::wstring(L"tasklist|findstr /i \"") + strProcess + L".exe\">nul 2>&1");
		bool bRunning = (_wsystem(cmd_query.data()) == 0);
		bFound := False;

	This is, however, not recommended:
	* 1) It is very slow (~160ms in one test in Aug 2018) and
	* 2) A command prompt briefly pops up to execute the command, creating an visual flicker. }
	bFound := False;
	try
		hSnapShot := CreateToolHelp32SnapShot(TH32CS_SNAPPROCESS, 0);
		process.dwSize := Sizeof(TProcessEntry32);
		if (Process32First(hSnapShot, process)) then
			bFound := (AnsiCompareText(strProcessName, process.szExeFile) = 0);

		if (not bFound) then
			begin
			while (Process32Next(hSnapShot, process)) do
				begin
				bFound := (AnsiCompareText(strProcessName, process.szExeFile) = 0);
				if (bFound) then
					break;
				end;
			end;

		CloseHandle(hSnapShot);
	except
	end;

	Result := bFound;
end;

function GetProcessThreadCount(dwProcessID: DWORD) : Integer;
var
	nThreadCount: Integer;
	hSnapShot: THandle;
	bNextThread: Boolean;
	threadEntry: TThreadEntry32;
begin
	// Returns the number of running threads in the given process
	nThreadCount := 0;
	hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
	if (hSnapShot <> INVALID_HANDLE_VALUE) then
		begin
		try
			// Get the first thread
			threadEntry.dwSize := SizeOf(TThreadEntry32);
			bNextThread := Thread32First(hSnapShot, threadEntry);
			while (bNextThread) do
				begin
				if (threadEntry.th32OwnerProcessID = dwProcessID) then
					begin
					// This thread is owned by the requested process...
					Inc(nThreadCount);
					end;

				// Onto the next thread
				bNextThread := Thread32Next(hSnapShot, threadEntry);
			end;
		finally
			// Close handle
			CloseHandle(hSnapShot);
		end;
		end;

	Result := nThreadCount;
end;

function GetSystemThreadCount() : Integer;
var
	nThreadCount: Integer;
	hSnapShot: THandle;
	bNextThread: Boolean;
	threadEntry: TThreadEntry32;
begin
	// Count total threads for all processes (see "GetProcessThreadCount")
	nThreadCount := 0;
	hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
	if (hSnapShot <> INVALID_HANDLE_VALUE) then
		begin
		try
			threadEntry.dwSize := SizeOf(TThreadEntry32);
			bNextThread := Thread32First(hSnapShot, threadEntry);
			while (bNextThread) do
				begin
				Inc(nThreadCount);
				bNextThread := Thread32Next(hSnapShot, threadEntry);
				end;
		finally
			CloseHandle(hSnapShot);
		end;
		end;

	Result := nThreadCount;
end;

function IsThreadRunning(dwThreadID: DWORD) : Boolean;
var
	bThreadRunning: Boolean;
	hSnapShot: THandle;
	bNextThread: Boolean;
	threadEntry: TThreadEntry32;
begin
	// Determine if the requested thread is running (see "GetProcessThreadCount")
	bThreadRunning := False;
	hSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
	if (hSnapShot <> INVALID_HANDLE_VALUE) then
		begin
		try
			threadEntry.dwSize := SizeOf(TThreadEntry32);
			bNextThread := Thread32First(hSnapShot, threadEntry);
			while (bNextThread) do
				begin
				if (threadEntry.th32ThreadID = dwThreadID) then
					begin
					bThreadRunning := True;
					break;
					end;

				bNextThread := Thread32Next(hSnapShot, threadEntry);
				end;
		finally
			CloseHandle(hSnapShot);
		end;
		end;

	Result := bThreadRunning;
end;

function FindWindowByTitle(hStartHandle: HWND; strWindowTitle: string) : HWND;
var
	hNextHandle: HWND;
	strNextTitle: array[0..260] of Char;
begin
	// Find a window with the requested title (eg. "Notepad"). This method should be provided with
	// a (starting) handle, such as the main application window. For example:
	//		hWindowToFind := FindWindowByTitle(Application.Handle, "Notepad");

	// Get the first window
	hNextHandle := GetWindow(hStartHandle, GW_HWNDFIRST);
	while (hNextHandle > 0) do
		begin
		// Retrieve the title of the window
		GetWindowText(hNextHandle, strNextTitle, 255);
		if (Pos(strWindowTitle, StrPas(strNextTitle)) <> 0) then
			begin
			// Found it!
			Result := hNextHandle;
			Exit;
			end
		else
			begin
			// Not found, try the next window
			hNextHandle := GetWindow(hNextHandle, GW_HWNDNEXT);
			end;
	end;

	// If we get here, the window was not found
	Result := 0;
end;

procedure GetDiskSpaceGB(const strDrive: String; var fTotalGB: Single; var fTotalFreeGB: Single);
var
	nFreeBytes64, nTotalBytes64, nTotalFreeBytes64: Int64;
begin
	// Return the disk space (in GB)
	// Note: Network drive sizes can exceed 5+ TB! If converting to a 32-bit integer, convert bytes
	// into MB or (better) GB. Do not use kB because a 5x10^12 bytes is ~5x10^9 kB which overflows
	// the range of a 32-bit integer.
	fTotalGB := 0.0;
	fTotalFreeGB := 0.0;
	if (GetDiskFreeSpaceEx(PChar(strDrive),
			nFreeBytes64, nTotalBytes64, PLargeInteger(@nTotalFreeBytes64))) then
		begin
		// Drive found and details returned!
		fTotalGB := (nTotalBytes64 / GIGA_BYTE);
		fTotalFreeGB := (nTotalFreeBytes64 / GIGA_BYTE);
		end;
end;

function GetDiskFileSystem(const cstrDrive: String) : String;
var
	szPartitionType: array[0..32] of Char;
	dwDummy: DWORD;
begin
	// Return the file system (usually NTFS or FAT32)
	GetVolumeInformation(PChar(cstrDrive), nil, 0, nil,
		dwDummy, dwDummy, szPartitionType, SizeOf(szPartitionType));
	Result := Format('%s', [szPartitionType]);

	// Note to developer: Systems running FAT32 are less robust to sudden power cuts. The file
	// system can be updated from the command line using:
	//		convert c: /fs:ntfs
	// This change is one-way! Once you convert to NTFS, you cannot switch back to FAT32.
end;

function GetSystemDrives(cdwDrives: DWORD = DRIVE_ALL_TYPES) : String;
var
	dwDrives, dwDriveType: DWORD;
	acDrives: array[0..128] of Char;
	pDrive: PChar;
	strAllDrives, strTmp: String;
	bFirstDrive: Boolean;
begin
	// Build up a list of all drives defined on (or attached to) the system
	strAllDrives := '?';
	dwDrives := GetLogicalDriveStrings(SizeOf(acDrives), acDrives);
	if (dwDrives > 0) then
		begin
		if (dwDrives > SizeOf(acDrives)) then
			raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));

		pDrive := acDrives;
		strAllDrives := '';
		bFirstDrive := True;
		while (pDrive^ <> #0) do
			begin
			// Each drive type has an associated type
			strTmp := '';
			dwDriveType := GetDriveType(pDrive);
			if (dwDriveType = DRIVE_UNKNOWN) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_UNKNOWN)) then
				strTmp := Format('%s [Unknown]', [pDrive]);

			if (dwDriveType = DRIVE_NO_ROOT_DIR) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_NO_ROOT_DIR)) then
				strTmp := Format('%s [No Root]', [pDrive]);

			if (dwDriveType = DRIVE_REMOVABLE) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_REMOVABLE)) then
				strTmp := Format('%s [Removable]', [pDrive]);

			if (dwDriveType = DRIVE_FIXED) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_FIXED)) then
				strTmp := Format('%s [Fixed]', [pDrive]);

			if (dwDriveType = DRIVE_REMOTE) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_REMOTE)) then
				strTmp := Format('%s [Net]', [pDrive]);

			if (dwDriveType = DRIVE_CDROM) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_CDROM)) then
				strTmp := Format('%s [CD]', [pDrive]);

			if (dwDriveType = DRIVE_RAMDISK) and
					((cdwDrives = DRIVE_ALL_TYPES) or (cdwDrives = DRIVE_RAMDISK)) then
				strTmp := Format('%s [RAM]', [pDrive]);

			// Remove trailing backslashes
			strTmp := StringReplace(strTmp, '\', '', [rfReplaceAll]);

			// Build up the list of drives...
			if (Length(strTmp) > 0) then
				begin
				if (bFirstDrive) then
					strAllDrives := (strAllDrives + Format('%s', [strTmp]))
				else
					strAllDrives := (strAllDrives + Format(', %s', [strTmp]));

				bFirstDrive := False;
				end;

			// Move onto the next drive
			Inc(pDrive, 4);
			end;
		end;

	Result := strAllDrives;
end;

procedure SaveToClipboard(const cstrText: String);
begin
	// Save some text to the Windows clipboard
	Clipboard.AsText := cstrText;
end;

// Registry
function RegGetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	var pValue: Pointer; var dwValSize: Cardinal): Boolean;
var
	strNameReversed, strSubKey: String;
	nPos: Integer;
	dwMyValType, dwBufferSize: DWORD;
	hTemp: HKEY;
	pBuffer: Pointer;
begin
	// Get a value from the registry
	// Note: This and the next function were originally adapted from:
	//		http://www.swissdelphicenter.ch/torry/showcode.php?id=2008
	Result := False;
	strNameReversed := AnsiReverseString(cstrName);
	nPos := (Length(cstrName) - Pos('\', strNameReversed));
	if (nPos > 0) then
		begin
		strSubKey := Copy(cstrName, 1, nPos);
		if (RegOpenKeyEx(hRootKey, PChar(strSubKey), 0, KEY_READ, hTemp) = ERROR_SUCCESS) then
			begin
			strSubKey := Copy(cstrName, nPos + 2, Length(cstrName) - nPos - 1);
			if (RegQueryValueEx(hTemp, PChar(strSubKey), nil, @dwMyValType, nil, @dwBufferSize) = ERROR_SUCCESS) then
				begin
				GetMem(pBuffer, dwBufferSize);
				if (RegQueryValueEx(hTemp, PChar(strSubKey), nil, @dwMyValType, pBuffer, @dwBufferSize) = ERROR_SUCCESS) then
					begin
					if (dwValType = dwMyValType) then
						begin
						// * For REG_DWORD:
						//		CopyMemory(@dwTarget, pValue, dwBufferSize);
						// * For REG_SZ (with trailing NULL character):
						//		SetLength(strTarget, dwBufferSize);
						//		if (dwBufferSize > 0) then
						//			CopyMemory(@strTarget[1], pValue, dwBufferSize);
						// * For REG_SZ (without trailing NULL character, usually want this):
						//		SetLength(strTarget, 0);
						//		if (dwBufferSize > 1) then
						//			begin
						//			SetLength(strTarget, (dwBufferSize-1));
						//			CopyMemory(@strTarget[1], pValue, (dwBufferSize-1));
						//			end;
						pValue := pBuffer;
						dwValSize := dwBufferSize;
						Result := True;
						end
					else
						FreeMem(pBuffer);
					end
				else
					FreeMem(pBuffer);
				end;

			RegCloseKey(hTemp);
			end;
		end;
end;

function RegSetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	pValue: Pointer; dwValueSize: Cardinal): Boolean;
var
	strNameReversed, strSubKey: String;
	nPos: Integer;
	dwDummy: DWORD;
	hTemp: HKEY;
begin
	// Set a value in the registry
	Result := False;
	strNameReversed := AnsiReverseString(cstrName);
	nPos := (Length(cstrName) - Pos('\', strNameReversed));
	if (nPos > 0) then
		begin
		strSubKey := Copy(cstrName, 1, nPos);
		if (RegCreateKeyEx(hRootKey, PChar(strSubKey), 0, nil,
				REG_OPTION_NON_VOLATILE, KEY_WRITE,
				nil, hTemp, @dwDummy) = ERROR_SUCCESS) then
			begin
			strSubKey := Copy(cstrName, (nPos + 2), (Length(cstrName) - nPos));
			Result := (RegSetValueEx(
				hTemp, PChar(strSubKey), 0, dwValType, pValue, dwValueSize) = ERROR_SUCCESS);
			RegCloseKey(hTemp);
			end;
		end;
end;

function RegKeyExists(hRootKey: HKEY; const cstrName: String): Boolean;
var
	hTemp: HKEY;
begin
	// Does a registry key exist? Example usage:
	//		if (RegKeyExists(HKEY_CURRENT_USER, 'Software\Microsoft'))
	Result := False;
	if (RegOpenKeyEx(hRootKey, PChar(cstrName), 0, KEY_READ, hTemp) = ERROR_SUCCESS) then
		begin
		Result := True;
		RegCloseKey(hTemp);
		end;
end;

function RegValueExists(hRootKey: HKEY; const cstrName: String): Boolean;
var
	strNameReversed, strSubKey: String;
	nPos: Integer;
	hTemp: HKEY;
begin
	// Does a registry value exist? Example usage:
	//		if (RegValueExists(HKEY_CURRENT_USER, 'Software\Microsoft\xyz'))
	// which returns true if the key "Software\Microsoft" and the value "xyz" exist
	Result := False;
	strNameReversed := AnsiReverseString(cstrName);
	nPos := (Length(cstrName) - Pos('\', strNameReversed));
	if (nPos > 0) then
		begin
		strSubKey := Copy(cstrName, 1, nPos);
		if (RegOpenKeyEx(hRootKey, PChar(strSubKey), 0, KEY_READ, hTemp) = ERROR_SUCCESS) then
			begin
			strSubKey := Copy(cstrName, (nPos + 2), (Length(cstrName) - nPos));
			Result := (RegQueryValueEx(hTemp, PChar(strSubKey), nil, nil, nil, nil) = ERROR_SUCCESS);
			RegCloseKey(hTemp);
			end;
		end;
end;

procedure RegEnumSubKeys(hRootKey: HKEY; const cstrName: String; astrKeys: TStringList);
var
	registry: TRegistry;
begin
	// Enumerate sub-keys of the given registry key
	registry := TRegistry.Create();
	try
		registry.RootKey := hRootKey;
		registry.OpenKeyReadOnly(cstrName);
		try
			registry.GetValueNames(astrKeys);
		except
		end;
	finally
		registry.Free;
	end;
end;

procedure RegEnumSubValues(hRootKey: HKEY; const cstrName: String; astrValues: TStringList);
var
	registry: TRegistry;
begin
	// Enumerate values of the given registry key
	// Note to developer: Delphi 7 produces 32-bit processes. When run on a 64-bit OS, calls to the
	// registry may be redirected by "Windows-on-Windows 64-bit" or WOW64 on later versions of
	// Windows. This only affects a subsetof registry keys (mostly in HKLM). Ffr example:
	//		* HKLM\SOFTWARE\Microsoft\Windows\CurrentVersion\Run
	// will actually return the values from:
	//		* HKLM\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\Run
	// There doesn't appear to be any way for a 32-bit application to read the first registry key
	registry := TRegistry.Create();
	try
		registry.RootKey := hRootKey;
		registry.OpenKeyReadOnly(cstrName);
		try
			registry.GetValueNames(astrValues);
		except
		end;
	finally
		registry.Free;
	end;
end;

procedure DumpRegKeyValues(var fp: TextFile; hRootKey: HKEY; const cstrKey: String);
var
	strHeader: String;
	astrValues: TStringList;
	nValue: Integer;
begin
	// Save all values in the given registry key
	if (hRootKey = HKEY_CLASSES_ROOT) then
		strHeader := ('[HKCR\' + cstrKey + ']')
	else if (hRootKey = HKEY_CURRENT_USER) then
		strHeader := ('[HKCU\' + cstrKey + ']')
	else if (hRootKey = HKEY_LOCAL_MACHINE) then
		strHeader := ('[HKLM\' + cstrKey + ']')
	else if (hRootKey = HKEY_USERS) then
		strHeader := ('[USERS\' + cstrKey + ']')
	else
		strHeader := ('[' + cstrKey + ']');

	WriteLn(fp, strHeader);

	astrValues := TStringList.Create();
	RegEnumSubValues(hRootKey, cstrKey, astrValues);
	if (astrValues.Count > 0) then
		begin
		for nValue:=0 to (astrValues.Count - 1) do
			WriteLn(fp, '    ' + astrValues[nValue]);
		end
	else
		WriteLn(fp, '(None)');

	WriteLn(fp, '');

	// Clean up
	astrValues.Free();
end;

procedure DumpRegValue(var fp: TextFile; hRootKey: HKEY; const cstrName: String; dwValType: Cardinal);
var
	strValue: String;
	pData: Pointer;
	dwBufferSize: DWORD;
begin
	strValue := '';
	if (RegGetValue(hRootKey, cstrName, dwValType, pData, dwBufferSize)) then
		begin
		SetLength(strValue, 0);
		if (dwBufferSize > 1) then
			begin
			SetLength(strValue, (dwBufferSize-1));
			CopyMemory(@strValue[1], pData, (dwBufferSize-1));
			end;

		FreeMem(pData);
		WriteLn(fp, Format('%s = %s', [cstrName, strValue]));
		end;
end;

function RegisterOCX(strDLL: String; bSilent: Boolean = False) : Boolean;
type
	TDllRegisterServer = function(): HRESULT; stdcall;
var
	hOCX: THandle;
	funcRegister: TDllRegisterServer;
	bSuccess: Boolean;
begin
	// Register an ActiveX or COM DLL / OCX
	bSuccess := False;
	if (FileExists(strDLL)) then
		begin
			hOCX := 0;
			try
				hOCX := LoadLibrary(PChar(strDLL));
				if (hOCX > 0) then
					begin
					funcRegister := GetProcAddress(hOCX, 'DllRegisterServer');
					if (@funcRegister <> nil) then
						bSuccess := (funcRegister() = S_OK);
					end;
			finally
				FreeLibrary(hOCX);
			end;
		end;

	if (not bSilent) then
		begin
		if (bSuccess) then
			MessageBox(0,
				PAnsiChar(Format('Success...%s registered OK!', [ExtractFileName(strDLL)])),
				'Register', MB_ICONEXCLAMATION)
		else
			MessageBox(0,
				PAnsiChar(Format('%s not registered OK.', [ExtractFileName(strDLL)])),
				'Register', MB_ICONEXCLAMATION);
		end;

	Result := bSuccess;
end;

// File utilities
function FileHasData(const cstrFile: String) : Boolean;
var
	fpTxt: TextFile;
begin
	// This method will fail or return no data if:
	// * The file is genuinely empty
	// * The file has an open file handle associated with it
	// * It's entry in the Master File Table (NTFS) or File Allocation Table (FAT32) is corrupt
	Result := False;
	try
		try
			AssignFile(fpTxt, cstrFile);
			Reset(fpTxt);
			Result := (not EOF(fpTxt));
		except
		end;
	finally
		CloseFile(fpTxt);
	end;
end;

procedure GetFolderListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
var
	find: TSearchRec;
begin
	// Find all files in a folder and add to a list. The caller has responsibility for ensuring the
	// list is in the right state (ie. created and empty!).

	// Add a final backslash to the path (if required, eg. C:\Temp\)
	if (RightStr(strFolder, 1) <> '\') then
		strFolder := (strFolder + '\');

	// Now generate the list
	if (FindFirst(strFolder + strWildCard, faAnyFile - faDirectory, find) = 0) then
		begin
		repeat
			astrList.AddObject(strFolder + find.Name, TObject(find.Time))
		until (FindNext(find) <> 0);

		FindClose(find);
		end;

	// Are we looking recursively through the folder?
	if (bRecursive) then
		begin
		if (FindFirst(strFolder + '*.*', faDirectory, find) = 0) then
			try
				repeat
					if ((find.Attr and faDirectory) <> 0) and
							(find.Name <> '.') and
							(find.Name <> '..') then
						GetFolderListing(strFolder + find.Name, strWildCard, astrList, True);
				until FindNext(find) <> 0;
			finally
				FindClose(find);
			end;
		end;
end;

function DeleteFolder(strFolder: String) : Boolean;
var
	find: TSearchRec;
begin
	// Delete a folder (including contents)
	if (FindFirst(strFolder + '*.*', faAnyFile, find) = 0) then
		begin
		repeat
			if (find.Attr and faDirectory = 0) then
				DeleteFile(strFolder + find.Name)
			else
				if (find.Name <> '.') and (find.Name <> '..') then
					DeleteFolder(strFolder + find.Name + '\');

		until (FindNext(find) <> 0);

		FindClose(find);
		end;

	Result := RemoveDirectory(PChar(strFolder));
end;

function IsFolderWriteable(const cstrPath: String) : Boolean;
var
	fileTest: file;
	strFile: String;
	nChar: Cardinal;
begin
	// Generate a random filename that does NOT exist in the directory
	Result := True;
	repeat
		strFile := IncludeTrailingPathDelimiter(cstrPath);
		for nChar:=1 to (250 - Length(strFile)) do
			strFile := (strFile + char(Random(26) + 65));
	until (not FileExists(strFile));

	// Attempt to write the file to the directory. This will fail on something like a CD drive or
	// if the user does not have permission, but otherwise should work.
	try
		AssignFile(fileTest, strFile);
		Rewrite(fileTest, 1);

		// Note: Actually check for the existence of the file. Windows may appear to have created
		// the file, but this fails (without an exception) if advanced security attibutes for the
		// folder have denied "Create Files / Write Data" access to the logged in user.
		if (not FileExists(strFile)) then
			Result := False;
	except
		Result := False;
	end;

	// If the file was written to the path, delete it
	if (Result) then
		begin
		CloseFile(fileTest);
		Erase(fileTest);
		end;

	// Note: An alternative method uses the Windows API "GetFileAttributes":
	{ fa := GetFileAttributes(PChar(strPath));	// fa: Cardinal;
	if ((fa and FILE_ATTRIBUTE_DIRECTORY) <> 0) and ((fa and FILE_ATTRIBUTE_READONLY) <> 0) then
		ShowMessage('Directory is read-only'); }

	// Unfortunately, FILE_ATTRIBUTE_READONLY does not get set by Windows for folders. Attempting
	// to write a file and catching errors works reliably.
end;

procedure CopyFilesBetweenFolders(strSourceFolder, strTargetFolder, strWildcard: String);
var
	find: TSearchRec;
begin
	// Find all files in Source (based on wildcard) and copy to Target
	if (FindFirst(strSourceFolder + '\' + strWildCard, faAnyFile - faDirectory, find) = 0) then
		begin
		repeat
			CopyFile(PChar(strSourceFolder + '\' + find.Name),
				PChar(strTargetFolder + '\' + find.Name), False);
		until (FindNext(find) <> 0);

		FindClose(find);
		end;
end;

function GetSizeOfFile(strFilename: String) : DWORD;
var
	fHandle: Integer;
	dwFileSize: DWORD;
begin
	// Get the size of the file, in BYTEs
	dwFileSize := 0;
	try
		fHandle := FileOpen(strFilename, fmOpenRead);
		dwFileSize := GetFileSize(fHandle, nil);
		FileClose(fHandle);
	except
	end;

	if (dwFileSize = $FFFFFFFF) then
		Result := 0		// Error
	else
		Result := dwFileSize;
end;

function GetFullFileVersion(szFile: PChar) : String;
var
	strVersion: String;
	pstrBuffer: PChar;
	dwSize, dwLength: DWORD;
	pVersion: pointer;
	ver: VS_FIXEDFILEINFO;
begin
	// Retrieve file version info as MAJOR.MINOR.MICRO.BUILD eg. "v1.54.0.325"
	strVersion := '';
	dwSize := GetFileVersionInfoSize(szFile, dwSize);
	if (dwSize > 0) then
		begin
		pstrBuffer := AllocMem(dwSize);
		try
			GetFileVersionInfo(szFile, 0, dwSize, pstrBuffer);
			if (VerQueryValue(pstrBuffer, PChar(strVersion), pVersion, dwLength)) then
				begin
				CopyMemory(@ver, pVersion, SizeOf(ver));
				strVersion := Format('v%d.%d.%d.%d', [
					ver.dwFileVersionMS shr 16,
					ver.dwFileVersionMS and $FFFF,
					ver.dwFileVersionLS shr 16,
					ver.dwFileVersionLS and $FFFF]);
				end;
		finally
			FreeMem(pstrBuffer, dwSize);
		end;
	end;

	Result := strVersion;
end;

procedure ChangeFilename(strOldPath, strNewPath: String);
begin
	// Change the name of a file (with short delay)
	if (FileExists(strOldPath)) and (not FileExists(strNewPath)) then
		begin
		// "RenameFile" can fail, just ignore. Reasons for failure include:
		// * user does not have permission to modify the file
		// * path is read-only eg. a read-only USB stick
		try
			RenameFile(strOldPath, strNewPath);
		finally
		end;
		end;
end;

// System
function TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;
var
	nErrorCode: Integer;
begin
	// Helper function to convert string to integer. Usage:
	//		bSuccess:= TryStrToInt(ebValue.Text, nValue);

	// Examples:
	//		TryStrToInt("13", nValue)		True, nValue set to 13
	//		TryStrToInt("1x", nValue)		False, nValue unchanged
	Val(cstrInput, nOutput, nErrorCode);
	Result := (nErrorCode = 0);
end;
// End: Private methods

end.
