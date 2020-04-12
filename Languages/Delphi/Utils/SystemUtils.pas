{ General system utilities designed for Windows applications }
unit SystemUtils;

interface

uses
  Windows, Classes, StdCtrls;

const
  DUMMY_INTERFACE_CONSTANT = 0;

type
  PDummyInterfacePointer = ^Integer;

// Public methods

// Windows
function IsWindows64Bit() : Boolean;
function IsWindows10() : Boolean;
procedure SetSystem32Path(var strSys32: String; bRedirect: Boolean);
function ExpandEnvironment(const cstrValue: String): String;

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
procedure SaveToClipboard(const cstrText: String);
function TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;

implementation

uses
  Clipbrd, StrUtils, SysUtils;

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
procedure SaveToClipboard(const cstrText: String);
begin
	// Save some text to the clipboard. Usually only used for development purposes.
	Clipboard.AsText := cstrText;
end;

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
