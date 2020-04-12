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

  // Network adapters
  MAX_ADAPTER_ADDRESS_LENGTH		= 8;
  MAX_ADAPTER_DESCRIPTION_LENGTH	= 128;
  MAX_ADAPTER_NAME_LENGTH			= 256;

type
  // Information about a specific network adapter
  ADAPTER_INFO = record
	bLive, bDhcpEnabled: Boolean;
	strMacAddress, strDescription, strIpAddress, strSubnetMask: String;
  end;

  // Generic floating-point precision point
  PFloatPoint = ^TFloatPoint;
  TFloatPoint = packed record
	X, Y: Single;
  end;

  // Generic floating-point precision rectangle
  PFloatRect = ^TFloatRect;
  TFloatRect = packed record
	Left, Top, Right, Bottom: Single;
  end;

  // Generic quadrilateral
  PQuadrilateral = ^TQuadrilateral;
  TQuadrilateral = packed record
	aPts: array[0..3] of TPoint;
  end;

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

// Network
function ShareFolder(wstrFolder, wstrName: WideString) : Boolean;
procedure UnshareFolder(wstrName: WideString);
function GetAdapterInfo(var connect: array of ADAPTER_INFO) : BYTE;
procedure GetIpAddressRange(nOctet: Integer; var nMin: Integer; var nMax: Integer);
procedure TranslateStringToIpAddress(strIP: String; var ipAddress);
function Ping(strIpAddress : String) : Boolean;
function PingWithError(strIpAddress : String; var strError: String) : Boolean;
function HasInternet(const strURL: String) : Boolean;

// File utilities
function FileHasData(const cstrFile: String) : Boolean;
procedure GetFolderListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
function DeleteFolder(strFolder: String) : Boolean;
function IsFolderWriteable(const cstrPath: String) : Boolean;
procedure CopyFilesBetweenFolders(strSourceFolder, strTargetFolder, strWildcard: String);
function GetSizeOfFile(strFilename: String) : DWORD;
procedure ChangeFilename(strOldPath, strNewPath: String);

// String
function IsNumber(const cstrInput: String) : Boolean;
function TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;
function GetTimeStringFromSeconds(dwSeconds: DWORD; bIncludeSeconds: Boolean = True) : String;
function GetIsoDateTimeString(dtSource: TDateTime) : String;
function ConvertTitleCase(const cstrInput: String) : String;
function GetRandomString(const cbyLength: BYTE; const cbLettersOnly: Boolean) : String;
procedure ParseString(const strSource: String; const strDelimiter: String; list: TStringList);

// System, math and other general methods
function WithinRect(pt: TPoint; rct: TRect): Boolean;
function FloatPoint(fX: Single; fY: Single) : TFloatPoint;
function RotatePoint(pt: TPoint; fAngle: Single; ptOrigin: TPoint) : TPoint; overload
function RotatePoint(fpt: TFloatPoint; fAngle: Single; fptOrigin: TFloatPoint) : TFloatPoint; overload
procedure RotatePoints(var pts: array of TPoint; fAngle: Single; ptOrigin: TPoint); overload
procedure RotatePoints(var fpts: array of TFloatPoint; fAngle: Single; fptOrigin: TFloatPoint); overload
function DegreesToRadians(fDegrees: Single) : Single;
function RadiansToDegrees(fDegrees: Single) : Single;

implementation

uses
  Clipbrd, Registry, StrUtils, SysUtils, TLHelp32, WinSock;

const
  // Disk sizes / capacities
  KILO_BYTE		= Int64(1024);
  MEGA_BYTE		= Int64(1024 * KILO_BYTE);
  GIGA_BYTE		= Int64(1024 * MEGA_BYTE);
  TERA_BYTE		= Int64(1024 * GIGA_BYTE);

  // Time constants
  SECONDS_IN_DAY	= (3600 * 24);
  SECONDS_IN_MONTH	= (3600 * 24 * 30);
  SECONDS_IN_YEAR	= (3600 * 24 * 365);
  MINUTES_IN_HOUR	= 60;
  MINUTES_IN_DAY	= (24 * 60);

  // Maximum time (used by various system timers that use GetTickCount)
  MAX_TIME: DWORD = $FFFFFFFF;

  // Date / time stamps are always given in ISO 8601 format (yyyy-mm-ddThh:nn:ss) regardless of
  // what regional settings are in use
  DATETIME_FORMAT_ISO8601	= '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d';
  DATE_FORMAT				= 'yyyy-mm-dd';
  TIME_FORMAT				= 'hh:nn:ss';

type
  // This record is used to specify shared network resources (eg. folders)
  T_SHARE_INFO_2 = packed record
	shi2_netname		: PWCHAR;
	shi2_type			: DWORD;
	shi2_remark			: PWCHAR;
	shi2_permissions	: DWORD;
	shi2_max_uses		: DWORD;
	shi2_current_uses	: DWORD;
	shi2_path			: PWCHAR;
	shi2_passwd			: PWCHAR;
  end;

  // These records are used by the Ping feature to determine whether an IPv4 address is reachable
  TSunB = packed record
	by1, by2, by3, by4: BYTE;
  end;

  TSunW = packed record
	w1, w2: WORD;
  end;

  PIpAddress = ^TIpAddress;
  TIpAddress = record
	case Integer of
		0: (S_un_b: TSunB);
		1: (S_un_w: TSunW);
		2: (S_addr: LongWord);
  end;
  IpAddress = TIpAddress;

 // Definitions from the Iphlpapi unit (apparently defined in later versions of Delphi)
  PIP_MASK_STRING = ^IP_MASK_STRING;
  IP_ADDRESS_STRING = record
	S: array[0..15] of Char;
  end;
  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  IP_MASK_STRING = IP_ADDRESS_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  _IP_ADDR_STRING = record
	Next: PIP_ADDR_STRING;
	IpAddress: IP_ADDRESS_STRING;
	IpMask: IP_MASK_STRING;
	Context: DWORD;
  end;
  IP_ADDR_STRING = _IP_ADDR_STRING;

   PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  _IP_ADAPTER_INFO = record
	Next: PIP_ADAPTER_INFO;
	ComboIndex: DWORD;
	AdapterName: array[0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
	Description: array[0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
	AddressLength: UINT;
	Address: array[0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
	Index: DWORD;
	Type_: UINT;
	DhcpEnabled: UINT;
	CurrentIpAddress: PIP_ADDR_STRING;
	IpAddressList: IP_ADDR_STRING;
	GatewayList: IP_ADDR_STRING;
	DhcpServer: IP_ADDR_STRING;
	HaveWins: BOOL;
	PrimaryWinsServer: IP_ADDR_STRING;
	SecondaryWinsServer: IP_ADDR_STRING;
	LeaseObtained: Integer;
	LeaseExpires: Integer;
  end;
  IP_ADAPTER_INFO = _IP_ADAPTER_INFO;

// Functions imported from external DLLs
function NetShareAdd(strServername: PChar;
	dwLevel: DWORD; pBuf: pointer; pdwError: PDWORD) : DWORD; stdcall;
	external 'netapi32.dll' name 'NetShareAdd';
function NetShareDel(strServername: PChar;
	strNetname: PWChar; dwReserved: DWORD) : DWORD; stdcall;
	external 'netapi32.dll' name 'NetShareDel';
function NetGetJoinInformation(lpServer: LPCWSTR;
	lpNameBuffer: LPWSTR; pBufferType: Pointer) : LongInt; stdcall;
	external 'netapi32.dll' name 'NetGetJoinInformation';
function NetApiBufferFree(pBuffer: Pointer) : Integer; stdcall;
	external 'netapi32.dll' name 'NetApiBufferFree';

function GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG) : DWORD; stdcall;
	external 'iphlpapi.dll' name 'GetAdaptersInfo';
// Note: The function "GetExtendedTcpTable" from iphlpapi.dll retrieves a table that contains a
// list of TCP endpoints available to the application. Could be interesting...

// Note: Before v1.57.75.0, the "IcmpX" methods below were imported from icmp.dll. This was not
// correct because icmp.dll was intended for Windows 2000 only.
function IcmpCreateFile() : THandle; stdcall; external 'iphlpapi.dll';
function IcmpCloseHandle(icmpHandle: THandle) : Boolean; stdcall; external 'iphlpapi.dll';
function IcmpSendEcho(IcmpHandle: THandle; ipDest: IpAddress;
	pRequestData: Pointer; nRequestSize: SmallInt; RequestOptions: Pointer;
	pReplyBuffer: Pointer; dwReplySize: DWORD; dwTimeout: DWORD) : DWORD; stdcall; external 'iphlpapi.dll';

function InternetCheckConnectionA(lpszUrl: PAnsiChar; dwFlags: DWORD; dwReserved: DWORD) : BOOL; stdcall;
	external 'wininet.dll' name 'InternetCheckConnectionA';

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

// Network
function ShareFolder(wstrFolder, wstrName: WideString) : Boolean;
var
	sh: T_SHARE_INFO_2;
	dwResult: DWORD;
	dwError: DWORD;
begin
	// Share a folder across the network
	with sh do begin
		shi2_netname := PWidechar(wstrName);
		shi2_type := 0;
		shi2_remark := nil;
		shi2_permissions := 0;
		shi2_max_uses := $FFFFFFFF;
		shi2_current_uses := 0;
		shi2_path := PWidechar(wstrFolder);
		shi2_passwd := nil;
	end;

	dwResult := NetShareAdd(nil, 2, @sh, @dwError);
	Result := (dwResult = 0);
end;

procedure UnshareFolder(wstrName: WideString);
begin
	// Remove a network folder share
	NetShareDel(nil, PWidechar(wstrName), 0);
end;

function GetAdapterInfo(var connect: array of ADAPTER_INFO) : BYTE;
var
	byAdapterCount: BYTE;
	pAdapterList, pAdapter: PIP_ADAPTER_INFO;
	uBufferLen: ULong;
	dwStatus: DWORD;
	nAddress: Integer;
begin
	// According to MSDN, over-allocate memory to avoid having to call "GetAdaptersInfo" multiple
	// times
	byAdapterCount := 0;
	uBufferLen := (1024 * 15);
	GetMem(pAdapterList, uBufferLen);
	try
	repeat
		dwStatus := GetAdaptersInfo(pAdapterList, uBufferLen);
		case dwStatus of
		ERROR_SUCCESS:
				begin
				// Some versions of Windows return ERROR_SUCCESS with uBufferLen=0 instead of
				// returning ERROR_NO_DATA as documented...
				if (uBufferLen = 0) then
					raise Exception.Create('No network adapter on the local computer.');

				break;
				end;

		ERROR_NOT_SUPPORTED:
			begin
			raise Exception.Create('GetAdaptersInfo is not supported by the operating system.');
			end;

		ERROR_NO_DATA:
			begin
			raise Exception.Create('No network adapter on the local computer.');
			end;

		ERROR_BUFFER_OVERFLOW:
			begin
			ReallocMem(pAdapterList, uBufferLen);
			end;
		else
			SetLastError(dwStatus);
			RaiseLastOSError();
		end;
	until False;

	// Retrieve the data for the first few adapters only. If we start supporting computers with
	// numerous (or temporary) adapters, this code may need to be modified.
	pAdapter := pAdapterList;
	while (pAdapter <> nil) and (byAdapterCount < 5) do
		begin
		// This adapter is alive
		connect[byAdapterCount].bLive := True;

		// MAC address
		if (pAdapter^.AddressLength > 0) then
			begin
			for nAddress:=0 to pAdapter^.AddressLength - 1 do
				begin
				if (nAddress > 0) then
					connect[byAdapterCount].strMacAddress := (connect[byAdapterCount].strMacAddress + '-');

				connect[byAdapterCount].strMacAddress :=
					(connect[byAdapterCount].strMacAddress + IntToHex(pAdapter^.Address[nAddress], 2));
				end;
			end;

		// Network adapter description
		connect[byAdapterCount].strDescription := pAdapter^.Description;

		// DHCP enabled?
		connect[byAdapterCount].bDhcpEnabled := (pAdapter^.DhcpEnabled <> 0);

		// IP address and subnet mask
		connect[byAdapterCount].strIpAddress := pAdapter^.IpAddressList.IpAddress.S;
		connect[byAdapterCount].strSubnetMask := pAdapter^.IpAddressList.IpMask.S;

		// Move onto the next adapter (though note we only return information for the first two)
		pAdapter := pAdapter^.next;
		Inc(byAdapterCount);
		end;
	finally
		FreeMem(pAdapterList);
	end;

	// Return the number of adapters to the caller
	Result := byAdapterCount;
end;

procedure GetIpAddressRange(nOctet: Integer; var nMin: Integer; var nMax: Integer);
begin
	// Restrictions on IPv4 addresses depend on the class of network that the PC will be attached
	// to and can be somewhat complex. The X-ray will only be connected to class A/B/C networks for
	// which the IPv4 ranges are:
	// 1st octet		Other octets
	// 1-223			0-255

	// Note: This restriction applies to the IP Address and Default Gateway only. There is no such
	// restriction for the Subnet Mask.
	if (nOctet = 1) then
		begin
		nMin := 1;
		nMax := 223;
		end
	else
		begin
		nMin := 0;
		nMax := 255;
		end;
end;

procedure TranslateStringToIpAddress(strIP: String; var ipAddress);
var
	phe: PHostEnt;
	pac: PChar;
begin
	try
		phe := GetHostByName(PChar(strIP));
		if (Assigned(phe)) then
			begin
			pac := phe^.h_addr_list^;
			if (Assigned(pac)) then
				begin
				with TIpAddress(ipAddress).S_un_b do
					begin
					by1 := Byte(pac[0]);
					by2 := Byte(pac[1]);
					by3 := Byte(pac[2]);
					by4 := Byte(pac[3]);
					end;
				end
			else
				begin
				raise Exception.Create('Error getting IP from HostName');
				end;
			end
		else
			begin
			raise Exception.Create('Error getting HostName');
			end;
	except
		FillChar(ipAddress, SizeOf(ipAddress), #0);
	end;
end;

function Ping(strIpAddress : String) : Boolean;
const
	// The ICMP buffer needs to be large enough to accommodate any ICMP error messages:
	// * sizeof(ICMP_ECHO_REPLY) + 8 = 28 bytes on 32-bit Windows (or 64-bit Windows + 32-bit app)
	// * sizeof(ICMP_ECHO_REPLY32) + 8 = 32 bytes on 64-bit Windows (with a 64-bit application)
	ICMP_ECHO_BUFFER = 128;		// This appears to work as low as 28 on 32-bit Windows XP
var
	hPingHandle: THandle;
	address: IpAddress;
	dwReplies: DWORD;
	abyReplyBuffer: array[1..ICMP_ECHO_BUFFER] of BYTE;
begin
	// Use this function to determine if an IPv4 address can be reached

	// Note,1: This simple ICMP "ping" test only confirms that the IP address is reachable. The
	// server (eg. VNC or 3rd party application) on the host machine may not be running or the port
	// may not be available (or set incorrectly). This test also fails if the target machine is not
	// configured to respond to incoming ICMP echo requests.

	// Note,2: The ICMP echo consumes ~750µs when the IP address is reachable but ~500ms when the
	// IP address is not reachable!
	Result := False;

	hPingHandle := IcmpCreateFile();	// This can be cached for improved performance
	if (hPingHandle <> INVALID_HANDLE_VALUE) then
		begin
		TranslateStringToIpAddress(strIpAddress, address);
		dwReplies := IcmpSendEcho(
			hPingHandle, address, nil, 0, nil, @abyReplyBuffer, ICMP_ECHO_BUFFER, 200);
		IcmpCloseHandle(hPingHandle);

		// Success?
		Result := (dwReplies <> 0);
		end;
end;

function PingWithError(strIpAddress : String; var strError: String) : Boolean;
var
	bPingSuccess: Boolean;
	dwErrorCode: DWORD;
begin
	// This method returns a low-level Windows error code if ping fails (usually DBG builds only)
	bPingSuccess := Ping(strIpAddress);
	if (not bPingSuccess) then
		begin
		// Note to developer: This fails for a variety of reasons. Some common ones are listed below:
		//		IP_BUF_TOO_SMALL (11001)		The reply buffer was too small
		//		IP_REQ_TIMED_OUT (11010)		The request timed out

		// On Windows 8.1 and 10, this method can fail with error code:
		//		ERROR_INVALID_PARAMETER (87)	The parameter is incorrect
		// which happens if:
		// * the handle is not valid or
		// * the reply size buffer is too small or
		// * the timeout parameter is zero (must be non-zero on Windows 10)

		// See the following for additional details:
		// * https://docs.microsoft.com/en-us/windows/desktop/api/icmpapi/nf-icmpapi-icmpsendecho
		// * https://docs.microsoft.com/en-gb/windows/desktop/api/ipexport/ns-ipexport-icmp_echo_reply
		// * https://docs.microsoft.com/en-gb/windows/desktop/Debug/system-error-codes
		dwErrorCode := GetLastError();
		strError := Format('WinError = %d', [dwErrorCode]);
		end;

	Result := bPingSuccess;
end;

function HasInternet(const strURL: String) : Boolean;
const
	FLAG_ICC_FORCE_CONNECTION = $00000001;
begin
	// Test an internet connection to the named server

	// There are two alternate ways of checking an internet connection:
	// 1): Use the Indy "TIdHTTP" component to perform an HTTP GET operation. If the response is
	//		empty, then a connection cannot be established.
	// 2) Use a Windows function designed for this purpose ("InternetCheckConnectionA")

	// Both methods timeout after a long period (15s or more) if the  ethernet adapter(s) on the
	// PC are disabled, but (2) is faster under normal circumstances.
	Result := Boolean(InternetCheckConnectionA(PAnsiChar(strURL), FLAG_ICC_FORCE_CONNECTION, 0));
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

// String
function IsNumber(const cstrInput: String) : Boolean;
var
	nChar: Integer;
begin
	Result := False;
	for nChar:=1 to Length(cstrInput) do
		case cstrInput[nChar] of
			'0'..'9':;
		else
			Exit;
		end;

	Result := True;
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

function GetTimeStringFromSeconds(dwSeconds: DWORD; bIncludeSeconds: Boolean = True) : String;
var
	dwRemainder: DWORD;
	dwYears, dwMonths, dwDays, dwHours, dwMinutes: DWORD;
	strTimeString: String;
begin
	// Format a time string from a (potentially very large) number of seconds

	// First get the breakdown in terms of year / month / etc
	dwRemainder := dwSeconds;
	dwYears := (dwRemainder div SECONDS_IN_YEAR);
	dwRemainder := (dwRemainder - (SECONDS_IN_YEAR*dwYears));

	dwMonths := (dwRemainder div (SECONDS_IN_MONTH));
	dwRemainder := (dwRemainder - (SECONDS_IN_MONTH*dwMonths));

	dwDays := (dwRemainder div (SECONDS_IN_DAY));
	dwRemainder := (dwRemainder - (SECONDS_IN_DAY*dwDays));

	dwHours := (dwRemainder div 3600);
	dwRemainder := (dwRemainder - (3600*dwHours));

	dwMinutes := (dwRemainder div 60);
	dwRemainder := (dwRemainder - (60*dwMinutes));		// This is the seconds

	// Now format the string
	// Note: We don't tend to use spaces between the value and unit because the strings are long
	strTimeString := '';
	if (bIncludeSeconds) then
		begin
		// Include seconds
		if (dwYears > 0) then
			strTimeString := Format('%.2dyr, %.2dmth, %.2dd, %.2dh, %.2dmin, %.2ds', [
				dwYears, dwMonths, dwDays, dwHours, dwMinutes, dwRemainder])
		else if (dwMonths > 0) then
			strTimeString := Format('%.2dmth, %.2dday, %.2dhr, %.2dmin, %.2ds', [
				dwMonths, dwDays, dwHours, dwMinutes, dwRemainder])
		else if (dwDays > 0) then
			// Can start to use spaces from here because the length is shorter
			strTimeString := Format('%.2d day, %.2d hr, %.2d min, %.2d s', [
				dwDays, dwHours, dwMinutes, dwRemainder])
		else if (dwHours > 0) then
			strTimeString := Format('%.2d hr, %.2d min, %.2d s', [
				dwHours, dwMinutes, dwRemainder])
		else if (dwMinutes > 0) then
			strTimeString := Format('%.2d min, %.2d s', [dwMinutes, dwRemainder])
		else
			strTimeString := Format('%.2d s', [dwRemainder]);
		end
	else
		begin
		// Leave out the seconds (ie. smallest unit is minutes)
		if (dwRemainder > 30) then
			Inc(dwMinutes);

		if (dwYears > 0) then
			strTimeString := Format('%.2dyr, %.2dmth, %.2dd, %.2dh, %.2dmin', [
				dwYears, dwMonths, dwDays, dwHours, dwMinutes])
		else if (dwMonths > 0) then
			strTimeString := Format('%.2dmth, %.2dday, %.2dhr, %.2dmin', [
				dwMonths, dwDays, dwHours, dwMinutes])
		else if (dwDays > 0) then
			// Can start to use spaces from here because the length is shorter
			strTimeString := Format('%.2d day, %.2d hr, %.2d min', [dwDays, dwHours, dwMinutes])
		else if (dwHours > 0) then
			strTimeString := Format('%.2d hr, %.2d min', [dwHours, dwMinutes])
		else
			strTimeString := Format('%.2d min', [dwMinutes]);
		end;

	Result := strTimeString;
end;

function GetIsoDateTimeString(dtSource: TDateTime) : String;
begin
	// Return a formatted date/time using the ISO 8601 specification "yyyy-mm-ddThh:nn:ss"
	// Note: You cannot use a single call to "FormatDateTime" as in:
	//		strDateTime := FormatDateTime('yyyy-mm-ddThh:nn:ss', Now());
	// According to the documentation, "T" has a special format meaning (displaying the time using
	// the system ShortTimeFormat setting). We need to construct the string manually.
	Result := (
		FormatDateTime(DATE_FORMAT, dtSource) + 'T' +
		FormatDateTime(TIME_FORMAT, dtSource));
end;

function ConvertTitleCase(const cstrInput: String) : String;
var
	strOutput: String;
begin
	// Convert "CHICKEN" or "chicken" to "Chicken"
	strOutput:= (
		AnsiUppercase(MidStr(cstrInput, 1, 1)) +
		AnsiLowercase(MidStr(cstrInput, 2, Length(cstrInput))));
	Result := strOutput;
end;

function GetRandomString(const cbyLength: BYTE; const cbLettersOnly: Boolean) : String;
var
	strRandom: String;
	astrChars: TStringList;
	byStart, byEnd, byCharacter: BYTE;
begin
	// Generate a randomised string (usually for test purposes)
	strRandom := '';
	if (cbyLength > 0) and (cbyLength < 200) then
		begin
		// Assign allowed characters to a temporary array
		astrChars := TStringList.Create();
		if (cbLettersOnly) then
			begin
			// Lowercase letters only
			byStart := 97;	// a
			byEnd := 122;	// z
			end
		else
			begin
			// Any printable ASCII character
			byStart := 33;	// !
			byEnd := 126;	// ~
			end;

		for byCharacter:=byStart to byEnd do
			astrChars.Add(Chr(byCharacter));

		for byCharacter:=0 to (cbyLength-1) do
			strRandom := (strRandom + astrChars[Random(astrChars.Count)]);

		// Clean up
		astrChars.Free();
		end;

	Result := strRandom;
end;

procedure ParseString(const strSource: String; const strDelimiter: String; list: TStringList);
begin
	// Parse a string based on the delimiter. For example the string "a.b.11.22" is converted
	// into the list {a, b, 11, 22}.

	// A Delphi 7 bug in "ExtractStrings" is apparently addressed in later versions of Delphi. The
	// solution below was suggested in: https://stackoverflow.com/questions/2625707
	list.Text := AnsiReplaceStr(strSource, strDelimiter, #13#10);
end;

// System, math and other general methods
function WithinRect(pt: TPoint; rct: TRect): Boolean;
begin
	// Is the point with the given rectangle?
	Result := False;
	if (	(pt.X >= rct.Left) and (pt.X <= rct.Right) and
			(pt.Y >= rct.Top) and (pt.Y <= rct.Bottom)) then
		Result := True;
end;

function FloatPoint(fX: Single; fY: Single) : TFloatPoint;
var
	fpt: TFloatPoint;
begin
	fpt.X := fX;
	fpt.Y := fY;
	Result := fpt;
end;

function RotatePoint(pt: TPoint; fAngle: Single; ptOrigin: TPoint) : TPoint;
var
	fSin, fCos: Extended;
	fX, fY: Extended;
	ptRotated: TPoint;
begin
	// Rotate a point by a given angle (in radians) around a pivot point (or origin)

	// Calculate trigonmetric values
	fSin := Sin(fAngle);
	fCos := Cos(fAngle);

	// Translate point back to the origin
	pt.X := (pt.X - ptOrigin.X);
	pt.Y := (pt.Y + ptOrigin.Y);

	// Rotate point
	fX := ((fCos * pt.X) - (fSin * pt.Y));
	fY := ((fSin * pt.X) + (fCos * pt.Y));

	// Translate point back to the pivot
	ptRotated.X := (Round(fX) + ptOrigin.X);
	ptRotated.Y := (Round(fY) - ptOrigin.Y);

	// Return result
	Result := ptRotated;
end;

function RotatePoint(fpt: TFloatPoint; fAngle: Single; fptOrigin: TFloatPoint) : TFloatPoint;
var
	fSin, fCos: Extended;
	fptRotated: TFloatPoint;
begin
	// See the "RotatePoint" overload above. This version works with TFloatPoints.
	fSin := Sin(fAngle);
	fCos := Cos(fAngle);

	fpt.X := (fpt.X - fptOrigin.X);
	fpt.Y := (fpt.Y + fptOrigin.Y);

	fptRotated.X := ((fCos * fpt.X) - (fSin * fpt.Y));
	fptRotated.Y := ((fSin * fpt.X) + (fCos * fpt.Y));

	fptRotated.X := (fptRotated.X + fptOrigin.X);
	fptRotated.Y := (fptRotated.Y - fptOrigin.Y);

	Result := fptRotated;
end;

procedure RotatePoints(var pts: array of TPoint; fAngle: Single; ptOrigin: TPoint);
var
	fSin, fCos: Extended;
	nPoint, nX, nY: Integer;
begin
	// As with "RotatePoint", but works on an array of points to rotate
	fSin := Sin(fAngle);
	fCos := Cos(fAngle);
	for nPoint:=Low(pts) to High(pts) do
		begin
		nX := (pts[nPoint].X - ptOrigin.X);
		nY := (pts[nPoint].Y + ptOrigin.Y);
		pts[nPoint].X := (Round((nX * fCos) - (nY * fSin)) + ptOrigin.X);
		pts[nPoint].Y := (Round((nX * fSin) + (nY * fCos)) - ptOrigin.Y);
		end;
end;

procedure RotatePoints(var fpts: array of TFloatPoint; fAngle: Single; fptOrigin: TFloatPoint);
var
	fSin, fCos: Extended;
	nPoint: Integer;
	fX, fY: Single;
begin
	// As with "RotatePoint", but works on an array of points to rotate
	fSin := Sin(fAngle);
	fCos := Cos(fAngle);
	for nPoint:=Low(fpts) to High(fpts) do
		begin
		fX := (fpts[nPoint].X - fptOrigin.X);
		fY := (fpts[nPoint].Y + fptOrigin.Y);
		fpts[nPoint].X := (((fX * fCos) - (fY * fSin)) + fptOrigin.X);
		fpts[nPoint].Y := (((fX * fSin) + (fY * fCos)) - fptOrigin.Y);
		end;
end;

function DegreesToRadians(fDegrees: Single) : Single;
begin
	// Convert an angle in degrees into radians
	Result := ((fDegrees / 180.0) * Pi);
end;

function RadiansToDegrees(fDegrees: Single) : Single;
begin
	// Convert an angle in radians into degrees
	Result := ((fDegrees / Pi) * 180.0);
end;
// End: Public methods

end.
