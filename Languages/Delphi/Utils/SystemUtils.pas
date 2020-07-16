{ General system utilities designed for Windows applications }
unit SystemUtils;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Classes, StdCtrls,
  CoreTypes;

const
  // String-to-Number extraction options
  STR_NUMERIC		= $01;	// "0" to "9"
  STR_MINUS			= $02;	// "-"  } Combine these if a plus symbol is likely
  STR_PLUS			= $04;	// "+"  }
  STR_DECIMAL		= $08;	// "."

// Public methods

// Initialisation and cache
procedure InitialiseSystemUtils();
procedure CloseSystemUtils();

// Windows
function IsWindows64Bit() : Boolean;
function IsWindows10() : Boolean;
function GetWindowsLocale() : String;
procedure SetSystem32Path(var strSys32: String; bRedirect: Boolean);
function ExpandEnvironment(const cstrInput: String): String;
function ReplaceEnvironmentVariables(const cstrInput: String): String;
function IsProcessRunning(strProcessName: String) : Boolean;
function GetProcessThreadCount(dwProcessID: DWORD) : Integer;
function GetSystemThreadCount() : Integer;
function IsThreadRunning(dwThreadID: DWORD) : Boolean;
function FindWindowByTitle(hStartHandle: HWND; strWindowTitle: string) : HWND;
procedure GetDiskSpaceGB(const strDrive: String; var fTotalGB: Single; var fTotalFreeGB: Single);
function GetDriveFileSystem(const cstrDrive: String) : String;
function GetSystemDrives(cdwDrives: DWORD = DRIVE_ALL_TYPES) : String;
function CheckDriveIsValid(cDriveLetter: Char) : Boolean;
procedure SaveToClipboard(const cstrText: String);
{$IFDEF DBG} function BreakIfScrollLock() : Boolean; {$ENDIF}

// Registry
function GetRootKey(const cstrKey: String) : HKEY;
function RegGetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	var pValue: Pointer; var dwValSize: Cardinal): Boolean;
function RegGetString(hRootKey: HKEY; const cstrName: String; var strValue: String): Boolean;
function RegGetMultiString(hRootKey: HKEY; const cstrName: String; astrValues: TStringList): Boolean;
function RegGetExpandString(hRootKey: HKEY; const cstrName: String; var strValue: String): Boolean;
function RegGetBinaryAsPointer(hRootKey: HKEY; const cstrName: String;
	var pBuffer: Pointer; var dwBufferSize: Cardinal): Boolean;
function RegGetBinaryAsString(hRootKey: HKEY; const cstrName: String; var strValue: String): Boolean;
function RegGetDWORD(hRootKey: HKEY; const cstrName: String; var dwValue: Cardinal): Boolean;
function RegSetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	pValue: Pointer; dwValueSize: Cardinal): Boolean;
function RegSetString(hRootKey: HKEY; const cstrName: String; strValue: String): Boolean;
function RegSetMultiString(hRootKey: HKEY; const cstrName: String; strValue: String): Boolean;
function RegSetExpandString(hRootKey: HKEY; const cstrName: String; strValue: String): Boolean;
function RegSetDWORD(hRootKey: HKEY; const cstrName: String; dwValue: Cardinal): Boolean;
function RegSetBinary(hRootKey: HKEY; const cstrName: String; abyValue: array of Byte): Boolean;
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
procedure GetListingSubFolders(strParentFolder: String; astrList: TStringList);
procedure GetFolderListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
procedure GetFolderMultipleListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
procedure SortFolderListingByDate(astrList: TStringList; bReverseOrder: Boolean = False);
procedure CopyFilesBetweenFolders(strSourceFolder, strTargetFolder, strWildcard: String);
function DeleteFolder(strFolder: String) : Boolean;
procedure EmptyFolder(strFolder: String);
function IsPathAvailable(const cstrPath: String) : Boolean;
function IsPathWriteable(const cstrPath: String) : Boolean;
function RemovePathExtInfo(strFullPath: String) : String;
function RemovePathInfo(strFullPath: String) : String;
function IsFileExtType(strFullFilename, strTestExt: String): Boolean;
function GetSizeOfFile(strFilename: String) : DWORD;
function GetFullFileVersion(szFile: PChar) : String;
procedure ChangeFilename(strOldPath, strNewPath: String);
function GetNextFilename(strFolder, strName, strExt: String) : String;

// Strings
function IsNumber(const cstrInput: String) : Boolean;
function TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;
function ExtractNumber(const cstrInput: String; byOptions: BYTE = STR_NUMERIC) : String;
function ConvertNumberWithThousands(const cfInput: Single) : String; overload;
function ConvertNumberWithThousands(const cstrInput: String) : String; overload;
function ConvertNumberWithSpaces(const cdwInput: DWORD) : String;
function ConvertNumberWithSigFigures(const cfInput: Single; nTotalDigits: Integer) : String;
function ConvertStringToWideString(const cstrInput: String) : WideString;
function ConvertWideStringToString(const cwstrInput: WideString): String;
function ConvertRawBuffer(pBuffer: PBYTE; const cdwBufferSize: DWORD; const cstrSeparator: String) : String;
function GetTimeStringFromSeconds(dwSeconds: DWORD; bIncludeSeconds: Boolean = True) : String;
function GetIsoDateTimeString(dtSource: TDateTime) : String;
function ConvertTitleCase(const cstrInput: String) : String;
function InsertFormattingChar(const cstrInput: String; const cChar: Char; const cnSpacing: Integer) : String;
procedure ParseString(const cstrSource: String; const strDelimiter: String; list: TStringList);
procedure ParseStringNull(const cstrSource: String; list: TStringList);
function GetRandomString(const cbyLength: BYTE; const cbLettersOnly: Boolean) : String;

// Mathematics and geometry
function WithinRect(pt: TPoint; rct: TRect): Boolean;
function FloatPoint(fX: Single; fY: Single) : TFloatPoint;
function RotatePoint(pt: TPoint; fAngle: Single; ptOrigin: TPoint) : TPoint; overload
function RotatePoint(fpt: TFloatPoint; fAngle: Single; fptOrigin: TFloatPoint) : TFloatPoint; overload
procedure RotatePoints(var pts: array of TPoint; fAngle: Single; ptOrigin: TPoint); overload
procedure RotatePoints(var fpts: array of TFloatPoint; fAngle: Single; fptOrigin: TFloatPoint); overload
function DegreesToRadians(fDegrees: Single) : Single;
function RadiansToDegrees(fDegrees: Single) : Single;
function IsBiggerFloat(fCheckMe, fCheckAgainst: Single; fAccuracy: Single = 0.001) : Boolean;
function GetArrayMin(pbyArray: PBYTE; pdwMinPos: PDWORD; dwCount: DWORD) : BYTE; overload;
function GetArrayMax(pbyArray: PBYTE; pdwMaxPos: PDWORD; dwCount: DWORD) : BYTE; overload;
function GetArrayMin(pnArray: PInteger; pdwMinPos: PDWORD; dwCount: DWORD) : Integer; overload;
function GetArrayMax(pnArray: PInteger; pdwMaxPos: PDWORD; dwCount: DWORD) : Integer; overload;
function GetArrayAverage(pfArray: PSingle; dwCount: DWORD) : Single;
function GetArrayStdDeviation(pfArray: PSingle; dwCount: DWORD; const cfAverage: Single) : Single;
procedure FitLineToData(padY_Values, padX_Values: PDouble; byPoints: BYTE; pdSlope, pdConst: PDouble);
procedure CalculateRSquared(padY_Values, padX_Values: PDouble; byPoints: BYTE;
	pdRSquared: PDouble; dSlope, dConst: Double);

// Validation utility functions (used for validating settings)
procedure Validate_BYTE(pbyVar: PBYTE; byMin, byMax, byDefault: BYTE);
procedure Validate_WORD(pwVar: PWORD; wMin, wMax, wDefault: WORD);
procedure Validate_DWORD(pdwVar: PDWORD; dwMin, dwMax, dwDefault: DWORD);
procedure Validate_ShortInt(pnVar: PShortInt; nMin, nMax, nDefault: ShortInt);
procedure Validate_SmallInt(pnVar: PSmallInt; nMin, nMax, nDefault: SmallInt);
procedure Validate_Integer(pnVar: PInteger; nMin, nMax, nDefault: Integer);
procedure Validate_Single(pfVar: PSingle; fMin, fMax, fDefault: Single);
procedure Validate_Double(pdVar: PDouble; dMin, dMax, dDefault: Double);

implementation

uses
  Clipbrd, Math, Registry, StrUtils, SysUtils, TLHelp32, WinSock;

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

  // Cache
  CACHE_SYSTEM_UTILS = record
	// Ping feature
	hPingHandle: THandle;
  end;

// Private variables
var
  // Cache (used to speed up operations)
  m_cache: CACHE_SYSTEM_UTILS;

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
// Initialisation and cache
procedure InitialiseSystemUtils();
var
	socketData: TWSADATA;
begin
	// Initialise Winsock DLLs
	WSAStartup($101, socketData);

	// Initialise the cache (and other private variables used in this unit)
	ZeroMemory(@m_cache, SizeOf(CACHE_SYSTEM_UTILS));

	// Ping feature
	m_cache.hPingHandle := IcmpCreateFile();
end;

procedure CloseSystemUtils();
begin
	// Release Winsock DLL resources
	WSACleanup();

	// Ping feature
	if (m_cache.hPingHandle <> INVALID_HANDLE_VALUE) then
		IcmpCloseHandle(m_cache.hPingHandle);
end;

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

function GetWindowsLocale() : String;
var
	strLocale: String;
	pData: Pointer;
	dwBufferSize: DWORD;
begin
	// Windows system locale (or code page). This has been tested on Windows XP and 10.
	strLocale := 'Unknown';
	if (RegGetValue(HKEY_LOCAL_MACHINE,
						PChar('SYSTEM\CurrentControlSet\Control\Nls\Language\Default'),
						REG_SZ, pData, dwBufferSize)) then
		begin
		// Got the locale (eg. "0809"), which should be a 4-digit hexadecimal code
		if (dwBufferSize > 1) then
			begin
			SetLength(strLocale, 0);
			SetLength(strLocale, (dwBufferSize-1));
			CopyMemory(@strLocale[1], pData, (dwBufferSize-1));
			end;

		FreeMem(pData);
		end;

	Result := strLocale;
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

function ExpandEnvironment(const cstrInput: String): String;
var
	szResult: array[0..1023] of Char;
	dwReturn: DWORD;
begin
	// Use this function as follows:
	//		strWindowsDir = ExpandEnvironment('%SystemRoot%');
	// which will return "C:\Windows" (no trailing backslash)

	// Alternatively use:
	//		var
	//			szSystemFolder: array[0..(MAX_PATH+1)] of Char;
	//		...
	//		GetWindowsDirectory(szSystemFolder, MAX_PATH+1);
	//		strWindowsDir := IncludeTrailingPathDelimiter(szSystemFolder);
	// which will return "C:\Windows\" (with trailing backslash)
	dwReturn := ExpandEnvironmentStrings(PChar(cstrInput), szResult, 1024);
	if (dwReturn = 0) then
		Result := cstrInput
	else
		Result := Trim(szResult);
end;

function ReplaceEnvironmentVariables(const cstrInput: String): String;
var
	strResult, strTmp: String;
	nPos, nPosStart: Integer;
begin
	// Replace delimited environment variables with their expanded value. For example:
	//		"abc %SystemRoot% 123"
	// is converted to:
	//		"abc C:\Windows 123"

	// Another example is registry key HKEY_CLASSES_ROOT\txtfile\shell\open\command:
	//		"%SystemRoot%\system32\NOTEPAD.EXE %1"
	// which should be converted to:
	//		C:\Windows\system32\NOTEPAD.EXE %1

	// Note: The final "%1" should be left intact! This algorithm may need to be adapted to
	// handle multiple parameters like "%1 %2".
	strResult := '';
	nPosStart := -1;
	for nPos:=1 to Length(cstrInput) do
		begin
		if (cstrInput[nPos] = '%') then
			begin
			// Delimiter
			if (nPosStart = -1) then
				begin
				// Start delimiter
				nPosStart := nPos;
				end
			else
				begin
				// End delimiter
				strTmp := AnsiMidStr(cstrInput, nPosStart, (nPos - nPosStart + 1));
				strResult := (strResult +
					ExpandEnvironment(AnsiMidStr(cstrInput, nPosStart, (nPos - nPosStart + 1))));
				nPosStart := -1;
				end;
			end
		else if (nPosStart = -1) then
			begin
			// Normal character (outside an environment variable)
			strResult := (strResult + cstrInput[nPos]);
			end;
		end;

	// If the position of the starting delimiter is set, we probably have a parameter such as "%1".
	// Add this back into the final result.
	if (nPosStart <> -1) then
		strResult := (strResult +
			 AnsiMidStr(cstrInput, nPosStart, (Length(cstrInput) - nPosStart + 1)));

	// Return the result
	Result := strResult;
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

function GetDriveFileSystem(const cstrDrive: String) : String;
var
	szPartitionType: array[0..32] of Char;
	dwDummy: DWORD;
begin
	// Return the file system (usually NTFS or FAT32). Drive should be in the form "X:\", though
	// "X:" sometimes works.
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

function CheckDriveIsValid(cDriveLetter: Char) : Boolean;
var
	strMask: String[6];
	sRec: TSearchRec;
	dwOldMode: Cardinal;
	nRetCode: Integer;
begin
	// Check whether the given drive letter is valid
	// * Case-insensitive (ie. "C" and "c" are equivalent)
	// * Will not find mapped drives (eg. using "subst P: C:\Tmp")
	dwOldMode := SetErrorMode(SEM_FAILCRITICALERRORS);
	strMask := '?:\*.*';
	strMask[1] := cDriveLetter;

	{$I-} { Don't raise exceptions if we fail! }
	nRetCode := FindFirst(strMask, faAnyfile, sRec);
	if (nRetCode = 0) then
	FindClose(sRec);
	{$I+}

	Result := Abs(nRetCode) in
		[ERROR_SUCCESS, ERROR_FILE_NOT_FOUND, ERROR_NO_MORE_FILES];
	SetErrorMode(dwOldMode);
end;

procedure SaveToClipboard(const cstrText: String);
begin
	// Save some text to the Windows clipboard
	Clipboard.AsText := cstrText;
end;

{$IFDEF DBG}
function BreakIfScrollLock() : Boolean;
begin
	// Debugging feature which forces a breakpoint (by throwing an exception) if Scroll Lock is
	// pressed while debugging the application in Delphi.

	// Note: It is possible to check for the debugger with a call into kernel32.dll:
	// {$IFDEF DBG} function IsDebuggerPresent(): BOOL; external 'kernel32.dll'; {$ENDIF}
	// Using "DebugHook" is simpler.
	Result := False;
	if (	(GetKeyState(VK_SCROLL) = 1) and
			(DebugHook <> 0)) then
		begin
		// Toggle Scroll Lock off and break
		Result := True;
		keybd_event(VK_SCROLL,
			MapVirtualKey(VK_SCROLL, 0), KEYEVENTF_EXTENDEDKEY, 0);
		keybd_event(VK_SCROLL,
			MapVirtualKey(VK_SCROLL, 0), (KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP), 0);
		DebugBreak();
		end;
end;
{$ENDIF}

// Registry
function GetRootKey(const cstrKey: String) : HKEY;
begin
	// Utility function which returns the Windows key for the given key name
	// Note: HKEY_USERS is not supported
	Result := 0;
	if (	(AnsiCompareText(cstrKey, 'HKCR') = 0) or
			(AnsiCompareText(cstrKey, 'HKEY_CLASSES_ROOT') = 0)) then
		Result := HKEY_CLASSES_ROOT
	else if (	(AnsiCompareText(cstrKey, 'HKCU') = 0) or
				(AnsiCompareText(cstrKey, 'HKEY_CURRENT_USER') = 0)) then
		Result := HKEY_CURRENT_USER
	else if (	(AnsiCompareText(cstrKey, 'HKLM') = 0) or
				(AnsiCompareText(cstrKey, 'HKEY_LOCAL_MACHINE') = 0)) then
		Result := HKEY_LOCAL_MACHINE;
end;

function RegGetValue(hRootKey: HKEY; const cstrName: String; dwValType: Cardinal;
	var pValue: Pointer; var dwValSize: Cardinal): Boolean;
var
	strNameReversed, strSubKey: String;
	nPos: Integer;
	dwMyValType, dwBufferSize: DWORD;
	hTemp: HKEY;
	pBuffer: Pointer;
begin
	// Read a value from the registry. Adapted from:
	//		http://www.swissdelphicenter.ch/torry/showcode.php?id=2008

	// Note: To read the value of a key, add a trailing backslash (ie. a blank value). In the
	// Windows registry editor, this has a type of REG_SZ and is displayed as "Name=(Default)".

	// ### Common registry key types ###
	// * REG_SZ			Null-terminated string
	//		Example: "Description"="@combase.dll,-5011"
	// * REG_MULTI_SZ	Sequence of null-terminated strings, terminated by an empty string (\0)
	//		Example: "DependOnService"=hex(7):52,00,70,00,63,00,45,(etc)
	// * REG_EXPAND_SZ	Null-terminated string that contains unexpanded references to environment
	//					variables (for example, "%PATH%")
	//		Example: "ImagePath"=hex(2):25,00,53,00,79,00,73,00,74,(etc)
	// * REG_BINARY		Binary data in any form
	//		Example: "FailureActions"=hex:00,02,00,00,00,60,ea,00,(etc)
	// * REG_DWORD		32-bit number
	//		Example: "Type"=dword:00000020
	// * REG_QWORD		64-bit number (not supported yet)
	//		Example: "MaximumRecordLength"=hex(b):00,d0,88,c3,10,00,00,00
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

function RegGetString(hRootKey: HKEY; const cstrName: String; var strValue: String): Boolean;
var
	pBuffer: Pointer;
	dwBufferSize: Cardinal;
begin
	// Read a REG_SZ value
	Result := False;
	if (RegGetValue(hRootKey, cstrName, REG_SZ, pBuffer, dwBufferSize)) then
		begin
		Dec(dwBufferSize);
		SetLength(strValue, dwBufferSize);
		if (dwBufferSize > 0) then
			CopyMemory(@strValue[1], pBuffer, dwBufferSize);

		FreeMem(pBuffer);
		Result := True;
		end;
end;

function RegGetMultiString(hRootKey: HKEY; const cstrName: String; astrValues: TStringList): Boolean;
var
	pBuffer: Pointer;
	dwBufferSize: Cardinal;
	strValue: String;
begin
	// Read a REG_MULTI_SZ value (sequence of null-terminated strings)
	Result := False;
	if (RegGetValue(hRootKey, cstrName, REG_MULTI_SZ, pBuffer, dwBufferSize)) then
		begin
		Dec(dwBufferSize);
		SetLength(strValue, dwBufferSize);
		if (dwBufferSize > 0) then
			CopyMemory(@strValue[1], pBuffer, dwBufferSize);

		FreeMem(pBuffer);
		ParseStringNull(strValue, astrValues);	// Strings are separated with null characters
		Result := True;
		end;
end;

function RegGetExpandString(hRootKey: HKEY; const cstrName: String; var strValue: String): Boolean;
var
	pBuffer: Pointer;
	dwBufferSize: Cardinal;
begin
	// Read a REG_EXPAND_SZ value (string with references to unexpanded environment variables)
	Result := False;
	if (RegGetValue(hRootKey, cstrName, REG_EXPAND_SZ, pBuffer, dwBufferSize)) then
		begin
		Dec(dwBufferSize);
		SetLength(strValue, dwBufferSize);
		if (dwBufferSize > 0) then
			CopyMemory(@strValue[1], pBuffer, dwBufferSize);

		FreeMem(pBuffer);
		Result := True;
		end;
end;

function RegGetBinaryAsPointer(hRootKey: HKEY; const cstrName: String;
	var pBuffer: Pointer; var dwBufferSize: Cardinal): Boolean;
begin
	// Read a REG_BINARY value (returned as a raw buffer). The caller should call "FreeMem" on the
	// returned buffer to prevent memory leaks.
	// Note: See RegGetBinaryAsString for how this raw buffer might be converted to a string
	Result := False;
	if (RegGetValue(hRootKey, cstrName, REG_BINARY, pBuffer, dwBufferSize)) then
		Result := True;
end;

function RegGetBinaryAsString(hRootKey: HKEY; const cstrName: String; var strValue: String): Boolean;
var
	pBuffer: Pointer;
	dwBufferSize: Cardinal;
begin
	// Read a REG_BINARY value (returned as raw data)
	Result := False;
	if (RegGetValue(hRootKey, cstrName, REG_BINARY, pBuffer, dwBufferSize)) then
		begin
		SetLength(strValue, dwBufferSize);
		CopyMemory(@strValue[1], pBuffer, dwBufferSize);
		FreeMem(pBuffer);
		Result := True;
		end;
end;

function RegGetDWORD(hRootKey: HKEY; const cstrName: String; var dwValue: Cardinal): Boolean;
var
	pBuffer: Pointer;
	dwBufferSize: Cardinal;
begin
	// Read a REG_DWORD value
	Result := False;
	if (RegGetValue(hRootKey, cstrName, REG_DWORD, pBuffer, dwBufferSize)) then
		begin
		CopyMemory(@dwValue, pBuffer, dwBufferSize);
		FreeMem(pBuffer);
		Result := True;
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

function RegSetString(hRootKey: HKEY; const cstrName: String; strValue: String): Boolean;
begin
	// Write a REG_SZ value
	Result := RegSetValue(hRootKey, cstrName, REG_SZ, PChar(strValue + #0), Length(strValue) + 1);
end;

function RegSetMultiString(hRootKey: HKEY; const cstrName: String; strValue: String): Boolean;
begin
	// Write a REG_MULTI_SZ value
	Result := RegSetValue(hRootKey, cstrName, REG_MULTI_SZ, PChar(strValue + #0#0), Length(strValue) + 2);
end;

function RegSetExpandString(hRootKey: HKEY; const cstrName: String; strValue: String): Boolean;
begin
	// Write a REG_MULTI_SZ value
	Result := RegSetValue(hRootKey, cstrName, REG_EXPAND_SZ, PChar(strValue + #0), Length(strValue) + 1);
end;

function RegSetDWORD(hRootKey: HKEY; const cstrName: String; dwValue: Cardinal): Boolean;
begin
	// Write a REG_DWORD value
	Result := RegSetValue(hRootKey, cstrName, REG_DWORD, @dwValue, SizeOf(Cardinal));
end;

function RegSetBinary(hRootKey: HKEY; const cstrName: String; abyValue: array of Byte): Boolean;
begin
	// Write a REG_BINARY value
	Result := RegSetValue(hRootKey, cstrName, REG_BINARY, @abyValue[Low(abyValue)], Length(abyValue));
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

procedure GetListingSubFolders(strParentFolder: String; astrList: TStringList);
var
	find: TSearchRec;
begin
	// Find the name of all sub-folders in the given parent folder (not recursive). Caller should
	// ensure the list is created and empty.

	// Add a final backslash to the path (if required, eg. C:\Temp\)
	if (RightStr(strParentFolder, 1) <> '\') then
		strParentFolder := (strParentFolder + '\');

	if (FindFirst(strParentFolder + '*.*', faDirectory, find) = 0) then
		try
			repeat
				if ((find.Attr and faDirectory) <> 0) and
						(find.Name <> '.') and
						(find.Name <> '..') then
					astrList.AddObject(strParentFolder + find.Name, TObject(find.Time));
			until FindNext(find) <> 0;
		finally
			FindClose(find);
		end;
end;

procedure GetFolderListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
var
	find: TSearchRec;
begin
	// Find all files in a given folder. Caller should ensure the list is created and empty.

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

procedure GetFolderMultipleListing(strFolder, strWildCard: String; astrList: TStringList;
	bRecursive: Boolean = False);
var
	astrFileTypes, astrTmp: TStringList;
	strTypes, strSub: String;
	nPosX, nFileType, nFile: integer;
begin
	// This is related to GetFolderListing except that you can ask for multiple file types,
	// delimited by a semi-colon (;).
	strTypes := strWildCard + ';';

	astrFileTypes := TStringList.Create();
	astrFileTypes.BeginUpdate();
	astrFileTypes.Clear();
	try
		while (Length(strTypes) > 0) do
			begin
			nPosX := Pos(';', strTypes);
			strSub := Copy(strTypes, 0, nPosX-1);
			astrFileTypes.Add(strSub);
			strTypes := Copy(strTypes, nPosX+1, MaxInt);
			end;
	finally
		astrFileTypes.EndUpdate();
	end;

	// Now use GetFolderListing to get a listing for each file type separately
	astrTmp := TStringList.Create();
	for nFileType:=0 to astrFileTypes.Count-1 do
		begin
		astrTmp.Clear();
		GetFolderListing(strFolder, astrFileTypes[nFileType], astrTmp, bRecursive);
		for nFile:=0 to (astrTmp.Count-1) do
			astrList.AddObject(astrTmp[nFile], astrTmp.Objects[nFile]);
		end;

	astrTmp.Free();
	astrFileTypes.Free();
end;

function __SortByFileDate(astrList: TStringList; nIndex1, nIndex2: Integer) : Integer;
var
	dtFile1, dtFile2: TDateTime;
begin
	// Custom sort procedure for ordering files by date
	dtFile1 := FileDateToDateTime(Integer(astrList.Objects[nIndex1]));
	dtFile2 := FileDateToDateTime(Integer(astrList.Objects[nIndex2]));

	Result := 0;
	if (dtFile1 < dtFile2) then
		Result := -1
	else if (dtFile1 > dtFile2) then
		Result := 1;
end;

function __SortByFileDateReverse(astrList: TStringList; nIndex1, nIndex2: Integer) : Integer;
var
	dtFile1, dtFile2: TDateTime;
begin
	// Exactly the same as "__SortByFileDate" but with inverted sorting
	dtFile1 := FileDateToDateTime(Integer(astrList.Objects[nIndex1]));
	dtFile2 := FileDateToDateTime(Integer(astrList.Objects[nIndex2]));

	Result := 0;
	if (dtFile1 > dtFile2) then
		Result := -1
	else if (dtFile1 < dtFile2) then
		Result := 1;
end;

procedure SortFolderListingByDate(astrList: TStringList; bReverseOrder: Boolean = False);
begin
	if (bReverseOrder) then
		astrList.CustomSort(__SortByFileDateReverse)
	else
		astrList.CustomSort(__SortByFileDate);
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

procedure EmptyFolder(strFolder: String);
var
	find: TSearchRec;
begin
	// Delete all files in a folder
	if (FindFirst(strFolder + '\*.*', faAnyFile, find) = 0) then
		begin
		repeat
			if (find.Attr and faDirectory = 0) then
				DeleteFile(strFolder + '\' + find.Name);

		until (FindNext(find) <> 0);

		FindClose(find);
		end;
end;

function IsPathAvailable(const cstrPath: String) : Boolean;
var
	nFreeBytes64, nTotalBytes64, nTotalFreeBytes64: Int64;
begin
	// Check for the existence of some folder
	Result := True;
	if (not GetDiskFreeSpaceEx(PChar(cstrPath), nFreeBytes64, nTotalBytes64,
			PLargeInteger(@nTotalFreeBytes64))) then
		Result := False;
end;

function IsPathWriteable(const cstrPath: String) : Boolean;
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
	// if the user does not have write permissions. Otherwise, this should work.
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

	// Unfortunately, Windows does not set FILE_ATTRIBUTE_READONLY for folders. Attempting to write
	// a file and catching errors works reliably.
end;

function RemovePathExtInfo(strFullPath: String) : String;
var
	nPosPath, nPosExt: Integer;
begin
	// Take a fully qualified path, and remove path and extension information, leaving just the
	// filename. Example: "C:\Temp\MyFile.txt" is converted to "MyFile".
	nPosPath := LastDelimiter(PathDelim + DriveDelim, strFullPath);
	nPosExt := LastDelimiter('.' + PathDelim + DriveDelim, strFullPath);
	Result := Copy(strFullPath, (nPosPath + 1), (nPosExt - nPosPath - 1));
end;

function RemovePathInfo(strFullPath: String) : String;
var
	nPosPath: Integer;
begin
	// Take a fully qualified path, and remove path information, leaving just the filename (or
	// folder name). Example: "C:\Temp\MyFile.txt" is converted to "MyFile.txt".
	nPosPath := LastDelimiter(PathDelim + DriveDelim, strFullPath);
	Result := Copy(strFullPath, (nPosPath + 1), Length(strFullPath));
end;

function IsFileExtType(strFullFilename, strTestExt: String): Boolean;
var
	strExtractedExt: String;
begin
	// Check whether the file has a particular extension (eg. "bmp")
	// Note to developer: Ensure that the test extension is already in lowercase (eg. '.jpg' or
	// similar) to save having to convert case in this method
	Result := False;
	strExtractedExt := AnsiLowerCase(ExtractFileExt(strFullFilename));
	if (AnsiCompareText(strExtractedExt, strTestExt) = 0) then
		Result := True;
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

function GetNextFilename(strFolder, strName, strExt: String) : String;
const
	MAX_ALLOWED_FILE: Integer = 5000;
var
	nFile, nHighestFound, nLowestNotFound: Integer;
	strBase, strFile: String;
	bFoundFileName, bFileExists: Boolean;
begin
	// First check for file0001. If that is missing, then search through all filenames from
	// file0002 to file5000 using an efficient binary search algorithm.

	// Note: We could search for the new filename by checking each file in turn (ie. file0001, then
	// file0002, then file0003, etc). While this is relatively quick when the filename to use is
	// low, it becomes very inefficient as the filename to use gets large. In one test (looking for
	// file4995) the algorithm given below consumed ~1.5ms, but searching one file at a time
	// consumed ~430ms!
	bFoundFileName := True;
	nFile := 1;
	strBase := Format('%s%s', [IncludeTrailingPathDelimiter(strFolder), strName]);
	strFile := Format('%s%4.4d.%s', [strBase, nFile, strExt]);
	bFileExists := FileExists(strFile);
	if (bFileExists) then
		begin
		// File0001 was found, so search for the next available filename
		bFoundFileName := False;
		nHighestFound := 1;
		nLowestNotFound := (MAX_ALLOWED_FILE + 1);
		while ((not bFoundFileName) and (nHighestFound < MAX_ALLOWED_FILE)) do
			begin
			nFile := ((nHighestFound + nLowestNotFound) div 2);
			strFile := Format('%s%4.4d.%s', [strBase, nFile, strExt]);
			bFileExists := FileExists(strFile);
			if (bFileExists) then
				nHighestFound := nFile
			else
				nLowestNotFound := nFile;

			if (nLowestNotFound = (nHighestFound + 1)) then
				begin
				bFoundFileName := True;
				nFile := (nHighestFound + 1);
				end;
			end;
		end;

	// Set the filename to use
	if (bFoundFileName) then
		strFile := Format('%s%4.4d.%s', [strBase, nFile, strExt])
	else
		strFile := Format('%s%4.4d.%s', [strBase, MAX_ALLOWED_FILE, strExt]);

	Result := strFile;
end;

// Strings
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
	//		bSuccess := TryStrToInt(ebValue.Text, nValue);

	// Examples:
	//		TryStrToInt("13", nValue)		True, nValue set to 13
	//		TryStrToInt("1x", nValue)		False, nValue unchanged
	Val(cstrInput, nOutput, nErrorCode);
	Result := (nErrorCode = 0);
end;

function ExtractNumber(const cstrInput: String; byOptions: BYTE = STR_NUMERIC) : String;
var
	strNumber: String;
	nLength, nChar: Integer;
	setAllowed: set of Char;
begin
	// Given the input "-23.7m/min" convert as follows:
	//		STR_NUMERIC										"237"
	//		STR_NUMERIC | STR_WITH_SIGN						"-237"
	//		STR_NUMERIC | STR_DECIMAL						"23.7"
	//		STR_NUMERIC | STR_WITH_SIGN | STR_DECIMAL		"-23.7"
	// Note: The STR_NUMERIC bit should always be set
	strNumber := '';

	// Work out which characters need to be included
	nLength := Length(cstrInput);
	if (nLength > 0) then
		begin
		// Build up the set of allowed characters
		setAllowed := [];
		if ((byOptions and STR_NUMERIC) <> 0) then
			setAllowed := (setAllowed + ['0'..'9']);

		if ((byOptions and STR_MINUS) <> 0) then
			setAllowed := (setAllowed + ['-']);

		if ((byOptions and STR_PLUS) <> 0) then
			setAllowed := (setAllowed + ['+']);

		if ((byOptions and STR_DECIMAL) <> 0) then
			setAllowed := (setAllowed + ['.']);

		// Extract the characters we are looking for
		for nChar:=1 to nLength do
			begin
			if (cstrInput[nChar] in setAllowed) then
				strNumber := (strNumber + cstrInput[nChar]);
			end;
		end
	else
		begin
		// Empty string, so just return zero (if looking for numbers)
		if ((byOptions and STR_NUMERIC) <> 0) then
			strNumber := '0';
		end;

	// Return result; caller will convert from a string into the appropriate type
	Result := strNumber;
end;

function ConvertNumberWithThousands(const cfInput: Single) : String;
begin
	// An alternative implementation is:
	//		strNumber := Format('%n', [cfInput]);
	//		nPosDecimal := Pos(DecimalSeparator, strNumber);
	//		Result := AnsiLeftStr(strNumber, nPosDecimal - 1);
	// But this is slower, uses several local variables doesn't deal correctly with rounding
	Result := FloatToStrF(Round(cfInput), ffNumber, 10, 0);
end;

function ConvertNumberWithThousands(const cstrInput: String) : String;
begin
	// See the other "ConvertNumberWithThousands" overload
	Result := ConvertNumberWithThousands(StrToFloat(cstrInput));
end;

function ConvertNumberWithSpaces(const cdwInput: DWORD) : String;
var
	dwTmp, dwBillions, dwMillions, dwThousands, dwUnits: DWORD;
begin
	// Given the input integer (unsigned 32-bit only!) 1234567, convert into "1 234 567"
	dwTmp := cdwInput;
	dwBillions := (dwTmp div GIGA);
	dwTmp := (dwTmp - (GIGA * dwBillions));
	dwMillions := (dwTmp div MEGA);
	dwTmp := (dwTmp - (MEGA * dwMillions));
	dwThousands := (dwTmp div KILO);
	dwUnits := (dwTmp - (KILO * dwThousands));
	if (dwBillions > 0) then
		Result := Format('%d %.3d %.3d %.3d', [dwBillions, dwMillions, dwThousands, dwUnits])
	else if (dwMillions > 0) then
		Result := Format('%d %.3d %.3d', [dwMillions, dwThousands, dwUnits])
	else if (dwThousands > 0) then
		Result := Format('%d %.3d', [dwThousands, dwUnits])
	else
		Result := IntToStr(dwUnits);
end;

function ConvertNumberWithSigFigures(const cfInput: Single; nTotalDigits: Integer) : String;
var
	nIntegralDigits: Integer;
begin
	// Format a floating point number with a target number of digits (significant figures). Floats
	// have the general format "a.b", where "a" is the "integral" and "b" is the "fractional" part.
	// The total number of digits is (a + b), the sum of digits before and after the decimal point.

	// Examples (with a total of 6 digits):
	//		1.2				formatted as "1.20000"
	//		1.23456789		formatted as "1.23456"
	//		1234.56789		formatted as "1234.56"

	// Note: Negative numbers are not supported at the moment
	Result := '0.0';
	if (cfInput < MIN_SINGLE) then
		Exit;

	nIntegralDigits := (Trunc(Log10(cfInput)) + 1);
	if (nIntegralDigits > nTotalDigits) then
		Result := Format('%.0f', [cfInput])
	else
		Result := FloatToStrF(cfInput, ffFixed, 7, (nTotalDigits - nIntegralDigits));
end;

function ConvertStringToWideString(const cstrInput: String) : WideString;
var
	wstrConverted: WideString;
	nLength: Integer;
begin
	// Given the input AnsiString command, convert to the WideString equivalent
	nLength := MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,
		PChar(@cstrInput[1]), -1, nil, 0);
	SetLength(wstrConverted, (nLength - 1));
	if (nLength > 1) then
		MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,
			PChar(@cstrInput[1]), -1, PWideChar(@wstrConverted[1]), (nLength - 1));

	Result := wstrConverted;
end;

function ConvertWideStringToString(const cwstrInput: WideString): String;
const
	// String <-> WideString conversions
	CONVERT_WIDE_TO_ANSI_OPTIONS: DWORD =
		(WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR);
var
	strConverted: String;
	nLength: Integer;
begin
	nLength := WideCharToMultiByte(CP_ACP, CONVERT_WIDE_TO_ANSI_OPTIONS,
		@cwstrInput[1], -1, nil, 0, nil, nil);
	SetLength(strConverted, (nLength - 1));
	if (nLength > 1) then
		WideCharToMultiByte(CP_ACP, CONVERT_WIDE_TO_ANSI_OPTIONS,
			@cwstrInput[1], - 1, @strConverted[1], nLength - 1, nil, nil);

	Result := strConverted;
end;

function ConvertRawBuffer(pBuffer: PBYTE; const cdwBufferSize: DWORD; const cstrSeparator: String) : String;
var
	strConverted: String;
	dwPos: DWORD;
begin
	// Convert a raw buffer into a string representation, using the given separator. Each byte is
	// converted to its' 2-digit hexadecimal representation (ie. "00" to "FF").
	// Note: The Windows registry displays REG_BINARY lowercase
	strConverted := '';
	for dwPos:=0 to (cdwBufferSize - 1) do
		begin
		strConverted := (strConverted + Format('%2.2x', [BYTE(pBuffer^)]) + cstrSeparator);
		Inc(pBuffer);
		end;

	Result := strConverted;
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

function InsertFormattingChar(const cstrInput: String; const cChar: Char; const cnSpacing: Integer) : String;
var
	strFormatted: String;
	nPos, nSpaces: Integer;
begin
	// Utility function which inserts a formatting character into a string. Example:
	// * You wish to format the MAC address "0052C2539000" as "00-52-C2-53-90-00"
	// * Input = "0052C2539000", character = '-', spacing = 2
	strFormatted := '';
	if (cnSpacing > 0) then
		begin
		nSpaces := 0;
		for nPos:=1 to Length(cstrInput) do
			begin
			if (nSpaces = cnSpacing) then
				begin
				strFormatted := (strFormatted + cChar);
				nSpaces := 0;
				end;

			strFormatted := (strFormatted + cstrInput[nPos]);
			Inc(nSpaces);
			end;
		end;

	Result := strFormatted;
end;

procedure ParseString(const cstrSource: String; const strDelimiter: String; list: TStringList);
begin
	// Parse a string based on the delimiter. For example the string "a.b.11.22" is converted
	// into the list {a, b, 11, 22}.

	// A Delphi 7 bug in "ExtractStrings" is apparently addressed in later versions of Delphi. The
	// solution below was suggested in: https://stackoverflow.com/questions/2625707
	list.Text := AnsiReplaceStr(cstrSource, strDelimiter, #13#10);
end;

procedure ParseStringNull(const cstrSource: String; list: TStringList);
var
	nPos: Integer;
	strTmp: String;
begin
	// Similar to ParseString. Used for strings separated by null (#0) characters.
	for nPos:=1 to Length(cstrSource) do
		begin
		if (cstrSource[nPos] = #0) then
			begin
			list.Add(strTmp);
			strTmp := '';
			end
		else
			strTmp := (strTmp + cstrSource[nPos]);
		end;
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

// Mathematics and geometry
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

function IsBiggerFloat(fCheckMe, fCheckAgainst: Single; fAccuracy: Single = 0.001) : Boolean;
begin
	if ((fCheckMe - fCheckAgainst) > fAccuracy) then
		Result := True
	else
		Result := False;
end;

function GetArrayMin(pbyArray: PBYTE; pdwMinPos: PDWORD; dwCount: DWORD) : BYTE;
var
	byMin: BYTE;
	dwPos: DWORD;
begin
	// Given an array of BYTE values, return the position and value of the minimum element
	// Usage example:
	//		byMin := GetArrayMin(@abyArray, @dwMinPos, SomeCount);
	byMin := 255;
	Result := byMin;
	if (pbyArray = nil) or (pdwMinPos = nil) then
		Exit;

	pdwMinPos^ := 0;
	for dwPos:=0 to (dwCount-1) do
		begin
		if (pbyArray^ < byMin) then
			begin
			byMin := pbyArray^;
			pdwMinPos^ := dwPos;
			end;

		Inc(pbyArray);
		end;

	Result := byMin;
end;

function GetArrayMax(pbyArray: PBYTE; pdwMaxPos: PDWORD; dwCount: DWORD) : BYTE;
var
	byMax: BYTE;
	dwPos: DWORD;
begin
	// Given an array of BYTE values, return the position and value of the maximum element
	byMax := 0;
	Result := byMax;
	if (pbyArray = nil) or (pdwMaxPos = nil) then
		Exit;

	pdwMaxPos^ := 0;
	for dwPos:=0 to (dwCount-1) do
		begin
		if (pbyArray^ > byMax) then
			begin
			byMax := pbyArray^;
			pdwMaxPos^ := dwPos;
			end;

		Inc(pbyArray);
		end;

	Result := byMax;
end;

function GetArrayMin(pnArray: PInteger; pdwMinPos: PDWORD; dwCount: DWORD) : Integer;
var
	nMin: Integer;
	dwPos: DWORD;
begin
	// Array of Integer values
	nMin := 2147483647;
	Result := nMin;
	if (pnArray = nil) or (pdwMinPos = nil) then
		Exit;

	pdwMinPos^ := 0;
	for dwPos:=0 to (dwCount-1) do
		begin
		if (pnArray^ < nMin) then
			begin
			nMin := pnArray^;
			pdwMinPos^ := dwPos;
			end;

		Inc(pnArray);
		end;

	Result := nMin;
end;

function GetArrayMax(pnArray: PInteger; pdwMaxPos: PDWORD; dwCount: DWORD) : Integer;
var
	nMax: Integer;
	dwPos: DWORD;
begin
	// Array of Integer values
	nMax := 0;
	Result := nMax;
	if (pnArray = nil) or (pdwMaxPos = nil) then
		Exit;

	pdwMaxPos^ := 0;
	for dwPos:=0 to (dwCount-1) do
		begin
		if (pnArray^ > nMax) then
			begin
			nMax := pnArray^;
			pdwMaxPos^ := dwPos;
			end;

		Inc(pnArray);
		end;

	Result := nMax;
end;

function GetArrayAverage(pfArray: PSingle; dwCount: DWORD) : Single;
var
	dwElement: DWORD;
	fSum: Single;
begin
	// Calculate the average of a collection of floating point numbers
	Result := 0.0;
	if (pfArray <> nil) and (dwCount > 0) then
		begin
		fSum := 0.0;
		for dwElement:=0 to (dwCount-1) do
			begin
			fSum := (fSum + pfArray^);
			Inc(pfArray);
			end;

		Result := (fSum / dwCount);
		end;
end;

function GetArrayStdDeviation(pfArray: PSingle; dwCount: DWORD; const cfAverage: Single) : Single;
var
	dwElement: DWORD;
	fSum, fDeviation: Single;
begin
	// Calculate the standard deviation of a collection of floating point numbers
	Result := 0.0;
	if (pfArray <> nil) and (dwCount > 0) and (cfAverage > MinSingle) then
		begin
		fSum := 0.0;
		for dwElement:=0 to (dwCount-1) do
			begin
			fDeviation := (pfArray^ - cfAverage);
			fSum := (fSum + (fDeviation*fDeviation));
			Inc(pfArray);
			end;

		Result := Sqrt(fSum / (dwCount - 1));
		end;
end;

procedure FitLineToData(padY_Values, padX_Values: PDouble; byPoints: BYTE; pdSlope, pdConst: PDouble);
var
	byPoint: BYTE;
	dTmpSq, dTmp1, dTmp2, dDeterminantAtA: Double;
	AtA, AtA_Inverse: array[0..1, 0..1] of Double;
	AtB: array[0..1] of Double;
begin
	if (padY_Values = nil) or (padX_Values = nil) or (pdSlope = nil) or (pdConst = nil) then
		Exit;

	// We are fitting an exponential of the form y = ce^(bx). This can be made into a normal linear
	// equation by: ln(y) = bx + ln(c)

	// Calculate the AtA matrix (A-transpose * A) where A is the matrix of x values:
	// [x-1		1]
	// [x-2		1]
	// [...		1]
	// [x-N		1]

	// A-transpose is therefore:
	// [x-1		x-2		...		x-N]
	// [1		1		1		1]
	dTmpSq := 0.0;
	dTmp1 := 0.0;
	dTmp2 := 0.0;
	for byPoint:=0 to (byPoints-1) do
		begin
		dTmpSq := (dTmpSq + Sqr(padX_Values^));
		dTmp1 := (dTmp1 + padX_Values^);
		dTmp2 := (dTmp2 + 1.0);

		Inc(padX_Values);
		end;

	AtA[0, 0] := dTmpSq;
	AtA[0, 1] := dTmp1;
	AtA[1, 0] := dTmp1;
	AtA[1, 1] := dTmp2;

	// Calculate the determinant of the AtA matrix. The determinant of this 2x2 matrix:
	// [a	b]		(1)
	// [c	d]
	// is defined as 1/(ad - bc). Note that the denominator should be non-zero. If it is equal to
	// zero, then the matrix is said to be non-invertible and we cannot calculate a best-fit line.
	// This is not likely.
	try
		dDeterminantAtA := (1.0 / (AtA[0, 0]*AtA[1, 1] - AtA[0, 1]*AtA[1, 0]));
	except
		// Error!
		Exit;
	end;

	// Now invert the AtA matrix. For the matrix (1) given above it is:
	// DETERMINANT		times		[d		-b]
	//								[-c		a]
	AtA_Inverse[0, 0] := (dDeterminantAtA * AtA[1, 1]);
	AtA_Inverse[0, 1] := (-1.0 * dDeterminantAtA * AtA[0, 1]);
	AtA_Inverse[1, 0] := (-1.0 * dDeterminantAtA * AtA[1, 0]);
	AtA_Inverse[1, 1] := (dDeterminantAtA * AtA[0, 0]);

	// Calculate the AtB matrix (A-transpose * B) where B is the matrix of y-values:
	// [y-1]
	// [y-2]
	// [...]
	// [y-N]
	// Note: We take the logarithm of the y values in order to fit the exponential
	Dec(padX_Values, byPoints);
	dTmp1 := 0.0;
	dTmp2 := 0.0;
	for byPoint:=0 to (byPoints-1) do
		begin
		dTmp1 := (dTmp1 + (Ln(padY_Values^) * padX_Values^));
		dTmp2 := (dTmp2 + Ln(padY_Values^));

		Inc(padY_Values);
		Inc(padX_Values);
		end;

	AtB[0] := dTmp1;
	AtB[1] := dTmp2;

	// Finally calculate the slope and constant of best-fit line
	pdSlope^ := (AtA_Inverse[0, 0]*AtB[0] + AtA_Inverse[0, 1]*AtB[1]);
	pdConst^ := (Exp(AtA_Inverse[1, 0]*AtB[0] + AtA_Inverse[1, 1]*AtB[1]));
end;

procedure CalculateRSquared(padY_Values, padX_Values: PDouble; byPoints: BYTE;
	pdRSquared: PDouble; dSlope, dConst: Double);
var
	byPoint: BYTE;
	dTotalY, dMeanY, dCalcY: Double;
	dESS, dTSS: Double;
begin
	if (padY_Values = nil) or (padX_Values = nil) or (pdRSquared = nil) then
		Exit;

	// The best-fit line is defined as the one where the sum of the squares between the actual y
	// values and the calculated y values is minimised.

	// The measure of how well this line fits the data is defined by r^2:
	// r^2 = 1 - (ESS/TSS) where
	// ESS = SUM(yi - y(calc)i)^2 where y(calc) is the calculated y from the best-fit line
	// TSS = SUM(yi - y(mean))^2 where y(mean) is the average of the raw y values

	// Calculate mean
	dTotalY := 0.0;
	for byPoint:=0 to (byPoints-1) do
		begin
		dTotalY := (dTotalY + Ln(padY_Values^));
		Inc(padY_Values);
		end;

	dMeanY := (dTotalY / byPoints);

	// Calculate ESS
	Dec(padY_Values, byPoints);
	dESS := 0.0;
	for byPoint:=0 to (byPoints-1) do
		begin
		dCalcY := (dConst * Exp(dSlope * padX_Values^));
		dESS := (dESS + Sqr(Ln(padY_Values^) - Ln(dCalcY)));

		Inc(padY_Values);
		Inc(padX_Values);
		end;

	// Calculate TSS
	Dec(padY_Values, byPoints);
	dTSS := 0.0;
	for byPoint:=0 to (byPoints-1) do
		begin
		dTSS := (dTSS + Sqr(Ln(padY_Values^) - dMeanY));
		Inc(padY_Values);
		end;

	pdRSquared^ := (1.0 - (dESS / dTSS));
end;

// Validation utility functions (used for validating settings)
procedure Validate_BYTE(pbyVar: PBYTE; byMin, byMax, byDefault: BYTE);
begin
	if (pbyVar = nil) then
		Exit;

	if (pbyVar^ < byMin) or (pbyVar^ > byMax) then
		pbyVar^ := byDefault;
end;

procedure Validate_WORD(pwVar: PWORD; wMin, wMax, wDefault: WORD);
begin
	if (pwVar = nil) then
		Exit;

	if (pwVar^ < wMin) or (pwVar^ > wMax) then
		pwVar^ := wDefault;
end;

procedure Validate_DWORD(pdwVar: PDWORD; dwMin, dwMax, dwDefault: DWORD);
begin
	if (pdwVar = nil) then
		Exit;

	if (pdwVar^ < dwMin) or (pdwVar^ > dwMax) then
		pdwVar^ := dwDefault;
end;

procedure Validate_ShortInt(pnVar: PShortInt; nMin, nMax, nDefault: ShortInt);
begin
	if (pnVar = nil) then
		Exit;

	if (pnVar^ < nMin) or (pnVar^ > nMax) then
		pnVar^ := nDefault;
end;

procedure Validate_SmallInt(pnVar: PSmallInt; nMin, nMax, nDefault: SmallInt);
begin
	if (pnVar = nil) then
		Exit;

	if (pnVar^ < nMin) or (pnVar^ > nMax) then
		pnVar^ := nDefault;
end;

procedure Validate_Integer(pnVar: PInteger; nMin, nMax, nDefault: Integer);
begin
	if (pnVar = nil) then
		Exit;

	if (pnVar^ < nMin) or (pnVar^ > nMax) then
		pnVar^ := nDefault;
end;

procedure Validate_Single(pfVar: PSingle; fMin, fMax, fDefault: Single);
begin
	if (pfVar = nil) then
		Exit;

	if (pfVar^ < fMin) or (pfVar^ > fMax) then
		pfVar^ := fDefault;
end;

procedure Validate_Double(pdVar: PDouble; dMin, dMax, dDefault: Double);
begin
	if (pdVar = nil) then
		Exit;

	if (pdVar^ < dMin) or (pdVar^ > dMax) then
		pdVar^ := dDefault;
end;
// End: Public methods

end.
