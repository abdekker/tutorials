{ Defines types and constants used across several modules and projects }
unit CoreTypes;

interface

uses
  Windows;

const
  // Floating point comparison
  // Note: Consider the floating point comparisons:
  //		A)	if (Abs(fFloat1 - fFloat2) > MIN_SINGLE) then
  //		B)	if (not SameValue(fFloat1, fFloat2, MIN_SINGLE)) then
  // A consumes ~4ns and B consumes ~45ns (Delphi 7). Despite being less efficient, the latter is
  // apparently recommended. This is because when the floats being compared are large compared
  // with epsilon (MIN_SINGLE), the IEEE representation of the 4-byte numbers may result in the
  // comparison always returning TRUE. The subject is complex, but in short, epsilon needs to adapt
  // to the relative sizes of the floats being compared. This is what the Delphi "SameValue"
  // library function does.

  // However, experiments show that A only breaks down somewhere between 1e10-5e10 (ie. very large
  // numbers!). Consequently, method A is suitable for use everywhere in this application.
  MIN_SINGLE: Single		= 0.0000001;
  MIN_DOUBLE: Double		= (1e-9);
  MIN_EXTENDED: Extended	= (1e-11);

  // Metric prefices
  KILO		= Int64(1000);
  MEGA		= Int64(1000 * KILO);
  GIGA		= Int64(1000 * MEGA);
  TERA		= Int64(1000 * GIGA);

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

  // When Windows is shut down, there are two options which are "shutdown" and "reboot"
  WINDOWS_SHUTDOWN		= 0;	// Shut down and turn off computer
  WINDOWS_REBOOT		= 1;	// Shut down and restart Windows

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

  // Character definitions for network messages
  STX	= #02;		// Start delimiter
  ETX	= #03;		// End delimiter
  TAB	= #09;
  US	= #31;		// ASCII Unit Separator (for separating messages); Hex 1F

  // Date / time stamps are always given in ISO 8601 format (yyyy-mm-ddThh:nn:ss) regardless of
  // what regional settings are in use
  DATETIME_FORMAT_ISO8601	= '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d';
  DATE_FORMAT				= 'yyyy-mm-dd';
  TIME_FORMAT				= 'hh:nn:ss';

  // Impulse filter (used for generating a smoothed average for various processes, such as the
  // average processing time used in the setup screens). The impulse filter works by adding a
  // small part of the new reading into the running average for all readings as follows:
  //	NEW_AVERAGE = (IMPULSE_NEW_VALUE * NEW_VALUE) + ((1 - IMPULSE_NEW_VALUE) * OLD_AVERAGE)
  // The sum of the constants should add up to 1.0
  IMPULSE_NEW_VALUE_SLOW: Single	= 0.001;
  IMPULSE_OLD_AVG_SLOW: Single		= 0.999;
  IMPULSE_NEW_VALUE: Single			= 0.01;
  IMPULSE_OLD_AVG: Single			= 0.99;
  IMPULSE_NEW_VALUE_FAST: Single	= 0.05;
  IMPULSE_OLD_AVG_FAST: Single		= 0.95;
  IMPULSE_NEW_VALUE_VFAST: Single	= 0.10;
  IMPULSE_OLD_AVG_VFAST: Single		= 0.90;

  // 8-bit colours
  COLOUR_BLACK			= 0;
  COLOUR_GREY_DARK		= 128;
  COLOUR_GREY_LIGHT		= 192;
  COLOUR_GREY_VLIGHT	= 228;
  COLOUR_WHITE			= 255;

  // User-defined 32-bit colours
  // Note: These are defined in BGR order
  COLOUR_BACKGROUND		= $F9E4DA;		// Light blue/gray colour
  COLOUR_GREEN			= $00C400;		// Darker green then 'Lime'
  COLOUR_DARK_ORANGE	= $0080FF;
  COLOUR_BRIGHT_ORANGE	= $60C0FF;
  COLOUR_PINK			= $FF00FF;		// Background of transparent icons (eg. Zoom exit)
  COLOUR_PASTILLE		= $80FF80;

  // Maximum level of nesting for iterating child controls (eg. setting character set)
  MAX_CHILD_CONTROL_ITERATIONS = 8;

type
  // Types used by both the LomaX4 and XrayMini projects

  // Types required in order to be able to pass parameters as pointers
  PTBitmap = ^TBitmap;

  // Simple procedure declarations used for a CALLBACK (within Delphi)
  // Note: For callbacks used by (say) an external C++ DLL, these need to be defined differently
  // and use the "cdecl" calling convention. See "RegisterCallbackPLC" in the PLC_HV_Comm unit for
  // examples of how this is done.
  TProcedureCallback = procedure() of object;
  TProcedureCallbackInt = procedure(nValue: Integer) of object;
  TProcedureCallbackDWORD = procedure(dwValue: DWORD) of object;

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

  // Structure used to read memory from Windows
  _MEMORYSTATUSEX = packed record
	dwLength: DWORD;
	dwMemoryLoad: DWORD;
	nTotalPhys64, nAvailPhys64: Int64;
	nTotalPageFile64, nAvailPageFile64: Int64;
	nTotalVirtual64, nAvailVirtual64, nAvailExtendedVirtual64: Int64;
  end;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  LPMEMORYSTATUSEX = ^_MEMORYSTATUSEX;

  // Generic linked list (or container) node for pointers
  PTPOINTERNODE = ^TPOINTERNODE;
  TPOINTERNODE = record
	pData: Pointer;
	pNextLink, pPreviousLink: PTPOINTERNODE;
  end;

  // Generic DWORD linked list type
  PTDWORDLIST = ^TDWORDLIST;
  TDWORDLIST = record
	dwValue: DWORD;
	pNextLink: PTDWORDLIST;
	pPreviousLink: PTDWORDLIST;
  end;

  // Histogram (8-bit)
  PTHISTOGRAM = ^THISTOGRAM;
  THISTOGRAM = record
	adwHistogram: array[0..255] of DWORD;

	dwArea, dwVolume: DWORD;
	fVolumeSkew: Single;
	dwTotalGreyLevel, dwModeCount: DWORD;
	byModeBin, byMinBin, byMaxBin, byMedian: BYTE;
	fMean, fStdDev: Single;
  end;

  // Image (8-bit and 24-bit)
  PTIMAGECELL = ^TIMAGECELL;
  PPTIMAGECELL = ^PTIMAGECELL;
  TIMAGECELL = record
	handleDC: HDC;
	handleBitmap: HBITMAP;
	bmi: BITMAPINFO;
	pBitmapBits: PBYTE;
	b24Valid: Boolean;
	pBits: PBYTE;
	nWidth, nHeight: Integer;
	dwByteWidth, dwSize: DWORD;

	Histogram: THISTOGRAM;
  end;

  // OpenGL state (for 3D view)
  PTOGL_STATE = ^TOGL_STATE;
  TOGL_STATE = record
	hDC: HDC;
	hRC: HGLRC;
	hInstance: THANDLE;
	hWnd: HWND;
	bFullScreen: Integer;
	nWidth: Integer;
	nHeight: Integer;
  end;

implementation

end.
