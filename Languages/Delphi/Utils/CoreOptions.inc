// General include and compiler options for Delphi 7 projects

// Debug or non-debug build?
{$IF Defined(DEBUG)}
	{$DEFINE DBG}		// Debug build
{$ELSEIF Defined(NDEBUG)}
	{$DEFINE NDBG}		// Non-debug (ie. release) build
{$IFEND}

// Compiler switches:
//								DEBUG		RELEASE
// Debug Info ($D)				On			Off
// Optimisation ($O)			Off			On
// Range Checking ($R)			On			Off
// I/O Checking ($I)			On			Off
// Overflow Checking ($Q)		On			Off
{$IFDEF DBG}
	{$D+}
	{$O-}
	{$R+}
	{$I+}
	{$Q+}
{$ELSE}
	{$D-}
	{$O+}
	{$R-}
	{$I-}
	{$Q-}
{$ENDIF}

{ Notes to developer about compiler switches:
* Range Checking ($R)
	You can check if enabled (ie. $R+) with the following code:
		byByteValue: BYTE;
		abyBytes: array[1..2] of BYTE;
		//...
		byByteValue := 3;
		abyBytes[byByteValue] := 7;		// Index needs to be a variable to trick the compiler
	If enabled, you get "Range check error". If not, there is often no visible effect although the
	application may throw an exception or terminate suddenly (if other memory was accessed).

* Overflow Checking ($Q)
	You can check if enabled (ie. $Q+) with the following code:
		byByteValue: BYTE;
		//...
		byByteValue := 255;
		Inc(byByteValue);
	If enabled, you get an "Integer overflow" error. If not, byByteValue is set to 0.
}

{ To remove debug information in RELEASE mode (see Project > Options > Compiler > Debugging),
uncomment these lines. These options have no effect on the size or speed of the executable. Debug
information is added to the DCU (object) files. Removing the options (very slightly) improves
compile / link time and prevents the developer debugging in RELEASE mode (perhaps as a reminder to
use DEBUG mode only?). However, overall it is useful to have good post mortem debug information, so
leave these commented out in normal operation. }
//{$IFDEF NDBG}
//	{$D-}		// Remove "Debug information"
//	{$L-}		// Remove "Local symbols"
//	{$Y-}		// Remove "Reference info/Definitions only"
//	{$C-}		// Remove "Assertions"
//{$ENDIF}

