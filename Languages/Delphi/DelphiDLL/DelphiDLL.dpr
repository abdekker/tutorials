library DelphiDLL;

{ This is a toy DLL to demonstrate the use of DLLs. The libary export two functions Max and Min.
The DLL matches the DelphiMainForm which is a Delphi project which calls the DLL. }

{ Automatically inserted by Delphi 7:
Important note about DLL memory management: ShareMem must be the first unit in your library's USES
clause AND your project's (select Project-View Source) USES clause if your DLL exports any
procedures or functions that pass strings as parameters or function results. This applies to all
strings passed to and from your DLL--even those that are nested in records and classes. ShareMem is
the interface unit to the BORLNDMM.DLL shared memory manager, which must be deployed along with
your DLL. To avoid using BORLNDMM.DLL, pass string information using PChar or ShortString
parameters. }

uses
  SysUtils,
  Classes;

{$R *.res}

// Declare with "stdcall" to interface with other languages
function Min(X, Y: Integer) : Integer; stdcall;
begin
	if (X < Y) then Min := X else Min := Y;
end;

function Max(X, Y: Integer) : Integer; stdcall;
begin
	if (X > Y) then Max := X else Max := Y;
end;

exports
	// Make available to calling applications
	Min name 'Min',
	Max name 'Max';

{ Notes:
* Defining exports with parameters is only required for overloaded functions. An example:
	exports
		Divide(X, Y: Integer) name 'Divide_Ints',
		Divide(X, Y: Real) name 'Divide_Reals';
* Using "index" is specific to Windows and does not appear to have any effect. An example:
	exports
		Min index 1 name 'Min',
		Max index 2 name 'Max';
}

begin
end.
