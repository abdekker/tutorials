unit DelphiClientForm;
{ This test application consuming methods exported from a C++ DLL. MSVC supports two main calling
conventions for functions exported from DLLs: "cdecl" and "stdcall". See ..\SimpleDLL\SimpleDLL.h
for additional details. }

// Uncomment the appropriate line below depending on whether "__cdecl" or "__stdcall" was used in
// the C++ DLL (with "__declspec(dllexport)")
//{$DEFINE DLL_CDECL}
{$DEFINE DLL_STDCALL}

// Choose whether this will be the 32-bit or 64-bit version of the Fibonacci numbers
//{$DEFINE FIBO_32BIT}
{$DEFINE FIBO_64BIT}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

const
  // This project consumes a sample DLL generated using Visual Studio 2019
  DELPI_DLL = 'SimpleDLL.dll';

  // 32/64-bit?
  BIT_DEPTH = {$IFDEF FIBO_32BIT} '32' {$ELSE} '64' {$ENDIF};

type
  TfrmDelphiClient = class(TForm)
	gbSettings: TGroupBox;
	btnUseTestMethods: TButton;
    btnUseFibonacciMethods: TButton;
	memoOutput: TMemo;

	btnExit: TButton;

	procedure FormShow(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure btnUseTestMethodsClick(Sender: TObject);
	procedure btnUseFibonacciMethodsClick(Sender: TObject);

  private
	{ Private declarations }

  public
	{ Public declarations }
  end;

  // Function variables (think of a "pointer to function" without the pointer)
{$IF Defined(DLL_CDECL)}			// cdecl
  // Fibonacci functions
  TFibonacciInit = procedure(a, b:
	{$IFDEF FIBO_32BIT} Cardinal {$ENDIF}
	{$IFDEF FIBO_64BIT} Uint64 {$ENDIF}); cdecl;
  TFibonacciNext = function() : Boolean; cdecl;
  TFibonacciCurrent = function() :
	{$IFDEF FIBO_32BIT} Cardinal {$ENDIF}
	{$IFDEF FIBO_64BIT} Uint64 {$ENDIF}; cdecl;
  TFibonacciIndex = function() : Cardinal; cdecl;

  // Test functions
  TSimpleReturn = function() : Cardinal; cdecl;
  TSimpleSum = function(const a: Integer; const b: Integer) : Integer; cdecl;
  TSimpleMultiply = function(const a: Single; const b: Char) : Single; cdecl;
{$ELSEIF Defined(DLL_STDCALL)}		// stdcall
  TFibonacciInit = procedure(const a, b:
	{$IFDEF FIBO_32BIT} Cardinal {$ELSE} UInt64 {$ENDIF}); stdcall;
  TFibonacciNext = function() : Boolean; stdcall;
  TFibonacciCurrent = function() :
	{$IFDEF FIBO_32BIT} Cardinal {$ELSE} UInt64 {$ENDIF}; stdcall;
  TFibonacciIndex = function() : Cardinal; stdcall;

  TSimpleReturn = function() : Cardinal; stdcall;
  TSimpleSum = function(const a: Integer; const b: Integer) : Integer; stdcall;
  TSimpleMultiply = function(const a: Single; const b: Char) : Single; stdcall;
{$IFEND}

var
  frmDelphiClient: TfrmDelphiClient;

implementation

{$R *.dfm}

procedure TfrmDelphiClient.FormShow(Sender: TObject);
begin
	// Initialise form
	btnUseFibonacciMethods.Caption :=
		{$IFDEF FIBO_32BIT} 'Fibonacci (32-bit)' {$ENDIF}
		{$IFDEF FIBO_64BIT} 'Fibonacci (64-bit)' {$ENDIF};

	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	memoOutput.Lines.Add('Click one of the buttons to use methods from the external DLL...');
	memoOutput.Lines.EndUpdate();

	// Set focus to the Exit button
	btnExit.SetFocus();
end;

procedure TfrmDelphiClient.btnExitClick(Sender: TObject);
begin
	ModalResult := mrOk;
	Close();
end;

procedure TfrmDelphiClient.btnUseTestMethodsClick(Sender: TObject);
var
	hDLL: THandle;
	fSimpleReturn, fNotFound: TSimpleReturn;
	fSimpleSum: TSimpleSum;
	fSimpleMultiply: TSimpleMultiply;
begin
	// Load the external library written in Visual Studio 2019 (this is case insensitive)
	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	memoOutput.Lines.Add('Test: Attempting to load DLL...');
	hDLL := LoadLibrary(DELPI_DLL);
	if (hDLL <> 0) then
		begin
		// Assign functions from the DLL to variables
		// Note: "fNotFound" has been deliberately defined to demonstrate what happens if the
		// function could not be found in the DLL
		memoOutput.Lines.Add('Test: DLL loaded! Attempting to find functions...');
{$IF Defined(DLL_CDECL)}
		@fSimpleReturn := GetProcAddress(hDLL, 'SimpleReturn');
		@fNotFound := GetProcAddress(hDLL, 'SomethingRandom');
		@fSimpleSum := GetProcAddress(hDLL, 'SimpleSum');
		@fSimpleMultiply := GetProcAddress(hDLL, 'SimpleMultiply');
{$ELSEIF Defined(DLL_STDCALL)}
		@fSimpleReturn := GetProcAddress(hDLL, '_SimpleReturn@0');
		@fNotFound := GetProcAddress(hDLL, '_SomethingRandom@0');
		@fSimpleSum := GetProcAddress(hDLL, '_SimpleSum@8');
		@fSimpleMultiply := GetProcAddress(hDLL, '_SimpleMultiply@8');
{$IFEND}

		// Check whether functions were found with either:
		//		found := Assigned(@FUNCTION);
		//		found := (@FUNCTION <> nil);

		// Call methods...
		if(		(Assigned(@fSimpleReturn)) and
				(Assigned(@fSimpleSum)) and
				(Assigned(@fSimpleMultiply))) then
			begin
			memoOutput.Lines.Add('Test: Success! Calling functions from DLL...');
			memoOutput.Lines.Add('');
			memoOutput.Lines.Add(Format('SimpleReturn() = %d', [fSimpleReturn()]));
			memoOutput.Lines.Add(Format('SimpleSum(4, -11) = %d', [fSimpleSum(4, -11)]));
			memoOutput.Lines.Add(
				Format('SimpleMultiply(3.7, 12) = %.1f', [fSimpleMultiply(3.7, Char(12))]));
			end
		else
			memoOutput.Lines.Add('Test: Unable to extract function pointer(s) from DLL');

		// Unload library
		memoOutput.Lines.Add('');
		memoOutput.Lines.Add('Test: DLL unloaded');
		FreeLibrary(hDLL);
		end
	else
		memoOutput.Lines.Add(Format('Test: Unable to locate %s', [DELPI_DLL]));

	memoOutput.Lines.EndUpdate();
end;

procedure TfrmDelphiClient.btnUseFibonacciMethodsClick(Sender: TObject);
var
	hDLL: THandle;
	fFiboInit: TFibonacciInit;
	fFiboNext: TFibonacciNext;
	fFiboCurrent: TFibonacciCurrent;
	fFiboIndex: TFibonacciIndex;
begin
	// Load the external library written in Visual Studio 2019 (this is case insensitive)
	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	memoOutput.Lines.Add('Fibonacci: Attempting to load DLL...');
	hDLL := LoadLibrary(DELPI_DLL);
	if (hDLL <> 0) then
		begin
		// Assign functions from the DLL to the function variables
		// Note: The VS 2019 DLL provides 32-bit and 64-bit versions of the Fibonacci functions
		memoOutput.Lines.Add('Fibonacci: DLL loaded! Attempting to find functions...');

{$IF Defined(DLL_CDECL)}
		@fFiboInit := GetProcAddress(hDLL, 'fibonacciInit' + BIT_DEPTH);
		@fFiboNext := GetProcAddress(hDLL, 'fibonacciNext' + BIT_DEPTH);
		@fFiboCurrent := GetProcAddress(hDLL, 'fibonacciCurrent' + BIT_DEPTH);
		@fFiboIndex := GetProcAddress(hDLL, 'fibonacciIndex' + BIT_DEPTH);
{$ELSEIF Defined(DLL_STDCALL)}
		// Note: Because sizeof(Cardinal) is 4-bytes, the 32-bit version uses "@8". And because
		// SizeOf(UInt64) is 8-bytes, the 64-bit version uses "@16".
		@fFiboInit := GetProcAddress(hDLL,
			{$IFDEF FIBO_32BIT} '_fibonacciInit32@8' {$ENDIF}
			{$IFDEF FIBO_64BIT} '_fibonacciInit64@16' {$ENDIF});
		@fFiboNext := GetProcAddress(hDLL, '_fibonacciNext' + BIT_DEPTH + '@0');
		@fFiboCurrent := GetProcAddress(hDLL, '_fibonacciCurrent' + BIT_DEPTH + '@0');
		@fFiboIndex := GetProcAddress(hDLL, '_fibonacciIndex' + BIT_DEPTH + '@0');
{$IFEND}
		// Call methods...
		if(		(Assigned(@fFiboInit)) and
				(Assigned(@fFiboNext)) and
				(Assigned(@fFiboCurrent)) and
				(Assigned(@fFiboIndex))) then
			begin
			memoOutput.Lines.Add('Fibonacci: Success! Calling functions from DLL...');
			memoOutput.Lines.Add('');

			// Note: Delphi 7 does not have a "IntToStr" overload for signed Int64. We cast the
			// C++ unsigned long long (UInt64) types to a signed Int64. Any values larger than the
			// max Int64 (9223372036854775807) will not be displayed correctly.
			fFiboInit(1, 1);
			repeat
				memoOutput.Lines.Add(Format('%d: %s', [
					fFiboIndex(),
					{$IFDEF FIBO_32BIT} IntToStr(fFiboCurrent()) {$ENDIF}
					{$IFDEF FIBO_64BIT} IntToStr(Int64(fFiboCurrent())) {$ENDIF}]));
			until (not fFiboNext());

			memoOutput.Lines.Add(
				Format('%d Fibonacci values fit in an unsigned %s-bit integer', [
					fFiboIndex() + 1, BIT_DEPTH]));
			end
		else
			memoOutput.Lines.Add('Fibonacci: Unable to extract function pointer(s) from DLL');

		// Unload library
		memoOutput.Lines.Add('');
		memoOutput.Lines.Add('Fibonacci: DLL unloaded');
		FreeLibrary(hDLL);
		end
	else
		memoOutput.Lines.Add(Format('Fibonacci: Unable to locate %s', [DELPI_DLL]));

	memoOutput.Lines.EndUpdate();
end;

end.
