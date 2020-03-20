{ Test application which consumes methods exported from a C++ DLL. MSVC supports two main calling
conventions for functions exported from DLLs: "cdecl" and "stdcall". See ..\SimpleDLL\SimpleDLL.h
for additional details.

An exported function is sometimes referred to as the "procedure entry point" in Delphi }
unit DelphiClientForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

// There is a D7 bug related to using UInt64 with compiler range-checking. The CoreOptions include
// switches range-checking on for DEBUG builds, so explicitly toggle it off in this project.
{$IFDEF DBG} {$R-} {$ENDIF}

{ Uncomment for the relevant calling convention used in the C++ DLL with "__declspec(dllexport)"
	* use DLL_CDECL for "__cdecl"
	* use DLL_STDCALL for"__stdcall" }
//{$DEFINE DLL_CDECL}
{$DEFINE DLL_STDCALL}

interface

uses
  Controls, Classes, Forms, StdCtrls,
  FormUtils;

const
  // This project consumes a sample C++ DLL generated in Visual Studio 2019
  DLL_NAME = 'SimpleDLL.dll';

  // Fibonacci functions exported from the C++ DLL
  // Note: Sizeof(Cardinal) is 4-bytes, so the 32-bit version of "fibonacciInit" uses "@8". Since
  // sizeOf(UInt64) is 8-bytes, the 64-bit version uses "@16".
  FIBO_INIT32 =
	{$IF Defined(DLL_CDECL)} 'fibonacciInit32'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciInit32@8' {$IFEND};
  FIBO_INIT64 =
	{$IF Defined(DLL_CDECL)} 'fibonacciInit64'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciInit64@16' {$IFEND};

  FIBO_NEXT32 =
	{$IF Defined(DLL_CDECL)} 'fibonacciNext32'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciNext32@0' {$IFEND};
  FIBO_NEXT64 =
	{$IF Defined(DLL_CDECL)} 'fibonacciNext64'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciNext64@0' {$IFEND};

  FIBO_CURRENT32 =
	{$IF Defined(DLL_CDECL)} 'fibonacciCurrent32'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciCurrent32@0' {$IFEND};
  FIBO_CURRENT64 =
	{$IF Defined(DLL_CDECL)} 'fibonacciCurrent64'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciCurrent64@0' {$IFEND};

  // The same method is used for both the 32-bit and 64-bit Fibonacci sequences
  FIBO_INDEX =
	{$IF Defined(DLL_CDECL)} 'fibonacciIndex'
	{$ELSEIF Defined(DLL_STDCALL)} '_fibonacciIndex@0' {$IFEND};

  // Test functions exported from the C++ DLL
  TEST_RETURN =
	{$IF Defined(DLL_CDECL)} 'SimpleReturn'
	{$ELSEIF Defined(DLL_STDCALL)} '_SimpleReturn@0' {$IFEND};
  TEST_SUM =
	{$IF Defined(DLL_CDECL)} 'SimpleSum'
	{$ELSEIF Defined(DLL_STDCALL)} '_SimpleSum@8' {$IFEND};
  TEST_MULTIPLY =
	{$IF Defined(DLL_CDECL)} 'SimpleMultiply'
	{$ELSEIF Defined(DLL_STDCALL)} '_SimpleMultiply@8' {$IFEND};

  // Test for a function that is deliberately NOT defined / exported
  TEST_NOT_IN_DLL = 'MissingInAction';

type
  CACHE_DELPHI_CLIENT = record
	szMsgErrUnableToLocateDLL: String;
  end;

  TfrmDelphiClient = class(TForm)
	gbSettings: TGroupBox;

	lblFunctionMethod: TLabel;
	ddlFunctionMethod: TComboBox;
	lblFibonacciBitDepth: TLabel;
	rbFibonacciBitDepth32: TRadioButton;
	rbFibonacciBitDepth64: TRadioButton;
	btnUseTestMethods: TButton;
	btnUseFibonacciMethods: TButton;
	memoOutput: TMemo;

	btnExit: TButton;

	procedure FormCreate(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure rbFibonacciBitDepthClick(Sender: TObject);
	procedure btnUseTestMethodsClick(Sender: TObject);
	procedure btnUseFibonacciMethodsClick(Sender: TObject);

  private
	{ Private declarations }
	m_cache: CACHE_DELPHI_CLIENT;

	procedure UseTestMethodsManual();
	procedure UseTestMethodsStatic();

	procedure SetFibonacciButtonText();
	procedure UseFibonacciMethodsManual32();
	procedure UseFibonacciMethodsManual64();
	procedure UseFibonacciMethodsStatic32();
	procedure UseFibonacciMethodsStatic64();

  public
	{ Public declarations }
  end;

  // Function variables (think of a "pointer to function" without the pointer)
{$IF Defined(DLL_CDECL)}			// cdecl
  // Fibonacci functions
  TFibonacciInit32 = procedure(a, b: Cardinal); cdecl;
  TFibonacciInit64 = procedure(a, b: UInt64); cdecl;
  TFibonacciNext = function() : Boolean; cdecl;
  TFibonacciCurrent32 = function() : Cardinal; cdecl;
  TFibonacciCurrent64 = function() : Uint64; cdecl;
  TFibonacciIndex = function() : Cardinal; cdecl;

  // Test functions
  TSimpleReturn = function() : Cardinal; cdecl;
  TSimpleSum = function(const a: Integer; const b: Integer) : Integer; cdecl;
  TSimpleMultiply = function(const a: Single; const b: Char) : Single; cdecl;
{$ELSEIF Defined(DLL_STDCALL)}		// stdcall
  TFibonacciInit32 = procedure(a, b: Cardinal); stdcall;
  TFibonacciInit64 = procedure(a, b: UInt64); stdcall;
  TFibonacciNext = function() : Boolean; stdcall;
  TFibonacciCurrent32 = function() : Cardinal; stdcall;
  TFibonacciCurrent64 = function() : Uint64; stdcall;
  TFibonacciIndex = function() : Cardinal; stdcall;

  TSimpleReturn = function() : Cardinal; stdcall;
  TSimpleSum = function(const a: Integer; const b: Integer) : Integer; stdcall;
  TSimpleMultiply = function(const a: Single; const b: Char) : Single; stdcall;
{$IFEND}

var
  frmDelphiClient: TfrmDelphiClient;

implementation

uses
  Windows, Math, SysUtils;

const
  MSG_LOAD_DLL					= 'Load DLL...';
  MSG_FIND_FUNCTIONS			= 'Success! Attempting to find functions...';
  MSG_CALL_FUNCTIONS			= 'Success! Calling functions from DLL...';
  MSG_UNLOAD_DLL				= 'Unload DLL...';
  MSG_UNLOAD_DLL_SUCCESS		= 'Success! DLL unloaded';

  MSG_ERR_EXTRACT_FUNCTIONS		= 'Unable to extract function pointer(s) from DLL';
  // m_cache.szMsgErrUnableToLocateDLL

type
  // Enumeration
  TFunctionsExtraction = (eFunctionsManual, eFunctionsStatic);

  // Static function declarations (alternative to using LoadLibrary / GetProcAddress)

{$IF Defined(DLL_CDECL)}			// cdecl
  // Fibonacci functions
  procedure fibonacciInit32(const a, b: Cardinal); cdecl; external DLL_NAME name FIBO_INIT32;
  procedure fibonacciInit64(const a, b: UInt64); cdecl; external DLL_NAME name FIBO_INIT64;
  function fibonacciNext32() : Boolean; cdecl; external DLL_NAME name FIBO_NEXT32;
  function fibonacciNext64() : Boolean; cdecl; external DLL_NAME name FIBO_NEXT64;
  function fibonacciCurrent32() : Cardinal; cdecl; external DLL_NAME name FIBO_CURRENT32;
  function fibonacciCurrent64() : UInt64; cdecl; external DLL_NAME name FIBO_CURRENT64;
  function fibonacciIndex() : Cardinal; cdecl; external DLL_NAME name FIBO_INDEX;

  // Test functions
  function SimpleReturn() : Cardinal; cdecl; external DLL_NAME name TEST_RETURN;
  function SimpleSum(const a, b: Integer) : Integer; cdecl; external DLL_NAME name TEST_SUM;
  function SimpleMultiply(const a: Single; const b: Char) : Single; cdecl;
	external DLL_NAME name TEST_MULTIPLY;
  function NotInDLL() : Cardinal; cdecl; external DLL_NAME name TEST_NOT_IN_DLL;
{$ELSEIF Defined(DLL_STDCALL)}		// stdcall
  procedure fibonacciInit32(const a, b: Cardinal); stdcall; external DLL_NAME name FIBO_INIT32;
  procedure fibonacciInit64(const a, b: UInt64); stdcall; external DLL_NAME name FIBO_INIT64;
  function fibonacciNext32() : Boolean; stdcall; external DLL_NAME name FIBO_NEXT32;
  function fibonacciNext64() : Boolean; stdcall; external DLL_NAME name FIBO_NEXT64;
  function fibonacciCurrent32() : Cardinal; stdcall; external DLL_NAME name FIBO_CURRENT32;
  function fibonacciCurrent64() : UInt64; stdcall; external DLL_NAME name FIBO_CURRENT64;
  function fibonacciIndex() : Cardinal; stdcall; external DLL_NAME name FIBO_INDEX;

  function SimpleReturn() : Cardinal; stdcall; external DLL_NAME name TEST_RETURN;
  function SimpleSum(const a, b: Integer) : Integer; stdcall; external DLL_NAME name TEST_SUM;
  function SimpleMultiply(const a: Single; const b: Char) : Single; stdcall;
	external DLL_NAME name TEST_MULTIPLY;
  function NotInDLL() : Cardinal; stdcall; external DLL_NAME name TEST_NOT_IN_DLL;
{$IFEND}

{$R *.dfm}

procedure TfrmDelphiClient.FormCreate(Sender: TObject);
begin
	// Initialise cache
	ZeroMemory(@m_cache, SizeOf(CACHE_DELPHI_CLIENT));
	m_cache.szMsgErrUnableToLocateDLL := Format('Unable to locate %s', [DLL_NAME]);
end;

procedure TfrmDelphiClient.FormShow(Sender: TObject);
begin
	// Initialise form
	SetComboHandlers(ddlFunctionMethod);
	ddlFunctionMethod.Items.Clear();
	ddlFunctionMethod.Items.AddObject('LoadLibrary / GetProcAddress', TObject(eFunctionsManual));
	ddlFunctionMethod.Items.AddObject('Statically-defined entry points', TObject(eFunctionsStatic));
	ddlFunctionMethod.ItemIndex := 0;

	rbFibonacciBitDepth32.Checked := True;
	SetFibonacciButtonText();

	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	memoOutput.Lines.Add('Click one of the buttons to use methods from an external DLL...');
	memoOutput.Lines.EndUpdate();

	// Set focus to the Exit button
	btnExit.SetFocus();
end;

procedure TfrmDelphiClient.btnExitClick(Sender: TObject);
begin
	ModalResult := mrOk;
	Close();
end;

procedure TfrmDelphiClient.rbFibonacciBitDepthClick(Sender: TObject);
begin
	// Update the text on the Fibonacci button
	SetFibonacciButtonText();
end;

procedure TfrmDelphiClient.btnUseTestMethodsClick(Sender: TObject);
var
	eExtraction: TFunctionsExtraction;
begin
	// Get the function extraction method and the bit depth
	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	eExtraction :=
		TFunctionsExtraction(ddlFunctionMethod.Items.Objects[ddlFunctionMethod.ItemIndex]);

	if (eExtraction = eFunctionsManual) then
		begin
		// Manual extraction of functions using LoadLibrary and GetProcAddress
		UseTestMethodsManual();
		end
	else if (eExtraction = eFunctionsStatic) then
		begin
		// Statically-defined procedure entry points
		UseTestMethodsStatic();
		end;

	ScrollMemoLastLine(memoOutput.Handle, memoOutput.Lines.Count);
	memoOutput.Lines.EndUpdate();
end;

procedure TfrmDelphiClient.btnUseFibonacciMethodsClick(Sender: TObject);
var
	eExtraction: TFunctionsExtraction;
	nBitDepth: Integer;
begin
	// Get the function extraction method and the bit depth
	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	eExtraction :=
		TFunctionsExtraction(ddlFunctionMethod.Items.Objects[ddlFunctionMethod.ItemIndex]);
	nBitDepth := IfThen(rbFibonacciBitDepth32.Checked, 32, 64);

	if (eExtraction = eFunctionsManual) then
		begin
		// Manual extraction of functions using LoadLibrary and GetProcAddress
		if (nBitDepth = 32) then
			UseFibonacciMethodsManual32()
		else if (nBitDepth = 64) then
			UseFibonacciMethodsManual64();
		end
	else if (eExtraction = eFunctionsStatic) then
		begin
		// Statically-defined procedure entry points
		if (nBitDepth = 32) then
			UseFibonacciMethodsStatic32()
		else if (nBitDepth = 64) then
			UseFibonacciMethodsStatic64();
		end;

	ScrollMemoLastLine(memoOutput.Handle, memoOutput.Lines.Count);
	memoOutput.Lines.EndUpdate();
end;

// Start: Private functions
procedure TfrmDelphiClient.UseTestMethodsManual();
var
	hDLL: THandle;
	fSimpleReturn, fNotInDLL: TSimpleReturn;
	fSimpleSum: TSimpleSum;
	fSimpleMultiply: TSimpleMultiply;
begin
	// Test methods with manual extraction
	memoOutput.Lines.Add('Test methods...run-time, manual extraction of function pointers');
	memoOutput.Lines.Add('');

	// Load the external library written in Visual Studio 2019 (this is case insensitive)
	memoOutput.Lines.Add(MSG_LOAD_DLL);
	hDLL := LoadLibrary(DLL_NAME);
	if (hDLL <> 0) then
		begin
		// Assign functions from the DLL to variables
		memoOutput.Lines.Add(MSG_FIND_FUNCTIONS);
		@fSimpleReturn := GetProcAddress(hDLL, TEST_RETURN);
		@fSimpleSum := GetProcAddress(hDLL, TEST_SUM);
		@fSimpleMultiply := GetProcAddress(hDLL, TEST_MULTIPLY);

		// Check whether functions were found with either:
		//		found := Assigned(@FUNCTION);
		//		found := (@FUNCTION <> nil);

		// Call methods...
		if(		(Assigned(@fSimpleReturn)) and
				(Assigned(@fSimpleSum)) and
				(Assigned(@fSimpleMultiply))) then
			begin
			memoOutput.Lines.Add(MSG_CALL_FUNCTIONS);
			memoOutput.Lines.Add('');
			memoOutput.Lines.Add(Format('SimpleReturn() = %d', [fSimpleReturn()]));
			memoOutput.Lines.Add(Format('SimpleSum(4, -11) = %d', [fSimpleSum(4, -11)]));
			memoOutput.Lines.Add(
				Format('SimpleMultiply(3.7, 12) = %.1f', [fSimpleMultiply(3.7, Char(12))]));
			end
		else
			memoOutput.Lines.Add(MSG_ERR_EXTRACT_FUNCTIONS);

		// Demonstrate using a method that is not defined / exported in the DLL
		memoOutput.Lines.Add('');
		try
			@fNotInDLL := GetProcAddress(hDLL, TEST_NOT_IN_DLL);
			if (Assigned(@fNotInDLL)) then
				memoOutput.Lines.Add(Format('Method %s found...huh?', [TEST_NOT_IN_DLL]))
			else
				begin
				memoOutput.Lines.Add(
					Format('Method "%s" was not found in DLL...but use it anyway!', [TEST_NOT_IN_DLL]));
				fNotInDLL();
				end;
		except
			on E: Exception do memoOutput.Lines.Add(Format('%s.', [E.Message]));
		end;

		// Unload library
		memoOutput.Lines.Add('');
		memoOutput.Lines.Add(MSG_UNLOAD_DLL);
		FreeLibrary(hDLL);
		memoOutput.Lines.Add(MSG_UNLOAD_DLL_SUCCESS);
		end
	else
		memoOutput.Lines.Add(m_cache.szMsgErrUnableToLocateDLL);

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('Test...manual...complete');
end;

procedure TfrmDelphiClient.UseTestMethodsStatic();
begin
	// Test methods with static definition
	memoOutput.Lines.Add('Test methods...compile-time, statically-defined procedure entry points');
	memoOutput.Lines.Add('');

	// Call methods...
	memoOutput.Lines.Add(Format('SimpleReturn() = %d', [SimpleReturn()]));
	memoOutput.Lines.Add(Format('SimpleSum(4, -11) = %d', [SimpleSum(4, -11)]));
	memoOutput.Lines.Add(
		Format('SimpleMultiply(3.7, 12) = %.1f', [SimpleMultiply(3.7, Char(12))]));

	// "UseTestMethodsManual" demonstrated using a method not found in the DLL. With (compile-time)
	// static functions this is not possible because the program crashes immediately (possibly as a
	// safety feature). This is one advantage of using compile-time functions...you find out about
	// the missing procedure entry point problem before even attempting to use the method!
	{try
		NotInDLL();
	except
		on E: Exception do memoOutput.Lines.Add(Format('%s.', [E.Message]));
	end;}

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('Test...static...complete');
end;

procedure TfrmDelphiClient.SetFibonacciButtonText();
var
	nBitDepth: Integer;
begin
	nBitDepth := IfThen(rbFibonacciBitDepth32.Checked, 32, 64);
	btnUseFibonacciMethods.Caption := Format('Fibonacci (%d-bit)', [nBitDepth]);
end;

procedure TfrmDelphiClient.UseFibonacciMethodsManual32();
var
	hDLL: THandle;
	fFiboInit: TFibonacciInit32;
	fFiboNext: TFibonacciNext;
	fFiboCurrent: TFibonacciCurrent32;
	fFiboIndex: TFibonacciIndex;
begin
	// 32-bit Fibonacci methods with manual extraction
	// Note: The VS 2019 DLL provides 32-bit and 64-bit versions of the Fibonacci functions
	memoOutput.Lines.Add('32-bit Fibonacci methods...run-time, manual extraction of function pointers');
	memoOutput.Lines.Add('');
	memoOutput.Lines.Add(MSG_LOAD_DLL);
	hDLL := LoadLibrary(DLL_NAME);
	if (hDLL <> 0) then
		begin
		// Assign functions from the DLL to the function variables
		memoOutput.Lines.Add(MSG_FIND_FUNCTIONS);
		@fFiboInit := GetProcAddress(hDLL, FIBO_INIT32);
		@fFiboNext := GetProcAddress(hDLL, FIBO_NEXT32);
		@fFiboCurrent := GetProcAddress(hDLL, FIBO_CURRENT32);
		@fFiboIndex := GetProcAddress(hDLL, FIBO_INDEX);

		// Call methods...
		if(		(Assigned(@fFiboInit)) and
				(Assigned(@fFiboNext)) and
				(Assigned(@fFiboCurrent)) and
				(Assigned(@fFiboIndex))) then
			begin
			memoOutput.Lines.Add(MSG_CALL_FUNCTIONS);
			memoOutput.Lines.Add('');

			fFiboInit(1, 1);
			repeat
				memoOutput.Lines.Add(Format('%d: %s', [
					fFiboIndex(),
					IntToStr(fFiboCurrent())]));
			until (not fFiboNext());

			memoOutput.Lines.Add(
				Format('%d Fibonacci values fit in an unsigned 32-bit integer', [fFiboIndex() + 1]));
			end
		else
			memoOutput.Lines.Add(MSG_ERR_EXTRACT_FUNCTIONS);

		// Unload library
		memoOutput.Lines.Add('');
		memoOutput.Lines.Add(MSG_UNLOAD_DLL);
		FreeLibrary(hDLL);
		memoOutput.Lines.Add(MSG_UNLOAD_DLL_SUCCESS);
		end
	else
		memoOutput.Lines.Add(m_cache.szMsgErrUnableToLocateDLL);

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('32-bit Fibonacci...manual...complete');
end;

procedure TfrmDelphiClient.UseFibonacciMethodsManual64();
var
	hDLL: THandle;
	fFiboInit: TFibonacciInit64;
	fFiboNext: TFibonacciNext;
	fFiboCurrent: TFibonacciCurrent64;
	fFiboIndex: TFibonacciIndex;
begin
	// 64-bit Fibonacci methods with manual extraction
	memoOutput.Lines.Add('64-bit Fibonacci methods...run-time, manual extraction of function pointers');
	memoOutput.Lines.Add('');
	memoOutput.Lines.Add(MSG_LOAD_DLL);
	hDLL := LoadLibrary(DLL_NAME);
	if (hDLL <> 0) then
		begin
		// Assign functions from the DLL to the function variables
		memoOutput.Lines.Add(MSG_FIND_FUNCTIONS);
		@fFiboInit := GetProcAddress(hDLL, FIBO_INIT64);
		@fFiboNext := GetProcAddress(hDLL, FIBO_NEXT64);
		@fFiboCurrent := GetProcAddress(hDLL, FIBO_CURRENT64);
		@fFiboIndex := GetProcAddress(hDLL, FIBO_INDEX);

		// Call methods...
		if(		(Assigned(@fFiboInit)) and
				(Assigned(@fFiboNext)) and
				(Assigned(@fFiboCurrent)) and
				(Assigned(@fFiboIndex))) then
			begin
			memoOutput.Lines.Add(MSG_CALL_FUNCTIONS);
			memoOutput.Lines.Add('');

			// Note: Delphi 7 does not have a "IntToStr" overload for unsigned Int64. We cast the
			// C++ unsigned long long (UInt64) types to a signed Int64. Any values larger than the
			// max Int64 (9223372036854775807) will not be displayed correctly.
			fFiboInit(1, 1);
			repeat
				memoOutput.Lines.Add(Format('%d: %s', [
					fFiboIndex(),
					IntToStr(Int64(fFiboCurrent()))]));
			until (not fFiboNext());

			memoOutput.Lines.Add(
				Format('%d Fibonacci values fit in an unsigned 64-bit integer', [fFiboIndex() + 1]));
			end
		else
			memoOutput.Lines.Add(MSG_ERR_EXTRACT_FUNCTIONS);

		// Unload library
		memoOutput.Lines.Add('');
		memoOutput.Lines.Add(MSG_UNLOAD_DLL);
		FreeLibrary(hDLL);
		memoOutput.Lines.Add(MSG_UNLOAD_DLL_SUCCESS);
		end
	else
		memoOutput.Lines.Add(m_cache.szMsgErrUnableToLocateDLL);

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('64-bit Fibonacci...manual...complete');
end;

procedure TfrmDelphiClient.UseFibonacciMethodsStatic32();
begin
	// 32-bit Fibonacci methods with static definition
	memoOutput.Lines.Add('32-bit Fibonacci methods...compile-time, statically-defined');
	memoOutput.Lines.Add('');

	// Call methods...
	fibonacciInit32(1, 1);
	repeat
		memoOutput.Lines.Add(Format('%d: %s', [
			fibonacciIndex(),
			IntToStr(fibonacciCurrent32())]));
	until (not fibonacciNext32());

	memoOutput.Lines.Add(
		Format('%d Fibonacci values fit in an unsigned 32-bit integer', [fibonacciIndex() + 1]));

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('32-bit Fibonacci...static...complete');
end;

procedure TfrmDelphiClient.UseFibonacciMethodsStatic64();
begin
	// 64-bit Fibonacci methods with static definition
	memoOutput.Lines.Add('64-bit Fibonacci methods...compile-time, statically-defined');
	memoOutput.Lines.Add('');

	fibonacciInit64(1, 1);
	repeat
		memoOutput.Lines.Add(Format('%d: %s', [
			fibonacciIndex(),
			IntToStr(Int64(fibonacciCurrent64()))]));
	until (not fibonacciNext64());

	memoOutput.Lines.Add(
		Format('%d Fibonacci values fit in an unsigned 64-bit integer', [fibonacciIndex() + 1]));

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('64-bit Fibonacci...static...complete');
end;
// End: Private functions

end.
