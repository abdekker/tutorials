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
  DLL_SIMPLE = 'SimpleDLL.dll';
  DLL_SIMPLE_MFC = 'SimpleDllMFC.dll';

  // Fibonacci functions exported from the standard C++ DLL
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

  // Test functions exported from the standard C++ DLL
  TEST_RETURN =
	{$IF Defined(DLL_CDECL)} 'SimpleReturn'
	{$ELSEIF Defined(DLL_STDCALL)} '_SimpleReturn@0' {$IFEND};
  TEST_SUM =
	{$IF Defined(DLL_CDECL)} 'SimpleSum'
	{$ELSEIF Defined(DLL_STDCALL)} '_SimpleSum@8' {$IFEND};
  TEST_MULTIPLY =
	{$IF Defined(DLL_CDECL)} 'SimpleMultiply'
	{$ELSEIF Defined(DLL_STDCALL)} '_SimpleMultiply@8' {$IFEND};

  // Test functions exported from the MFC C++ DLL
  // Note: Only one version required because a definition (.def) file was used internally in the
  // DLL to fix the name of exported functions
  MFC_OUTSIDE_CLASS1 = 'SimpleReturn1_OutsideClass';
  MFC_OUTSIDE_CLASS2 = 'SimpleReturn2_OutsideClass';

  // Test for a function that is deliberately NOT defined / exported
  TEST_NOT_IN_DLL = 'MissingInAction';

type
  CACHE_DELPHI_CLIENT = record
	szMsgSimpleDLL, szMsgSimpleDllMFC: String;
	szMsgErrUnableToLocateDLL, szMsgErrUnableToLocateDllMFC: String;
  end;

  TfrmDelphiClient = class(TForm)
	gbSettings: TGroupBox;
	lblFunctionClass: TLabel;
	ddlFunctionClass: TComboBox;
	lblFunctionMethod: TLabel;
	ddlFunctionMethod: TComboBox;
	lblFibonacciBitDepth: TLabel;
	rbFibonacciBitDepth32: TRadioButton;
	rbFibonacciBitDepth64: TRadioButton;
	btnUseFunctionsFromDLL: TButton;
	lblDllNameTitle: TLabel;
	lblDllName: TLabel;
	memoOutput: TMemo;

	btnExit: TButton;

	procedure FormCreate(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure OnControlChange(Sender: TObject);
	procedure btnUseFunctionsFromDLLClick(Sender: TObject);

  private
	{ Private declarations }
	m_cache: CACHE_DELPHI_CLIENT;

	procedure UpdateControls();

	procedure UseTestMethodsManual();
	procedure UseTestMethodsStatic();

	procedure UseFibonacciMethodsManual32();
	procedure UseFibonacciMethodsManual64();
	procedure UseFibonacciMethodsStatic32();
	procedure UseFibonacciMethodsStatic64();

	procedure UseTestMethodsManualMFC();
	procedure UseTestMethodsStaticMFC();

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

type
  // Enumerations
  TFunctionClass = (
	eClassSimpleTest = 0,
	eClassSimpleFibonacci,
	eClassSimpleTestMFC);
  TFunctionExtraction = (
	eExtractionManual = 0,
	eExtractionStatic);

  // Static function declarations (alternative to using LoadLibrary / GetProcAddress)

  // SimpleDLL.dll  (standard DLL)
{$IF Defined(DLL_CDECL)}			// cdecl
  // Fibonacci functions
  procedure fibonacciInit32(const a, b: Cardinal); cdecl; external DLL_SIMPLE name FIBO_INIT32;
  procedure fibonacciInit64(const a, b: UInt64); cdecl; external DLL_SIMPLE name FIBO_INIT64;
  function fibonacciNext32() : Boolean; cdecl; external DLL_SIMPLE name FIBO_NEXT32;
  function fibonacciNext64() : Boolean; cdecl; external DLL_SIMPLE name FIBO_NEXT64;
  function fibonacciCurrent32() : Cardinal; cdecl; external DLL_SIMPLE name FIBO_CURRENT32;
  function fibonacciCurrent64() : UInt64; cdecl; external DLL_SIMPLE name FIBO_CURRENT64;
  function fibonacciIndex() : Cardinal; cdecl; external DLL_SIMPLE name FIBO_INDEX;

  // Test functions
  function SimpleReturn() : Cardinal; cdecl; external DLL_SIMPLE name TEST_RETURN;
  function SimpleSum(const a, b: Integer) : Integer; cdecl; external DLL_SIMPLE name TEST_SUM;
  function SimpleMultiply(const a: Single; const b: Char) : Single; cdecl;
	external DLL_SIMPLE name TEST_MULTIPLY;
  function NotInDLL() : Cardinal; cdecl; external DLL_SIMPLE name TEST_NOT_IN_DLL;
{$ELSEIF Defined(DLL_STDCALL)}		// stdcall
  procedure fibonacciInit32(const a, b: Cardinal); stdcall; external DLL_SIMPLE name FIBO_INIT32;
  procedure fibonacciInit64(const a, b: UInt64); stdcall; external DLL_SIMPLE name FIBO_INIT64;
  function fibonacciNext32() : Boolean; stdcall; external DLL_SIMPLE name FIBO_NEXT32;
  function fibonacciNext64() : Boolean; stdcall; external DLL_SIMPLE name FIBO_NEXT64;
  function fibonacciCurrent32() : Cardinal; stdcall; external DLL_SIMPLE name FIBO_CURRENT32;
  function fibonacciCurrent64() : UInt64; stdcall; external DLL_SIMPLE name FIBO_CURRENT64;
  function fibonacciIndex() : Cardinal; stdcall; external DLL_SIMPLE name FIBO_INDEX;

  function SimpleReturn() : Cardinal; stdcall; external DLL_SIMPLE name TEST_RETURN;
  function SimpleSum(const a, b: Integer) : Integer; stdcall; external DLL_SIMPLE name TEST_SUM;
  function SimpleMultiply(const a: Single; const b: Char) : Single; stdcall;
	external DLL_SIMPLE name TEST_MULTIPLY;
  function NotInDLL() : Cardinal; stdcall; external DLL_SIMPLE name TEST_NOT_IN_DLL;
{$IFEND}

  // SimpleDllMFC.dll (MFC DLL)
{$IF Defined(DLL_CDECL)}			// cdecl
  function outsideClass1MFC() : Cardinal; cdecl; external DLL_SIMPLE_MFC name MFC_OUTSIDE_CLASS1;
  function outsideClass2MFC() : Cardinal; cdecl; external DLL_SIMPLE_MFC name MFC_OUTSIDE_CLASS2;
{$ELSEIF Defined(DLL_STDCALL)}		// stdcall
  function outsideClass1MFC() : Cardinal; stdcall; external DLL_SIMPLE_MFC name MFC_OUTSIDE_CLASS1;
  function outsideClass2MFC() : Cardinal; stdcall; external DLL_SIMPLE_MFC name MFC_OUTSIDE_CLASS2;
{$IFEND}

//ADAD
{?SimpleReturn_InsideClass@CSimpleDllMFCApp@@QAGIXZ
_SimpleReturn1_OutsideClass@0
_SimpleReturn2_OutsideClass@0}

{$R *.dfm}

procedure TfrmDelphiClient.FormCreate(Sender: TObject);
begin
	// Initialise cache
	ZeroMemory(@m_cache, SizeOf(CACHE_DELPHI_CLIENT));
	m_cache.szMsgSimpleDLL := Format('### %s ###', [DLL_SIMPLE]);
	m_cache.szMsgSimpleDllMFC := Format('### %s ###', [DLL_SIMPLE_MFC]);

	m_cache.szMsgErrUnableToLocateDLL := Format('Unable to locate %s', [DLL_SIMPLE]);
	m_cache.szMsgErrUnableToLocateDllMFC := Format('Unable to locate %s', [DLL_SIMPLE_MFC]);
end;

procedure TfrmDelphiClient.FormShow(Sender: TObject);
begin
	// Initialise form

	// Function class:
	// * SimpleDLL.dll (and test functions "SimpleReturn", "SimpleSum" and "SimpleMultiply"
	// * SimpleDLL.dll (and 32-bit or 64-bit Fibonacci functions)
	// * SimpleDllMFC.dll.dll (functions from outside and exported from inside a C++ class)
	SetComboHandlers(ddlFunctionClass);
	ddlFunctionClass.Items.Clear();
	ddlFunctionClass.Items.AddObject('Simple DLL (test functions)', TObject(eClassSimpleTest));
	ddlFunctionClass.Items.AddObject('Simple DLL (Fibonacci functions)', TObject(eClassSimpleFibonacci));
	ddlFunctionClass.Items.AddObject('MFC DLL (test functions)', TObject(eClassSimpleTestMFC));
	ddlFunctionClass.ItemIndex := ddlFunctionClass.Items.IndexOfObject(TObject(eClassSimpleTest));

	SetComboHandlers(ddlFunctionMethod);
	ddlFunctionMethod.Items.Clear();
	ddlFunctionMethod.Items.AddObject('LoadLibrary / GetProcAddress', TObject(eExtractionManual));
	ddlFunctionMethod.Items.AddObject('Statically-defined entry points', TObject(eExtractionStatic));
	ddlFunctionMethod.ItemIndex := ddlFunctionMethod.Items.IndexOfObject(TObject(eExtractionManual));

	rbFibonacciBitDepth32.Checked := True;
	UpdateControls();

	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();
	memoOutput.Lines.Add('Select a function class, extraction method...');
	memoOutput.Lines.EndUpdate();

	// Set focus to the Exit button
	btnExit.SetFocus();
end;

procedure TfrmDelphiClient.btnExitClick(Sender: TObject);
begin
	ModalResult := mrOk;
	Close();
end;

procedure TfrmDelphiClient.OnControlChange(Sender: TObject);
begin
	UpdateControls();
end;

procedure TfrmDelphiClient.btnUseFunctionsFromDLLClick(Sender: TObject);
var
	eClass: TFunctionClass;
	eExtraction: TFunctionExtraction;
	nBitDepth: Integer;
begin
	// Update controls based on the current selections
	memoOutput.Lines.BeginUpdate();
	memoOutput.Lines.Clear();

	eClass := TFunctionClass(ddlFunctionClass.Items.Objects[ddlFunctionClass.ItemIndex]);
	eExtraction := TFunctionExtraction(ddlFunctionMethod.Items.Objects[ddlFunctionMethod.ItemIndex]);
	if (eClass = eClassSimpleTest) then
		begin
		// SimpleDLL.dll: Test functions
		memoOutput.Lines.Add(m_cache.szMsgSimpleDLL);
		if (eExtraction = eExtractionManual) then
			begin
			// Manual extraction of functions using LoadLibrary and GetProcAddress
			UseTestMethodsManual();
			end
		else if (eExtraction = eExtractionStatic) then
			begin
			// Statically-defined procedure entry points
			UseTestMethodsStatic();
			end;
		end
	else if (eClass = eClassSimpleFibonacci) then
		begin
		// SimpleDLL.dll: Fibonacci functions
		memoOutput.Lines.Add(m_cache.szMsgSimpleDLL);
		nBitDepth := IfThen(rbFibonacciBitDepth32.Checked, 32, 64);
		if (eExtraction = eExtractionManual) then
			begin
			// Manual extraction of functions using LoadLibrary and GetProcAddress
			if (nBitDepth = 32) then
				UseFibonacciMethodsManual32()
			else if (nBitDepth = 64) then
				UseFibonacciMethodsManual64();
			end
		else if (eExtraction = eExtractionStatic) then
			begin
			// Statically-defined procedure entry points
			if (nBitDepth = 32) then
				UseFibonacciMethodsStatic32()
			else if (nBitDepth = 64) then
				UseFibonacciMethodsStatic64();
			end;
		end
	else if (eClass = eClassSimpleTestMFC) then
		begin
		// SimpleDllMFC.dll: Test functions
		memoOutput.Lines.Add(m_cache.szMsgSimpleDllMFC);
		if (eExtraction = eExtractionManual) then
			UseTestMethodsManualMFC()
		else if (eExtraction = eExtractionStatic) then
			UseTestMethodsStaticMFC();
		end;

	memoOutput.Lines.EndUpdate();
end;

// Start: Private functions
procedure TfrmDelphiClient.UpdateControls();
var
	eClass: TFunctionClass;
	nBitDepth: Integer;
	strButtonText, strDllName: String;
begin
	// Update controls based on the current selections
	eClass := TFunctionClass(ddlFunctionClass.Items.Objects[ddlFunctionClass.ItemIndex]);
	if (eClass = eClassSimpleTest) then
		begin
		strButtonText := 'Test functions';
		strDllName := DLL_SIMPLE;
		end
	else if (eClass = eClassSimpleFibonacci) then
		begin
		nBitDepth := IfThen(rbFibonacciBitDepth32.Checked, 32, 64);
		strButtonText := Format('Fibonacci (%d-bit) functions', [nBitDepth]);
		strDllName := DLL_SIMPLE;
		end
	else if (eClass = eClassSimpleTestMFC) then
		begin
		strButtonText := 'Test functions (MFC)';
		strDllName := DLL_SIMPLE_MFC;
		end;

	lblFibonacciBitDepth.Enabled := (eClass = eClassSimpleFibonacci);
	rbFibonacciBitDepth32.Enabled := lblFibonacciBitDepth.Enabled;
	rbFibonacciBitDepth64.Enabled := lblFibonacciBitDepth.Enabled;

	btnUseFunctionsFromDLL.Caption := strButtonText;
	lblDllName.Caption := strDllName;
end;

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
	hDLL := LoadLibrary(DLL_SIMPLE);
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
	// static functions this is not possible because the program crashes immediately. This is
	// probably a safety feature). This is one advantage of using compile-time functions...you find
	// out about the missing procedure entry point before even attempting to use the method!
	{try
		NotInDLL();		// Won't compile
	except
		on E: Exception do memoOutput.Lines.Add(Format('%s.', [E.Message]));
	end;}

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('Test...static...complete');
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
	hDLL := LoadLibrary(DLL_SIMPLE);
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
	hDLL := LoadLibrary(DLL_SIMPLE);
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

procedure TfrmDelphiClient.UseTestMethodsManualMFC();
var
	hDLL: THandle;
	fFunc1_OutsideClass, fFunc2_OutsideClass: TFibonacciIndex;
begin
	// Test methods from the MFC DLL with manual extraction
	memoOutput.Lines.Add('Test methods from MFC DLL...run-time, manual extraction of function pointers');
	memoOutput.Lines.Add('');
	memoOutput.Lines.Add(MSG_LOAD_DLL);
	hDLL := LoadLibrary(DLL_SIMPLE_MFC);
	if (hDLL <> 0) then
		begin
		memoOutput.Lines.Add(MSG_FIND_FUNCTIONS);
		// Note: Function names are as defined by the defintion (.def) file in the C++ DLL
		@fFunc1_OutsideClass := GetProcAddress(hDLL, MFC_OUTSIDE_CLASS1);
		@fFunc2_OutsideClass := GetProcAddress(hDLL, MFC_OUTSIDE_CLASS2);
		if (	Assigned(@fFunc1_OutsideClass) and
				Assigned(@fFunc2_OutsideClass)) then
			begin
				try
					// An attempt was made to export a method from
					// Curiously, the exported function from the CWinApp-derived class (from the
					// MFC DLL) cannot be used function directly in "Format":
					//		myString := Format('%d', [fFunc_InsideClass()]);
					// The above line crashes or does not change the string (depending on whether
					// "myString" has been set or not). But you can do this:
					//		uMyCardinal := fFunc_InsideClass();		// Or use "IntToStr"
					//		myString := Format('%d', [uMyCardinal]);
					// The reason for this problem is unknown, probably a bug in "Format"

					memoOutput.Lines.Add(Format('SimpleReturn1_OutsideClass: %d', [fFunc1_OutsideClass()]));
					memoOutput.Lines.Add(
						'    (Calls an internal, public method on a CWinApp-derived class)');
					memoOutput.Lines.Add(Format('SimpleReturn2_OutsideClass: %d', [fFunc2_OutsideClass()]));
				except
					on E: Exception do memoOutput.Lines.Add(Format('%s.', [E.Message]));
				end;
			end
		else
			memoOutput.Lines.Add(MSG_ERR_EXTRACT_FUNCTIONS);
		end
	else
		memoOutput.Lines.Add(m_cache.szMsgErrUnableToLocateDllMFC);

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('MFC DLL functions...manual...complete');
end;

procedure TfrmDelphiClient.UseTestMethodsStaticMFC();
var
	strOutsideClass1, strOutsideClass2: String;
begin
	// Test methods from the MFC DLL with static definition
	memoOutput.Lines.Add('Test methods from MFC DLL...compile-time, statically-defined');
	memoOutput.Lines.Add('');

	memoOutput.Lines.Add(Format('SimpleReturn1_OutsideClass: %d', [outsideClass1MFC()]));
	memoOutput.Lines.Add('    (Calls an internal, public method on a CWinApp-derived class)');
	memoOutput.Lines.Add(Format('SimpleReturn2_OutsideClass: %d', [outsideClass2MFC()]));

	memoOutput.Lines.Add('');
	memoOutput.Lines.Add('MFC DLL functions...static...complete');
end;
// End: Private functions

end.
