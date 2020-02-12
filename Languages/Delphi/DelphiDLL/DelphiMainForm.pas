unit DelphiMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

const
  DELPI_DLL = 'DelphiDLL.dll';

type
  CACHE_DELPHI_USE_DLL = record
	aEditBox: array[1..2] of TEdit;
  end;

  TfrmDelphiMain = class(TForm)
	gbSettings: TGroupBox;
	lblNumberOne: TLabel;
	lblNumberTwo: TLabel;
	ebNumberOne: TEdit;
	ebNumberTwo: TEdit;
	btnUseInternalMethods: TButton;
	btnUseExternalMethods: TButton;
	lblOutput: TStaticText;

	btnExit: TButton;

	procedure FormShow(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure btnUseInternalMethodsClick(Sender: TObject);
	procedure btnUseExternalMethodsClick(Sender: TObject);

  private
	{ Private declarations }
	m_aNumber: array[1..2] of Integer;
	m_strOutput: String;
	m_cache: CACHE_DELPHI_USE_DLL;

	function GetNumbers() : Boolean;
	function TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;

	function MinInternal(X, Y: Integer) : Integer;
	function MaxInternal(X, Y: Integer) : Integer;

  public
	{ Public declarations }
  end;

  // Function variable (think of a "pointer to function" without the pointer)
  TMinMaxFunc = function(X, Y: Integer) : Integer; stdcall;

var
  frmDelphiMain: TfrmDelphiMain;

implementation

{$R *.dfm}

procedure TfrmDelphiMain.FormShow(Sender: TObject);
begin
	// Initialise form
	m_aNumber[1] := 23;
	m_aNumber[2] := 75;
	ebNumberOne.Text := IntToStr(m_aNumber[1]);
	ebNumberTwo.Text := IntToStr(m_aNumber[2]);

	m_strOutput := 'Click one of the buttons to compare the integers...';
	lblOutput.Caption := m_strOutput;

	// Initialise cache
	ZeroMemory(@m_cache, SizeOf(CACHE_DELPHI_USE_DLL));
	m_cache.aEditBox[1] := TEdit(FindComponent('ebNumberOne'));
	m_cache.aEditBox[2] := TEdit(FindComponent('ebNumberTwo'));

	// Set focus to the Exit button
	btnExit.SetFocus();
end;

procedure TfrmDelphiMain.btnExitClick(Sender: TObject);
begin
	ModalResult := mrOk;
	Close();
end;

procedure TfrmDelphiMain.btnUseInternalMethodsClick(Sender: TObject);
begin
	// Use an internal method to compare the numbers
	if (GetNumbers()) then
		begin
		m_strOutput := (
			'Internal' + #13#10 +
			Format('  "Min" returns %d', [MinInternal(m_aNumber[1], m_aNumber[2])]) + #13#10 +
			Format('  "Max" returns %d', [MaxInternal(m_aNumber[1], m_aNumber[2])]));
		lblOutput.Caption := m_strOutput;
		end;
end;

procedure TfrmDelphiMain.btnUseExternalMethodsClick(Sender: TObject);
var
	hDLL: THandle;
	fMinExternal, fMaxExternal: TMinMaxFunc;	// Function variables
begin
	// Use an external DLL (written in Delphi) to compare the numbers
	if (GetNumbers()) then
		begin
		// Load the library (note that this is case insensitive)
		hDLL := LoadLibrary(DELPI_DLL);
		if (hDLL <> 0) then
			begin
			// Assign functions from the DLL to the function variables
			@fMinExternal := GetProcAddress(hDLL, 'Min');
			@fMaxExternal := GetProcAddress(hDLL, 'Max');
			if ((@fMinExternal <> nil) and (@fMinExternal <> nil)) then
				m_strOutput := (
					'External' + #13#10 +
					Format('  "Min" returns %d', [fMinExternal(m_aNumber[1], m_aNumber[2])]) +
					#13#10 +
					Format('  "Max" returns %d', [fMaxExternal(m_aNumber[1], m_aNumber[2])]))
			else
				m_strOutput := 'External: Unable to extract method pointer(s) from DLL';

			// Unload library
			FreeLibrary(hDLL);
			end
		else
			m_strOutput := Format('External: Unable to locate %s', [DELPI_DLL]);

		lblOutput.Caption := m_strOutput;
		end;
end;

{ Start: Private methods }
function TfrmDelphiMain.GetNumbers() : Boolean;
var
	nNumber: Integer;
	abHaveNumber: array[1..2] of Boolean;
begin
	// Extract the numbers from the interface
	for nNumber:=1 to 2 do
		begin
		abHaveNumber[nNumber] := TryStrToInt(m_cache.aEditBox[nNumber].Text, m_aNumber[nNumber]);
		if (not abHaveNumber[nNumber]) then
			begin
			m_strOutput := Format('Number %d is invalid or out-of-range', [nNumber]);
			lblOutput.Caption := m_strOutput;
			end;
		end;

	Result := (abHaveNumber[1] and abHaveNumber[2]);
end;

function TfrmDelphiMain.TryStrToInt(const cstrInput: String; out nOutput: Integer) : Boolean;
var
	nErrorCode: Integer;
begin
	// Helper function to convert string to integer; to use:
	// bSuccess:= TryStrToInt(ebValue.Text, nValue);	// Returns "False" if this fails
	Val(cstrInput, nOutput, nErrorCode);
	Result := (nErrorCode = 0);
end;

function TfrmDelphiMain.MinInternal(X, Y: Integer) : Integer;
begin
	// Local version of the same method as in the DelphiDLL project
	if (X < Y) then MinInternal := X else MinInternal := Y;
end;

function TfrmDelphiMain.MaxInternal(X, Y: Integer) : Integer;
begin
	// Local version of the same method as in the DelphiDLL project
	if (X > Y) then MaxInternal := X else MaxInternal := Y;
end;
{ End: Private methods}

end.
