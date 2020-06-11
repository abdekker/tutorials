unit puzzle0001;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

{ Print all numbers up to 1000 that have the digit 5 in their number }

interface

uses
  {Windows, Classes, Controls, ExtCtrls, Messages, StdCtrls,
  CoreFormClasses}
  Windows, Classes, Controls, StdCtrls,
  CoreFormClasses;

type
  TfrmPuzzle0001 = class(TGeneralBaseForm)
	btnExit: TButton;
	btnStart: TButton;
	lblTarget: TLabel;
	ebTarget: TEdit;
	stResults: TStaticText;

	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnStartClick(Sender: TObject);
	procedure btnExitClick(Sender: TObject);

  private
	{ Private declarations }
	m_bExiting: Boolean;

  public
	{ Public declarations }
	constructor Create(AOwner: TComponent); override;

  end;

var
  frmPuzzle0001: TfrmPuzzle0001;

implementation

uses
  Math, SysUtils,
  FormUtils;

const
  // Print all numbers up to (and including) this target value
  TARGET_DEFAULT = 1000;

{$R *.dfm}

// Constructor
constructor TfrmPuzzle0001.Create(AOwner: TComponent);
begin
	// Local data
	frmPuzzle0001 := Self;
	m_bExiting := False;

	// Call base constructor
	inherited;
end;

procedure TfrmPuzzle0001.FormDestroy(Sender: TObject);
begin
	// Clean up memory
end;

procedure TfrmPuzzle0001.FormShow(Sender: TObject);
var
	strTitle: String;
begin
	// Form show event
	strTitle := (
		'Puzzle 0001 - Only 5s '
		{$IFDEF DBG} + '[DBG]' {$ENDIF}
		{$IFDEF NDBG} + '[NDBG]' {$ENDIF});
	Caption := strTitle;

	ebTarget.Text := IntToStr(TARGET_DEFAULT);
end;

procedure TfrmPuzzle0001.btnStartClick(Sender: TObject);
var
	target, width: Integer;
	pos, tmp, count: Integer;
	strResult: String;
begin
	// Start or stop the puzzle
	// TODO: Do with string comparisons, etc
	if (not TryStrToInt(ebTarget.Text, target)) then
		target := TARGET_DEFAULT;

	width := Round(Log10(target));

	strResult := '';
	count := 0;
	for pos:=1 to target do
		begin
		tmp := pos;
		while (tmp > 0) do
			begin
			if ((tmp mod 10) = 5) then
				begin
				Inc(count);
				if (count = 1) then
					strResult := Format('%3d', [pos])
				else
					strResult := (strResult + Format(' %3d', [pos]));

				break;
				end
			else
				tmp := (tmp div 10);
			end;
		end;

	// Show the results (we can handle ~3150 on the size we provide)
	LockControl(stResults, True);
	stResults.Caption := strResult;
	LockControl(stResults, False);
end;

procedure TfrmPuzzle0001.btnExitClick(Sender: TObject);
begin
	m_bExiting := True;
	Close();
end;

// Private functions: Start
// Private functions: End

end.
