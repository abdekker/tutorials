{ Programming puzzles solved in Delphi. See these files for additional informtion:
* ..\Notes-Puzzles.txt
* ..\Notes-Puzzles-Details.txt }
unit PuzzlesMain;
{$I ..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Controls, Classes, Forms, StdCtrls;

const
	// Puzzle ranges; update when new puzzles are added
	PUZZLE_MIN: Integer = 1;
	PUZZLE_MAX: Integer = 9;

type
  TfrmPuzzlesMain = class(TForm)
	gbPuzzle: TGroupBox;
	btnExit: TButton;
	lblPuzzleNumber: TLabel;
	ebPuzzleNumber: TEdit;
	btnRunPuzzle: TButton;

	procedure FormCreate(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure btnRunPuzzleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private
	{ Private declarations }
	m_nPuzzle: Integer;

  public
	{ Public declarations }

  end;

var
  frmPuzzlesMain: TfrmPuzzlesMain;

implementation

uses
  Dialogs, SysUtils,
  CoreFormClasses, SystemUtils,
  puzzle0001, puzzle0008, puzzle0009;
{$R *.dfm}

procedure TfrmPuzzlesMain.FormCreate(Sender: TObject);
begin
	// Form constructor

	// Start with the latest puzzle...
	m_nPuzzle := PUZZLE_MAX;
end;

procedure TfrmPuzzlesMain.FormShow(Sender: TObject);
begin
	// Form load event
	ebPuzzleNumber.Text := IntToStr(m_nPuzzle);
end;

procedure TfrmPuzzlesMain.btnExitClick(Sender: TObject);
begin
	Close();
end;

procedure TfrmPuzzlesMain.btnRunPuzzleClick(Sender: TObject);
var
	bValidPuzzle, bPuzzleReady: Boolean;
	strValidMsg: String;
	puzzleClassName: TGeneralBase;
	frmPuzzle: TForm;
begin
	// Validate input...
	bValidPuzzle := TryStrToInt(ebPuzzleNumber.Text, m_nPuzzle);
	if (bValidPuzzle) then
		begin
		// The number is valid, but check that it is in range
		if ((m_nPuzzle < PUZZLE_MIN) or (m_nPuzzle > PUZZLE_MAX)) then
			bValidPuzzle := False;
		end;

	if (not bValidPuzzle) then
		begin
		strValidMsg := (
			Format('Error! "%s" is invalid.', [ebPuzzleNumber.Text]) + #13#10 +
			Format('Puzzle numbers range from %d to %d.', [PUZZLE_MIN, PUZZLE_MAX]));
		ShowMessage(strValidMsg);
		Exit;
		end;

	// If we reach here, then we have a valid puzzle number!
	bPuzzleReady := False;
	puzzleClassName := nil;
	case m_nPuzzle of
	1:		begin
			bPuzzleReady := True;
			puzzleClassName := TfrmPuzzle0001;
			end;

	2..7:	;	// Not solved yet in Delphi...

	8:		begin
			bPuzzleReady := True;
			puzzleClassName := TfrmPuzzle0008;
			end;
	9:		begin
			bPuzzleReady := True;
			puzzleClassName := TfrmPuzzle0009;
			end;
	end;

	// Show puzzle...
	if (bPuzzleReady) then
		begin
		frmPuzzle := puzzleClassName.Create(Self);
		frmPuzzle.ShowModal();
		frmPuzzle.Free();
		end
	else
		ShowMessage(Format('Puzzle %.4d is not yet solved in Delphi...', [m_nPuzzle]));
end;

end.
