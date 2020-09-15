{
Cloud Nine is a puzzle by Andrew Perkis. See the bottom of this file for additional notes,
including an earlier version of the Cloud Nine puzzle which was thought would result in
more efficient code (it didn't).

The grid we use in the this program is given below (as an ASCII image):

   +---+---+---+---+---+---+---+---+---+---+
 0 | x | x | x | x | C | C | x | x | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 9 | x | x | x | x | C | C | x | x | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 8 | x | x | C | C |   |   | C | C | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 7 | x | x | C | C |   |   | C | C | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 6 | C | C |   |   | C | C |   |   | C | C |
   +---+---+---+---+---+---+---+---+---+---+
 5 | C | C |   |   | C | C |   |   | C | C |
   +---+---+---+---+---+---+---+---+---+---+
 4 | x | x | C | C |   |   | C | C | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 3 | x | x | C | C |   |   | C | C | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 2 | x | x | x | x | C | C | x | x | x | x |
   +---+---+---+---+---+---+---+---+---+---+
 1 | x | x | x | x | C | C | x | x | x | x |
   +---+---+---+---+---+---+---+---+---+---+
	 A       C   D   E   F   G   H   J

"C" marks the clouds (there are nine clouds in the puzzle). Each cloud represents a single
"point" in the puzzle and is designated by the co-ordinate of the bottom left corner of
the cloud. The central cloud (E5) can be visited up to four times, but all other clouds
must be visited once only.
Note: Since each cloud is designated by using only the co-ordinate from the bottom-left
		corner, the following rows and columns are not part of the game:
		Rows: 2, 0
		Columns: B, J
Since "J" is easy to read than "I", "J5" designates the cloud on the far right, middle.

"x" marks cells that are not part of the puzzle).

The connectivity between cells is given below (but see GetCandidates in the code below):
A1		(none)
A3		(none)
A4		(none)
A5		D5/D6
A6		(none)
A7		(none)
A8		(none)
A9		(none)

C1		(none)
C3		C6,D6,F3,F4
C4		(none)
C5		C7,E5,E7
C6		C3,E5
C7		C5,D5,F7,F8
C8		(none)
C9		(none)

D1		(none)
D3		(none)
D4		(none)
D5		A5,C7,F3,G5
D6		A5,C3,G6
D7		(none)
D8		(none)
D9		(none)

E1		E4,F4
E3		E5,G3
E4		E1,E7,G3
E5		C5,C6,E3,F3,E8,F8,H5,H6		[This is the central cloud]
E6		(none)
E7		C5,E4,E9,G7
E8		E5,G6,G7
E9		E7,F7

F1		(none)
F3		C3,D5,E5
F4		C3,E1,F7,H6
F5		(none)
F6		(none)
F7		C7,E9,F4
F8		C7,E5
F9		(none)

G1		(none)
G3		E3,E4,G6,H6
G4		(none)
G5		D5,G7,J5
G6		D6,E8,G3,J5
G7		E7,E8,G5,H5
G8		(none)
G9		(none)

H1		(none)
H3		(none)
H4		(none)
H5		E5,G7
H6		E5,F4,G3
H7		(none)
H8		(none)
H9		(none)

J1		(none)
J3		(none)
J4		(none)
J5		G5,G6
J6		(none)
J7		(none)
J8		(none)
J9		(none)

The central cloud (E5) can be visited up to four times. All other clouds must be visited
once only. Given a sub-set of the numbers as clues, the object of the puzzle is to visit
all available squares exactly once (with the exception of the central cloud). Since there
are 25 available spaces, and the central cloud can be visited up to four times, any
solution (called a "tour") will contain between 25 and 28 jumps.

The rules state that a jump from the central cloud precludes a return back to the central
cloud on the immediate next turn.

An example tour is:
J5,G5,D5,A5,D6,G6,E8,E5,F3,C3,C6,E5,C5,C7,F8,E5,E3,G3,H6,F4,F7,E9,E7,E4,E1
}
unit CloudNineMain;
{$IFDEF DEBUG} {$O-} {$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

const
  // Maximum possible length of a successful tour of the Cloud Nine puzzle
  MAX_TOUR_LENGTH = 28;

  // Number of loops of analysis to perform (per tick of the Analysis timer); this is
  // done to ensure the CPU maintains a high load, but not so high as to starve other
  // threads
  ANALYSIS_LOOPS = 400;

type
  TfrmCloudNineMain = class(TForm)
	btnExit: TButton;

	AnalysisTimer: TTimer;
	UpdateTimer: TTimer;
	pageCloudNine: TPageControl;

	// Game sheet (where previous analysis is loaded)
	GameSheet: TTabSheet;
	gbGame: TGroupBox;

	// Analysis sheet (where new analysis is generated)
	AnalysisSheet: TTabSheet;
	gbAnalysis: TGroupBox;
	lblBaseCandidatesTitle: TLabel;
	lblBaseCandidates: TLabel;
	lblCandidatesReviewedTitle: TLabel;
	lblCandidatesReviewed: TLabel;
	lblCandidatesRejectedTitle: TLabel;
	lblCandidatesRejected: TLabel;
	lblCompletedToursTitle: TLabel;
	lblCompletedTours: TLabel;
	lblClosedToursTitle: TLabel;
	lblClosedTours: TLabel;
	lblOpenToursTitle: TLabel;
	lblOpenTours: TLabel;
	lblLastCompletedTour: TLabel;
	shpAnalysis: TShape;
	lblElapsed: TLabel;
	lblElapsedTitle: TLabel;
	lblLastRouteAnalysed: TLabel;
	btnAnalyse: TButton;
	lblSource: TLabel;
	ebSource: TEdit;
	btnLoadSource: TButton;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure btnExitClick(Sender: TObject);

	procedure btnAnalyseClick(Sender: TObject);

	procedure OnAnalysisTimerTick(Sender: TObject);
	procedure OnUpdateTimerTick(Sender: TObject);

  private
	{ Private declarations }
	m_bAnalysing, m_bAnalyseStart, m_bAnalysisLoop, m_bUpdating: Boolean;
	m_dwAnalysisStart: DWORD;

	m_astrLists: array[1..MAX_TOUR_LENGTH] of TStringList;
	m_astrCandidates, m_astrCompletedTours: TStringList;
	m_nCurrentList: Integer;

	// Output
	m_fpOutput: TextFile;

	// Stats
	m_nCandidatesReviewed, m_nCandidatesRejected: Int64;
	m_nCompletedTours, m_nClosedTours, m_nOpenTours: Integer;
	m_strLastRouteAnalysed, m_strLastCompletedTour: String;

	procedure StartAnalysis();
	procedure StopAnalysis();

	procedure GetCandidates(const cstrBase: String);
	function TourComplete(const cstrRoute: String) : Boolean;
	function RouteValid(const cstrRoute: String) : Boolean;
	function IsClosedTour(const cstrRoute: String) : Boolean;

	// Utility functions
	function CountTimes(const strSubText, strText: String; nLenText: Integer): Integer;

  public
	{ Public declarations }

  end;

var
  frmCloudNineMain: TfrmCloudNineMain;

implementation

uses
  strUtils;

{$R *.dfm}

// Private functions: Start

procedure TfrmCloudNineMain.StartAnalysis();
var
	nList: Integer;
	strOutputFile: String;
begin
	// Start analysis
	btnAnalyse.Caption := 'Stop';
	shpAnalysis.Brush.Color := clGreen;

	lblBaseCandidates.Caption := '0';
	lblCandidatesReviewed.Caption := '0';
	lblCandidatesRejected.Caption := '0';
	lblCompletedTours.Caption := '0';
	lblClosedTours.Caption := '0';
	lblOpenTours.Caption := '0';

	lblElapsedTitle.Visible := True;
	lblElapsed.Visible := True;
	lblElapsed.Caption := '00:00:00';
	lblLastRouteAnalysed.Visible := True;

	m_nCandidatesReviewed := 0;
	m_nCandidatesRejected := 0;
	m_nCompletedTours := 0;
	m_nClosedTours := 0;
	m_nOpenTours := 0;

	for nList:=1 to MAX_TOUR_LENGTH do
		m_astrLists[nList].Clear();

	m_astrCandidates.Clear();
	m_astrCompletedTours.Clear();
	m_nCurrentList := 1;

	strOutputFile := Format('C:\CloudNineOutput_%s.txt', [
		FormatDateTime('yyyymmdd_hhnnss', Now())]);
	AssignFile(m_fpOutput, strOutputFile);
	Rewrite(m_fpOutput);
	gbAnalysis.Caption := Format('Analysis (%s)', [strOutputFile]);

	m_dwAnalysisStart := GetTickCount();
	m_bAnalyseStart := True;
	AnalysisTimer.Enabled := True;
end;

procedure TfrmCloudNineMain.StopAnalysis();
begin
	// Stop analysis
	m_bAnalyseStart := False;
	btnAnalyse.Caption := 'Analyse';
	shpAnalysis.Brush.Color := clGray;
	lblElapsedTitle.Visible := False;
	lblElapsed.Visible := False;
	lblLastRouteAnalysed.Visible := False;
	Sleep(100);
	CloseFile(m_fpOutput);
end;

procedure TfrmCloudNineMain.GetCandidates(const cstrBase: String);
begin
	// Generate candidates
	m_astrCandidates.Clear();
	if (Length(cstrBase) = 0) then
		begin
		// Initial grid
		m_astrCandidates.Add('A5');

		m_astrCandidates.Add('C3');
		m_astrCandidates.Add('C5');
		m_astrCandidates.Add('C6');
		m_astrCandidates.Add('C7');

		m_astrCandidates.Add('D5');
		m_astrCandidates.Add('D6');

		m_astrCandidates.Add('E1');
		m_astrCandidates.Add('E3');
		m_astrCandidates.Add('E4');
		m_astrCandidates.Add('E5');
		m_astrCandidates.Add('E7');
		m_astrCandidates.Add('E8');
		m_astrCandidates.Add('E9');

		m_astrCandidates.Add('F3');
		m_astrCandidates.Add('F4');
		m_astrCandidates.Add('F7');
		m_astrCandidates.Add('F8');

		m_astrCandidates.Add('G3');
		m_astrCandidates.Add('G5');
		m_astrCandidates.Add('G6');
		m_astrCandidates.Add('G7');

		m_astrCandidates.Add('H5');
		m_astrCandidates.Add('H6');

		m_astrCandidates.Add('J5');
		end
	else
		begin
		if (AnsiCompareText(cstrBase, 'A5') = 0) then
			begin
			m_astrCandidates.Add('D5');
			m_astrCandidates.Add('D6');
			end
		else if (AnsiCompareText(cstrBase, 'C3') = 0) then
			begin
			m_astrCandidates.Add('C6');
			m_astrCandidates.Add('D6');
			m_astrCandidates.Add('F3');
			m_astrCandidates.Add('F4');
			end
		else if (AnsiCompareText(cstrBase, 'C5') = 0) then
			begin
			m_astrCandidates.Add('C7');
			m_astrCandidates.Add('E5');
			m_astrCandidates.Add('E7');
			end
		else if (AnsiCompareText(cstrBase, 'C6') = 0) then
			begin
			m_astrCandidates.Add('C3');
			m_astrCandidates.Add('E5');
			end
		else if (AnsiCompareText(cstrBase, 'C7') = 0) then
			begin
			m_astrCandidates.Add('C5');
			m_astrCandidates.Add('D5');
			m_astrCandidates.Add('F7');
			m_astrCandidates.Add('F8');
			end
		else if (AnsiCompareText(cstrBase, 'D5') = 0) then
			begin
			m_astrCandidates.Add('A5');
			m_astrCandidates.Add('C7');
			m_astrCandidates.Add('F3');
			m_astrCandidates.Add('G5');
			end
		else if (AnsiCompareText(cstrBase, 'D6') = 0) then
			begin
			m_astrCandidates.Add('A5');
			m_astrCandidates.Add('C3');
			m_astrCandidates.Add('G6');
			end
		else if (AnsiCompareText(cstrBase, 'E1') = 0) then
			begin
			m_astrCandidates.Add('E4');
			m_astrCandidates.Add('F4');
			end
		else if (AnsiCompareText(cstrBase, 'E3') = 0) then
			begin
			m_astrCandidates.Add('E5');
			m_astrCandidates.Add('G3');
			end
		else if (AnsiCompareText(cstrBase, 'E4') = 0) then
			begin
			m_astrCandidates.Add('E1');
			m_astrCandidates.Add('E7');
			m_astrCandidates.Add('G3');
			end
		else if (AnsiCompareText(cstrBase, 'E5') = 0) then
			begin
			m_astrCandidates.Add('C5');
			m_astrCandidates.Add('C6');
			m_astrCandidates.Add('E3');
			m_astrCandidates.Add('F3');
			m_astrCandidates.Add('E8');
			m_astrCandidates.Add('F8');
			m_astrCandidates.Add('H5');
			m_astrCandidates.Add('H6');
			end
		else if (AnsiCompareText(cstrBase, 'E7') = 0) then
			begin
			m_astrCandidates.Add('C5');
			m_astrCandidates.Add('E4');
			m_astrCandidates.Add('E9');
			m_astrCandidates.Add('G7');
			end
		else if (AnsiCompareText(cstrBase, 'E8') = 0) then
			begin
			m_astrCandidates.Add('E5');
			m_astrCandidates.Add('G6');
			m_astrCandidates.Add('G7');
			end
		else if (AnsiCompareText(cstrBase, 'E9') = 0) then
			begin
			m_astrCandidates.Add('E7');
			m_astrCandidates.Add('F7');
			end
		else if (AnsiCompareText(cstrBase, 'F3') = 0) then
			begin
			m_astrCandidates.Add('C3');
			m_astrCandidates.Add('D5');
			m_astrCandidates.Add('E5');
			end
		else if (AnsiCompareText(cstrBase, 'F4') = 0) then
			begin
			m_astrCandidates.Add('C3');
			m_astrCandidates.Add('E1');
			m_astrCandidates.Add('F7');
			m_astrCandidates.Add('H6');
			end
		else if (AnsiCompareText(cstrBase, 'F7') = 0) then
			begin
			m_astrCandidates.Add('C7');
			m_astrCandidates.Add('E9');
			m_astrCandidates.Add('F4');
			end
		else if (AnsiCompareText(cstrBase, 'F8') = 0) then
			begin
			m_astrCandidates.Add('C7');
			m_astrCandidates.Add('E5');
			end
		else if (AnsiCompareText(cstrBase, 'G3') = 0) then
			begin
			m_astrCandidates.Add('E3');
			m_astrCandidates.Add('E4');
			m_astrCandidates.Add('G6');
			m_astrCandidates.Add('H6');
			end
		else if (AnsiCompareText(cstrBase, 'G5') = 0) then
			begin
			m_astrCandidates.Add('D5');
			m_astrCandidates.Add('G7');
			m_astrCandidates.Add('J5');
			end
		else if (AnsiCompareText(cstrBase, 'G6') = 0) then
			begin
			m_astrCandidates.Add('D6');
			m_astrCandidates.Add('E8');
			m_astrCandidates.Add('G3');
			m_astrCandidates.Add('J5');
			end
		else if (AnsiCompareText(cstrBase, 'G7') = 0) then
			begin
			m_astrCandidates.Add('E7');
			m_astrCandidates.Add('E8');
			m_astrCandidates.Add('G5');
			m_astrCandidates.Add('H5');
			end
		else if (AnsiCompareText(cstrBase, 'H5') = 0) then
			begin
			m_astrCandidates.Add('E5');
			m_astrCandidates.Add('G7');
			end
		else if (AnsiCompareText(cstrBase, 'H6') = 0) then
			begin
			m_astrCandidates.Add('E5');
			m_astrCandidates.Add('F4');
			m_astrCandidates.Add('G3');
			end
		else if (AnsiCompareText(cstrBase, 'J5') = 0) then
			begin
			m_astrCandidates.Add('G5');
			m_astrCandidates.Add('G6');
			end;
		end;
end;

function TfrmCloudNineMain.TourComplete(const cstrRoute: String) : Boolean;
var
	bValidTour: Boolean;
	nLengthRoute: Integer;
begin
	// Given a route, check whether it completes a tour. This is when every spot in the
	// grid is visited exactly once (except the central cloud which can be visited up to
	// four times). A check has already been completed for route legality.

	// Note: Since there are 25 positions in the grid, a compete tour must be at least
	// 74 characters in length:
	// (25 * 2) characters for each position; (24 * 1) character for each comma
	nLengthRoute := Length(cstrRoute);
	if (nLengthRoute < 74) then
		begin
		Result := False;
		Exit;
		end;

	// Search for each location in the grid
	bValidTour := True;

	// Column A
	if (CountTimes('A5', cstrRoute, nLengthRoute) <> 1) then
		bValidTour := False;

	// Column B (not in use)
	// Column C
	if (bValidTour) then
		begin
		if (CountTimes('C3', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('C5', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('C6', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('C7', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Column D
	if (bValidTour) then
		begin
		if (CountTimes('D5', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('D6', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Column E; see note for "E5"
	if (bValidTour) then
		begin
		if (CountTimes('E1', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('E3', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('E4', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Note: E5 can occur up to 4 times
	if (bValidTour) then
		begin
		if (CountTimes('E5', cstrRoute, nLengthRoute) > 4) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('E7', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('E8', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('E9', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Column F
	if (bValidTour) then
		begin
		if (CountTimes('F3', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('F4', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('F7', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('F8', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Column G
	if (bValidTour) then
		begin
		if (CountTimes('G3', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('G5', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('G6', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('G7', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Column H
	if (bValidTour) then
		begin
		if (CountTimes('H5', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	if (bValidTour) then
		begin
		if (CountTimes('H6', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	// Column I (not in use)
	// Column J
	if (bValidTour) then
		begin
		if (CountTimes('J5', cstrRoute, nLengthRoute) <> 1) then
			bValidTour := False;
		end;

	Result := bValidTour;
end;

function TfrmCloudNineMain.RouteValid(const cstrRoute: String) : Boolean;
var
	bValidRoute: Boolean;
	strTwoFromEnd: String;
	nLengthRoute: Integer;
begin
	// This is similiar to the previous version but simply checks if the route is valid,
	// not complete.

	// Search for each location in the grid.
	bValidRoute := True;
	nLengthRoute := Length(cstrRoute);

	// Column A
	if (CountTimes('A5', cstrRoute, nLengthRoute) > 1) then
		bValidRoute := False;

	// Column B (not in use)
	// Column C
	if (bValidRoute) then
		begin
		if (CountTimes('C3', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('C5', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('C6', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('C7', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Column D
	if (bValidRoute) then
		begin
		if (CountTimes('D5', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('D6', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Column E; see note for "E5"
	if (bValidRoute) then
		begin
		if (CountTimes('E1', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('E3', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('E4', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Note: E5 can occur up to 4 times
	if (bValidRoute) then
		begin
		if (CountTimes('E5', cstrRoute, nLengthRoute) > 4) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('E7', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('E8', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('E9', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Column F
	if (bValidRoute) then
		begin
		if (CountTimes('F3', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('F4', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('F7', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('F8', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Column G
	if (bValidRoute) then
		begin
		if (CountTimes('G3', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('G5', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('G6', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('G7', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Column H
	if (bValidRoute) then
		begin
		if (CountTimes('H5', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	if (bValidRoute) then
		begin
		if (CountTimes('H6', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Column I (not in use)
	// Column J
	if (bValidRoute) then
		begin
		if (CountTimes('J5', cstrRoute, nLengthRoute) > 1) then
			bValidRoute := False;
		end;

	// Also check the rule that if we have left the central cloud, we don't return to it
	// the following move ie. "...E5,C5,E5" is not legal.
	// Note: Without this rule, the number of solutions is many times greater! Andrew
	// Perkis considers this rule essential in order not to trivialise the puzzle.
	if (bValidRoute) then
		begin
		if (nLengthRoute > 7) then
			begin
			strTwoFromEnd := AnsiMidStr(cstrRoute, (nLengthRoute - 7), 2);
			if (AnsiEndsStr('E5', cstrRoute)) and (AnsiCompareStr('E5', strTwoFromEnd) = 0) then
				bValidRoute := False;
			end;
		end;

	Result := bValidRoute;
end;

function TfrmCloudNineMain.IsClosedTour(const cstrRoute: String) : Boolean;
var
	strStart, strEnd, strSecondLast: String;
	nCountCandidates, nPosCandidates: Integer;
	bClosedTour: Boolean;
begin
	// A route is closed if you can jump back to the starting spot from the final spot
	// in the tour. For example, if you started on A5 and finished on D5, the tour would
	// be "closed".
	// Note: This will only happen if you start on the central cloud, but check that the
	// second-to-last move wasn't from the central cloud (ie. "E5") because that would
	// break the rule about jumping straight back to the central cloud!
	bClosedTour := False;

	strStart := AnsiLeftStr(cstrRoute, 2);
	strEnd := AnsiRightStr(cstrRoute, 2);
	strSecondLast := AnsiMidStr(cstrRoute, Length(cstrRoute)-4, 2);
	if (AnsiCompareText(strStart, 'E5') <> 0) or
			(AnsiCompareText(strSecondLast, 'E5') <> 0) then
		begin
		GetCandidates(strEnd);
		nCountCandidates := m_astrCandidates.Count;
		for nPosCandidates:=0 to (nCountCandidates-1) do
			begin
			if (AnsiCompareText(m_astrCandidates[nPosCandidates], strStart) = 0) then
				begin
				bClosedTour := True;
				break;
				end;
			end;
		end;

	Result := bClosedTour;
end;

// Utility functions
function TfrmCloudNineMain.CountTimes(const strSubText, strText: String;
	nLenText: Integer): Integer;
begin
	// Note: For a more general implementation, add "(Length(strSubText) = 0) into the
	// first if statement and replace the "div 2" with "div Length(strSubText)". Since
	// we are always checking for a two-character string such as "D4" in Cloud Nine, we
	// can optimise this code.
	if (nLenText = 0) or (Pos(strSubText, strText) = 0) then
		Result := 0
	else
		Result :=
			(nLenText - Length(StringReplace(strText, strSubText, '', [rfReplaceAll]))) div 2;
end;
// Private functions: End

procedure TfrmCloudNineMain.FormCreate(Sender: TObject);
var
	nList: Integer;
begin
	// Initialise form
	pageCloudNine.ActivePage := GameSheet;

	// General sheet

	// Analysis sheet
	m_bAnalysing := False;
	m_bAnalyseStart := False;
	m_bAnalysisLoop := False;
	m_bUpdating := False;
	m_dwAnalysisStart := 0;

	for nList:=1 to MAX_TOUR_LENGTH do
		m_astrLists[nList] := TStringList.Create();

	m_astrCandidates := TStringList.Create();
	m_astrCompletedTours := TStringList.Create();
	m_nCurrentList := 1;
	lblLastCompletedTour.Caption := '';

	// Stats
	m_nCandidatesReviewed := 0;
	m_nCandidatesRejected := 0;
	m_nCompletedTours := 0;
	m_nClosedTours := 0;
	m_nOpenTours := 0;

	m_strLastRouteAnalysed := '';
	m_strLastCompletedTour := '';
end;

procedure TfrmCloudNineMain.FormDestroy(Sender: TObject);
var
	nList: Integer;
begin
	// Clean up
	for nList:=1 to MAX_TOUR_LENGTH do
		m_astrLists[nList].Free();

	m_astrCandidates.Free();
	m_astrCompletedTours.Free();
end;

procedure TfrmCloudNineMain.btnExitClick(Sender: TObject);
begin
	// Exit!
	m_bAnalysing := False;
	AnalysisTimer.Enabled := False;
	UpdateTimer.Enabled := False;
	Application.Terminate();
end;

procedure TfrmCloudNineMain.btnAnalyseClick(Sender: TObject);
begin
	// Start / Stop analysing
	m_bAnalysing := (not m_bAnalysing);
	if (m_bAnalysing) then
		StartAnalysis()
	else
		StopAnalysis();
end;

procedure TfrmCloudNineMain.OnAnalysisTimerTick(Sender: TObject);
var
	nAnalysisCount, nCountCandidates, nPosCandidates, nCentralCloudCount: Integer;
	pList, pNextList: ^TStringList;
	strRoute, strLastPos, strNewRoute: String;
	bClosedTour: Boolean;
begin
	// Updating? Do nothing...
	if (m_bUpdating) then
		Exit;

	// Disable timer
	AnalysisTimer.Enabled := False;
	m_bAnalysisLoop := True;

	// Do the next round of analysing...
	if (m_bAnalysing) then
		begin
		if (m_bAnalyseStart) then
			begin
			// Starting analysis, initialisation

			// Get initial candidates and add them straight into List 1
			GetCandidates('');
			strRoute := m_astrCandidates[0];

			m_nCurrentList := 1;
			nCountCandidates := m_astrCandidates.Count;
			m_nCandidatesReviewed := nCountCandidates;
			for nPosCandidates:=0 to (nCountCandidates-1) do
				m_astrLists[1].Add(m_astrCandidates[nPosCandidates]);

			m_bAnalyseStart := False;
			end
		else
			begin
			// Continuing analysis...

			// Look at the first entry (route) in the current list:
			// * Does it complete a tour? If so, move to the "Completed Tours" list.
			// * If not, generate candidates from the final position in the route
			// * For each candidate determine whether appending it to the route is legal
			// * If legal, copy to the next list up the chain
			// * If illegal, ignore
			// * Remove the entry just looked at
			// * Set the number of the current active list
			nAnalysisCount := 0;
			while (nAnalysisCount < ANALYSIS_LOOPS) and (m_bAnalysing) do
				begin
				// Which list is current?
				pList := @m_astrLists[m_nCurrentList];
				if (m_nCurrentList < 28) then
					pNextList := @m_astrLists[m_nCurrentList+1];

				// Does this entry complete a tour?
				strRoute := pList^[0];
				if (TourComplete(strRoute)) then
					begin
					// Route completes a tour!
					Inc(m_nCompletedTours);
					m_astrCompletedTours.Add(strRoute);
					m_strLastCompletedTour := strRoute;

					// Is the tour closed or open? "Closed" means that from the last spot
					// in the tour, you could jump back to the very first spot. "Open"
					// means you cannot.
					bClosedTour := IsClosedTour(strRoute);

					// Count the number of times that the central cloud is used (will be
					// 3 or 4 times)
					nCentralCloudCount := CountTimes('E5', strRoute, Length(strRoute));

					// Append this information to the route and write to the output file
					if (bClosedTour) then
						begin
						strRoute := (strRoute + ' (Closed');
						Inc(m_nClosedTours);
						end
					else
						begin
						strRoute := (strRoute + ' (Open');
						Inc(m_nOpenTours);
						end;

					strRoute := (strRoute + Format(',CC=%d)', [nCentralCloudCount]));
					Writeln(m_fpOutput, strRoute);
					end
				else
					begin
					// Route does not complete a tour so generate fresh candidates
					Inc(m_nCandidatesRejected);
					strLastPos := AnsiRightStr(strRoute, 2);
					GetCandidates(strLastPos);

					nCountCandidates := m_astrCandidates.Count;
					Inc(m_nCandidatesReviewed, nCountCandidates);
					for nPosCandidates:=0 to (nCountCandidates-1) do
						begin
						strNewRoute := (strRoute + ',' + m_astrCandidates[nPosCandidates]);
						if (RouteValid(strNewRoute)) then
							pNextList.Add(strNewRoute)
						else
							Inc(m_nCandidatesRejected);
						end;
					end;

				// Delete the route we have just analysed
				pList^.Delete(0);

				// Set the current active list
				m_nCurrentList := MAX_TOUR_LENGTH;
				while (m_astrLists[m_nCurrentList].Count = 0) do
					begin
					Dec(m_nCurrentList);
					if (m_nCurrentList = 0) then
						begin
						// Finished!
						m_bAnalysing := False;

						// Add some stats to the output file
						Writeln(m_fpOutput, Format('Time taken  : %s', [lblElapsed.Caption]));
						Writeln(m_fpOutput, Format('Reviewed    : %d', [m_nCandidatesReviewed]));
						Writeln(m_fpOutput, Format('Rejected    : %d', [m_nCandidatesRejected]));
						Writeln(m_fpOutput, Format('Tours       : %d', [m_astrCompletedTours.Count]));
						Writeln(m_fpOutput, Format('  (Closed)  : %d', [m_nClosedTours]));
						Writeln(m_fpOutput, Format('  (Open)    : %d', [m_nOpenTours]));

						// Stop analysis (and close file)
						StopAnalysis();
						break;
						end;
					end;	// while (m_astrLists[m_nCurrentList].Count = 0) do

				Inc(nAnalysisCount);
				end;	// while (nAnalysisCount < ANALYSIS_LOOPS) and (m_bAnalysing) do
			end;
		end;

	// Restart timer
	m_strLastRouteAnalysed := strRoute;
	m_bAnalysisLoop := False;
	Application.ProcessMessages();
	AnalysisTimer.Enabled := (m_bAnalysing);
end;

procedure TfrmCloudNineMain.OnUpdateTimerTick(Sender: TObject);
var
	dwElapsed, dwHours, dwMinutes, dwSeconds: DWORD;
begin
	// Disable timer
	UpdateTimer.Enabled := False;
	m_bUpdating := True;

	// Which sheet is active?
	if (pageCloudNine.ActivePage = GameSheet) then
		begin
		// Game sheet
		end
	else if (pageCloudNine.ActivePage = AnalysisSheet) then
		begin
		// Analysis sheet

		// Don't update anything if busy with an analysis loop
		if (not m_bAnalysisLoop) then
			begin
			// Base candidates (number of possibilities in the base tree still to analyse)
			lblBaseCandidates.Caption := IntToStr(m_astrLists[1].Count);

			// Number of candidates reviewed so far?
			lblCandidatesReviewed.Caption := IntToStr(m_nCandidatesReviewed);

			// Number of candidates rejected so far?
			lblCandidatesRejected.Caption := IntToStr(m_nCandidatesRejected);

			// Number of solutions found so far?
			lblCompletedTours.Caption := IntToStr(m_astrCompletedTours.Count);
			lblClosedTours.Caption := IntToStr(m_nClosedTours);
			lblOpenTours.Caption := IntToStr(m_nOpenTours);
			lblLastCompletedTour.Caption := m_strLastCompletedTour;

			// Time elapsed so far
			dwElapsed := (GetTickCount() - m_dwAnalysisStart);
			dwHours := (dwElapsed div 3600000);
			dwElapsed := (dwElapsed - (dwHours * 3600000));
			dwMinutes := (dwElapsed div 60000);
			dwElapsed := (dwElapsed - (dwMinutes * 60000));
			dwSeconds := (dwElapsed div 1000);
			lblElapsed.Caption := Format('%.2d:%.2d:%.2d', [dwHours, dwMinutes, dwSeconds]);

			// Latest route analysed
			lblLastRouteAnalysed.Caption := m_strLastRouteAnalysed;
			end;	// if (not m_bAnalysisLoop) then
		end;	// else if (pageCloudNine.ActivePage = AnalysisSheet) then

	// Restart timer
	m_bUpdating := False;
	UpdateTimer.Enabled := True;
end;

{
### Additional notes ###

An earlier version of the grid is given below:
   +---+---+---+---+---+---+---+
 7 | x | x | x | C | x | x | x |
   +---+---+---+---+---+---+---+
 6 | x | x |   |   | C | x | x |
   +---+---+---+---+---+---+---+
 5 | x | C |   |   |   |   | x |
   +---+---+---+---+---+---+---+
 4 | C |   |   | C |   |   | C |
   +---+---+---+---+---+---+---+
 3 | x |   |   |   |   | C | x |
   +---+---+---+---+---+---+---+
 2 | x | x | C |   |   | x | x |
   +---+---+---+---+---+---+---+
 1 | x | x | x | C | x | x | x |
   +---+---+---+---+---+---+---+
	 A   B   C   D   E   F   G

"x" marks cells that are not available (and therefore not part of the puzzle). All other
spaces are available.

The connectivity between cells is given below (but see GetCandidates in the code below):
A1		(none)
A2		(none)
A3		(none)
A4		C3,C4
A5		(none)
A6		(none)
A7		(none)

B1		(none)
B2		(none)
B3		B5,C5,D4
B4		C2,D4
B5		B3,C3,D5,D6
B6		(none)
B7		(none)

C1		(none)
C2		B4,C4,E2,E3
C3		A4,B5,E2,E4
C4		A4,C2,E5
C5		B3,D3,D7,E6
C6		D4,E5,E6
C7		(none)

D1		D3,E3
D2		D4,F3
D3		C5,D1,F3
D4		B3,B4,C6,D2,D6,E2,F4,F5
D5		B5,D7,E3
D6		B5,D4
D7		C5,D5

E1		(none)
E2		C2,C3,D4
E3		C2,D1,D5,F5
E4		C3,E6,G4
E5		C4,C6,F3,G4
E6		C5,C6,E4,F4
E7		(none)

F1		(none)
F2		(none)
F3		D2,D3,E5,F5
F4		D4,E6
F5		D4,E3,F3
F6		(none)
F7		(none)

G1		(none)
G2		(none)
G3		(none)
G4		E4,E5
G5		(none)
G6		(none)
G7		(none)
}
end.

