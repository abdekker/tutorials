unit AWSimulatorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, StrUtils;

const
  cnBoard4x4 = 0;
  cnBoard6x6 = 1;

type
  TPlayer = (eBuilder, eDestroyer);
  TMoveTypes = (e11111, e211, e22, e31, e4);
  TWinType = (eLine, eDiagonal, eBlock, eCount, eDifference);

  // Game setup
  AW_GAME_SETUP = record
	byType: BYTE;										// 4x4 or 6x6
	byRows, byCols: BYTE;								// Rows and columns; same as type above
	byWinByNumber: BYTE;								// Requirement for number of plots to win
	byWinByDifference: BYTE;							// Requirement for win by having more plots than other
	abyClaim: array[eBuilder..eDestroyer] of BYTE;		// Requirement for claiming a plot (0/8)
	abyComponents: array[e11111..e4] of BYTE;			// How many components per move eg. 2-1-1 => 3
	abyTiles: array[e11111..e4, 1..5] of BYTE;			// For each component, number of tiles to add/remove

	bAlwayTake11111: Boolean;							// If available, always take 1-1-1-1-1?
  end;

  // Game state
  AW_GAME_STATE = record
	abyBoard: array[1..6, 1..6] of BYTE;				// Current board
	abyLegalPlots: array[1..6, 1..6] of Boolean;		// For current board, legal moves
	abyPlotPlayedTo: array[1..6, 1..6] of Boolean;		// Plot has been already been played to this turn

	ePlayer, eOtherPlayer: TPlayer;						// Current player to move and his opponent
	byPlots: array[eBuilder..eDestroyer] of BYTE;		// Plots claimed by each player

	wMoveNumber: WORD;									// Move number
	abyMoveRefCount: array[e11111..e4] of BYTE;			// Which moves are available

	bGameIsWon: Boolean;								// Has the game been won
  end;

  // Statistics
  AW_STATS = record
	dwStartTicks, dwNumberOfGames: DWORD;
	adwWinsForPlayer: array[eBuilder..eDestroyer] of DWORD;
	wShortestGame, wLongestGame: WORD;

	adwWinByType: array[eLine..eDifference] of DWORD;	// Win by type (eg. vertical line)
	abyMaxTilesRequired: array[0..8] of BYTE;			// How many 0s were needed, etc
  end;

  TfrmAWSimulator = class(TForm)
	btnOk: TButton;
	UpdateTimer: TTimer;

	gbSettings: TGroupBox;
	lblBoardSize: TLabel;
	ddlBoardSize: TComboBox;
    tbAlwaysTake11111: TCheckBox;

	gbResults: TGroupBox;
	btnStartAnalysis: TButton;
	btnStopAnalysis: TButton;
	lblNumberOfGamesTitle: TLabel;
	lblNumberOfGames: TLabel;

	lblWinsForPlayerTitle: TLabel;
	lblWinsForPlayer: TLabel;
	lblWinByTypeTitle: TLabel;
	lblLine: TLabel;
	lblLineTitle: TLabel;
	lblDiagonalTitle: TLabel;
	lblDiagonal: TLabel;
	lblBlockTitle: TLabel;
	lblBlock: TLabel;
	lblCountTitle: TLabel;
	lblCount: TLabel;
	lblDifferenceTitle: TLabel;
	lblDifference: TLabel;
	lblGameLengthTitle: TLabel;
	lblGameLength: TLabel;
	lblTilesRequiredTitle: TLabel;
	lblTile0Title: TLabel;
	lblTile1Title: TLabel;
	lblTile2Title: TLabel;
	lblTile3Title: TLabel;
	lblTitle4Title: TLabel;
	lblTitle5Title: TLabel;
	lblTitle6Title: TLabel;
	lblTitle7Title: TLabel;
	lblTitle8Title: TLabel;
	lblTile0: TLabel;
	lblTile1: TLabel;
	lblTile2: TLabel;
	lblTile3: TLabel;
	lblTile4: TLabel;
	lblTile5: TLabel;
	lblTile6: TLabel;
	lblTile7: TLabel;
	lblTile8: TLabel;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnOkClick(Sender: TObject);

	procedure ddlBoardSizeDropDown(Sender: TObject);
	procedure ddlBoardSizeCloseUp(Sender: TObject);

	procedure btnStartAnalysisClick(Sender: TObject);
	procedure btnStopAnalysisClick(Sender: TObject);

	procedure OnUpdateTimerTick(Sender: TObject);

  private
	{ Private declarations }
	m_Setup: AW_GAME_SETUP;
	m_Game: AW_GAME_STATE;
	m_Stats: AW_STATS;

	// Updates
	m_bAnalysing, m_bForceUpdate: Boolean;

	procedure RunAnalysis();
	procedure SetupNewBoard();
	procedure MakeRandomMove();
	function SetLegalMoves(byTiles: BYTE) : BYTE;
	procedure CheckForWin();

	procedure UpdateStats_Tiles();

  public
	{ Public declarations }

  end;

var
  frmAWSimulator: TfrmAWSimulator;

implementation

{$R *.dfm}

// Private functions: Start
procedure TfrmAWSimulator.RunAnalysis();
var
	dwSpinRound: DWORD;
begin
	// Run the analysis loop. Each loop takes the following form:
	// 1) Set up a new board
	// 2) Play a random (legal) move for Builder
	// 3) Check for won game
	// 4) If won, start back at step 1. If not, go to step 5.
	// 5) Play a random (legal) move for Destroyer
	// 6) Check for won game
	// 7) If won, start back at step 1. If not, go to step 2.

	dwSpinRound := 0;
	while (m_bAnalysing) do
		begin
		// Set up a new board
		SetupNewBoard();

		// Now move randomly until the game is won...
		while (not m_Game.bGameIsWon) do
			begin
			Inc(m_Game.wMoveNumber);
			MakeRandomMove();
			CheckForWin();
			UpdateStats_Tiles();

			Inc(dwSpinRound);
			if ((dwSpinRound mod 1000) = 0) then
				Application.ProcessMessages();
			end;

		// New game completed
		Inc(m_Stats.dwNumberOfGames);

		// Process messages to see if the "Stop" button was pressed
		Application.ProcessMessages();
		end;
end;

procedure TfrmAWSimulator.SetupNewBoard();
var
	eMoveType: TMoveTypes;
begin
	if (m_Setup.byType = cnBoard4x4) then
		begin
		// 4x4

		// 3444
		// 4444
		// 4444
		// 4443
		m_Game.abyBoard[1][1] := 3;
		m_Game.abyBoard[2][1] := 4;
		m_Game.abyBoard[3][1] := 4;
		m_Game.abyBoard[4][1] := 4;

		m_Game.abyBoard[1][2] := 4;
		m_Game.abyBoard[2][2] := 4;
		m_Game.abyBoard[3][2] := 4;
		m_Game.abyBoard[4][2] := 4;

		m_Game.abyBoard[1][3] := 4;
		m_Game.abyBoard[2][3] := 4;
		m_Game.abyBoard[3][3] := 4;
		m_Game.abyBoard[4][3] := 4;

		m_Game.abyBoard[1][4] := 4;
		m_Game.abyBoard[2][4] := 4;
		m_Game.abyBoard[3][4] := 4;
		m_Game.abyBoard[4][4] := 3;
		end
	else
		begin
		// 6x6

		// 444444
		// 434444
		// 444444
		// 444444
		// 444434
		// 444444
		m_Game.abyBoard[1][1] := 4;
		m_Game.abyBoard[2][1] := 4;
		m_Game.abyBoard[3][1] := 4;
		m_Game.abyBoard[4][1] := 4;
		m_Game.abyBoard[5][1] := 4;
		m_Game.abyBoard[6][1] := 4;

		m_Game.abyBoard[1][2] := 4;
		m_Game.abyBoard[2][2] := 3;
		m_Game.abyBoard[3][2] := 4;
		m_Game.abyBoard[4][2] := 4;
		m_Game.abyBoard[5][2] := 4;
		m_Game.abyBoard[6][2] := 4;

		m_Game.abyBoard[1][3] := 4;
		m_Game.abyBoard[2][3] := 4;
		m_Game.abyBoard[3][3] := 4;
		m_Game.abyBoard[4][3] := 4;
		m_Game.abyBoard[5][3] := 4;
		m_Game.abyBoard[6][3] := 4;

		m_Game.abyBoard[1][4] := 4;
		m_Game.abyBoard[2][4] := 4;
		m_Game.abyBoard[3][4] := 4;
		m_Game.abyBoard[4][4] := 4;
		m_Game.abyBoard[5][4] := 4;
		m_Game.abyBoard[6][4] := 4;

		m_Game.abyBoard[1][5] := 4;
		m_Game.abyBoard[2][5] := 4;
		m_Game.abyBoard[3][5] := 4;
		m_Game.abyBoard[4][5] := 4;
		m_Game.abyBoard[5][5] := 3;
		m_Game.abyBoard[6][5] := 4;

		m_Game.abyBoard[1][6] := 4;
		m_Game.abyBoard[2][6] := 4;
		m_Game.abyBoard[3][6] := 4;
		m_Game.abyBoard[4][6] := 4;
		m_Game.abyBoard[5][6] := 4;
		m_Game.abyBoard[6][6] := 4;
		end;

	m_Game.ePlayer := eBuilder;
	m_Game.eOtherPlayer := eDestroyer;
	m_Game.byPlots[eBuilder] := 0;
	m_Game.byPlots[eDestroyer] := 0;
	m_Game.bGameIsWon := False;

	m_Game.wMoveNumber := 0;
	for eMoveType:=e11111 to e4 do
		m_Game.abyMoveRefCount[eMoveType] := 0;

	// Initial stats update
	UpdateStats_Tiles();
end;

procedure TfrmAWSimulator.MakeRandomMove();
var
	byMoveCount, byChosenMove, byComponent, byMoveRow, byMoveCol: BYTE;
	byRow, byCol: BYTE;
	eMove, eChosenMove: TMoveTypes;
	bMoveFound: Boolean;
begin
	// First select a move from the available move types (where the reference count is zero).
	// Except for the first moves of the game, there will be 3 available moves.
	if (m_Game.wMoveNumber < 3) then
		byMoveCount := (6 - BYTE(m_Game.wMoveNumber))
	else
		byMoveCount := 3;

	if (	(m_Setup.bAlwayTake11111) and
			(m_Game.abyMoveRefCount[e11111] = 0)) then
		byChosenMove := 1
	else
		byChosenMove := ((Random(21474843647) mod byMoveCount) + 1);

	// Which move was selected? If "byChosenMove" is 2 (say), then find the second legal move.
	byMoveCount := 0;
	for eMove:=e11111 to e4 do
		begin
		if (m_Game.abyMoveRefCount[eMove] = 0) then
			Inc(byMoveCount);

		if (byMoveCount = byChosenMove) then
			break;
		end;

	// * Increase the reference count of the selected move
	// * Decrease the reference count of all other moves (a move is playable when this reaches 0)
	eChosenMove := eMove;
	for eMove:=e11111 to e4 do
		begin
		if (eMove = eChosenMove) then
			Inc(m_Game.abyMoveRefCount[eMove], 2)
		else
			begin
			if (m_Game.abyMoveRefCount[eMove] > 0) then
				Dec(m_Game.abyMoveRefCount[eMove]);
			end;
		end;

	// No moves have been played yet
	for byRow:=1 to m_Setup.byRows do
		begin
		for byCol:=1 to m_Setup.byCols do
			begin
			m_Game.abyPlotPlayedTo[byRow][byCol] := False;
			end;
		end;

	// Set the legal moves
	for byComponent:=1 to m_Setup.abyComponents[eChosenMove] do
		begin
		byMoveCount := SetLegalMoves(m_Setup.abyTiles[eChosenMove, byComponent]);
		bMoveFound := False;
		if (byMoveCount > 0) then
			begin
			byChosenMove := ((Random(21474843647) mod byMoveCount) + 1);

			// Ok, so which plot was selected? If "byChosenPlot" is 2 (say) then find
			// the second legal plot.
			byMoveCount := 0;
			for byRow:=1 to m_Setup.byRows do
				begin
				for byCol:=1 to m_Setup.byCols do
					begin
					if (m_Game.abyLegalPlots[byRow][byCol]) then
						Inc(byMoveCount);

					if (byMoveCount = byChosenMove) and (not bMoveFound) then
						begin
						bMoveFound := True;
						byMoveRow := byRow;
						byMoveCol := byCol;
						end;
					end;
				end;

			// Ok, make the move!
			if (bMoveFound) then
				begin
				if (m_Game.ePlayer = eBuilder) then
					Inc(m_Game.abyBoard[byMoveRow][byMoveCol], m_Setup.abyTiles[eChosenMove, byComponent])
				else
					Dec(m_Game.abyBoard[byMoveRow][byMoveCol], m_Setup.abyTiles[eChosenMove, byComponent]);

				m_Game.abyPlotPlayedTo[byMoveRow][byMoveCol] := True;
				end;
			end;
		end;
end;

function TfrmAWSimulator.SetLegalMoves(byTiles: BYTE) : BYTE;
var
	byRow, byCol: BYTE;
	byLegalMoveCount: BYTE;
begin
	// Find the legal moves (based on the move chosen, moves played so far, etc)
	byLegalMoveCount := 0;
	for byRow:=1 to m_Setup.byRows do
		begin
		for byCol:=1 to m_Setup.byCols do
			begin
			if (m_Game.ePlayer = eBuilder) then
				begin
				// Builder...adding the tile(s) must leave the plot with eight or fewer tiles
				if (	(m_Setup.abyClaim[eBuilder] >= (m_Game.abyBoard[byRow][byCol] + byTiles)) and
						(not m_Game.abyPlotPlayedTo[byRow][byCol]) and
						(m_Game.abyBoard[byRow][byCol] > m_Setup.abyClaim[eDestroyer])) then
					begin
					m_Game.abyLegalPlots[byRow][byCol] := True;
					Inc(byLegalMoveCount);
					end
				else
					m_Game.abyLegalPlots[byRow][byCol] := False;
				end
			else
				begin
				// Destroyer...removing the tile(s) must leave the plot with zero or move tiles
				if (	(m_Game.abyBoard[byRow][byCol] >= byTiles) and
						(not m_Game.abyPlotPlayedTo[byRow][byCol]) and
						(m_Game.abyBoard[byRow][byCol] < m_Setup.abyClaim[eBuilder])) then
					begin
					m_Game.abyLegalPlots[byRow][byCol] := True;
					Inc(byLegalMoveCount);
					end
				else
					m_Game.abyLegalPlots[byRow][byCol] := False;
				end;
			end;
		end;

	Result := byLegalMoveCount;
end;

procedure TfrmAWSimulator.CheckForWin();
var
	bGameWon: Boolean;
	byRow, byCol, byPlotsDiff: BYTE;
begin
	// Check for a win
	// Note: Check for all winning conditions for accurate stats when multiple winning conditions
	// are met
	bGameWon := False;

	// Lines of four, horizontal ?
	for byRow:=1 to m_Setup.byRows do
		begin
		for byCol:=1 to (m_Setup.byCols-3) do
			begin
			if (	(m_Game.abyBoard[byRow][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow][byCol+1] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow][byCol+2] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow][byCol+3] = m_Setup.abyClaim[m_Game.ePlayer])) then
				begin
				bGameWon := True;
				Inc(m_Stats.adwWinByType[eLine]);
				end;
			end;
		end;

	// Lines of four, vertical ?
	for byCol:=1 to m_Setup.byCols do
		begin
		for byRow:=1 to (m_Setup.byRows-3) do
			begin
			if (	(m_Game.abyBoard[byRow][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+1][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+2][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+3][byCol] = m_Setup.abyClaim[m_Game.ePlayer])) then
				begin
				bGameWon := True;
				Inc(m_Stats.adwWinByType[eLine]);
				end;
			end;
		end;

	// Diagonal line of four, top left -> bottom right ?
	for byRow:=1 to (m_Setup.byRows-3) do
		begin
		for byCol:=1 to (m_Setup.byCols-3) do
			begin
			if (	(m_Game.abyBoard[byRow][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+1][byCol+1] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+2][byCol+2] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+3][byCol+3] = m_Setup.abyClaim[m_Game.ePlayer])) then
				begin
				bGameWon := True;
				Inc(m_Stats.adwWinByType[eDiagonal]);
				end;
			end;
		end;

	// Diagonal line of four, top right -> bottom left ?
	for byRow:=m_Setup.byRows downto 4 do
		begin
		for byCol:=1 to (m_Setup.byCols-3) do
			begin
			if (	(m_Game.abyBoard[byRow][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow-1][byCol+1] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow-2][byCol+2] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow-3][byCol+3] = m_Setup.abyClaim[m_Game.ePlayer])) then
				begin
				bGameWon := True;
				Inc(m_Stats.adwWinByType[eDiagonal]);
				end;
			end;
		end;

	// Blocks of four ?
	for byCol:=1 to (m_Setup.byCols-1) do
		begin
		for byRow:=1 to (m_Setup.byRows-1) do
			begin
			if (	(m_Game.abyBoard[byRow][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow][byCol+1] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+1][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) and
					(m_Game.abyBoard[byRow+1][byCol+1] = m_Setup.abyClaim[m_Game.ePlayer])) then
				begin
				bGameWon := True;
				Inc(m_Stats.adwWinByType[eBlock]);
				end;
			end;
		end;

	// Count of plots claimed ?
	m_Game.byPlots[m_Game.ePlayer] := 0;
	for byRow:=1 to m_Setup.byRows do
		begin
		for byCol:=1 to m_Setup.byCols do
			begin
			if (m_Game.abyBoard[byRow][byCol] = m_Setup.abyClaim[m_Game.ePlayer]) then
				Inc(m_Game.byPlots[m_Game.ePlayer]);
			end;
		end;

	if (m_Game.byPlots[m_Game.ePlayer] >= m_Setup.byWinByNumber) then
		begin
		bGameWon := True;
		Inc(m_Stats.adwWinByType[eCount]);
		end;

	// Win by difference (having N plots more than your opponent) ?
	// Note: This win condition does not apply in the 6x6 game.
	if (m_Setup.byType = cnBoard4x4) then
		begin
		if (m_Game.byPlots[m_Game.ePlayer] > m_Game.byPlots[m_Game.eOtherPlayer]) then
			begin
			byPlotsDiff := (m_Game.byPlots[m_Game.ePlayer] - m_Game.byPlots[m_Game.eOtherPlayer]);
			if (byPlotsDiff >= m_Setup.byWinByDifference) then
				begin
				bGameWon := True;
				Inc(m_Stats.adwWinByType[eDifference]);
				end;
			end;
		end;

	// Has the game being won ?
	if (bGameWon) then
		begin
		// Game is won! Increment stats and start a new game.
		m_Game.bGameIsWon := True;
		Inc(m_Stats.adwWinsForPlayer[m_Game.ePlayer]);

		// Check if game sets a record for the length of game
		if (m_Game.wMoveNumber < m_Stats.wShortestGame) then
			m_Stats.wShortestGame := m_Game.wMoveNumber;

		if (m_Game.wMoveNumber > m_Stats.wLongestGame) then
			m_Stats.wLongestGame := m_Game.wMoveNumber;
		end
	else
		begin
		// Game is not won yet...switch the active player and continue the game
		m_Game.eOtherPlayer := m_Game.ePlayer;
		if (m_Game.ePlayer = eBuilder) then
			m_Game.ePlayer := eDestroyer
		else
			m_Game.ePlayer := eBuilder;
		end;
end;

procedure TfrmAWSimulator.UpdateStats_Tiles();
var
	byRow, byCol, byTile: BYTE;
	abyPlotCount: array[0..8] of BYTE;
begin
	// Update the stats for the number of plots of various stages of being
	// built from "destroyed" (0) to "built" (8)
	for byTile:=0 to 8 do
		abyPlotCount[byTile] := 0;

	// Count plots
	for byRow:=1 to m_Setup.byRows do
		begin
		for byCol:=1 to m_Setup.byCols do
			begin
			Inc(abyPlotCount[m_Game.abyBoard[byRow, byCol]]);
			end;
		end;

	// Update the overall stats for each tile type
	for byTile:=0 to 8 do
		begin
		if (abyPlotCount[byTile] > m_Stats.abyMaxTilesRequired[byTile]) then
			m_Stats.abyMaxTilesRequired[byTile] := abyPlotCount[byTile];
		end;
end;
// Private functions: End

procedure TfrmAWSimulator.FormCreate(Sender: TObject);
begin
	// Creation...
	ZeroMemory(@m_Setup, SizeOf(AW_GAME_SETUP));
	ZeroMemory(@m_Game, SizeOf(AW_GAME_STATE));
	ZeroMemory(@m_Stats, SizeOf(AW_STATS));

	// Updates
	m_bAnalysing := False;
	m_bForceUpdate := False;
end;

procedure TfrmAWSimulator.FormDestroy(Sender: TObject);
begin
	// Destruction...
end;

procedure TfrmAWSimulator.FormShow(Sender: TObject);
begin
	// Form show
	// Set up the board sizes
	ddlBoardSize.Items.Clear();
	ddlBoardSize.Items.Add('4 x 4');
	ddlBoardSize.Items.Add('6 x 6');
	ddlBoardSize.ItemIndex := 0;

	// Initial update
	m_bForceUpdate := True;
end;

procedure TfrmAWSimulator.btnOkClick(Sender: TObject);
begin
	// Terminate the application
	m_bAnalysing := False;
	ModalResult := mrOk;
	Application.Terminate();
end;

procedure TfrmAWSimulator.ddlBoardSizeDropDown(Sender: TObject);
begin
	// Modify the combo background colour (when extended) for aestethic reasons
	ddlBoardSize.Color := clSkyBlue;
end;

procedure TfrmAWSimulator.ddlBoardSizeCloseUp(Sender: TObject);
begin
	ddlBoardSize.Color := clInfoBk;
end;

procedure TfrmAWSimulator.btnStartAnalysisClick(Sender: TObject);
var
	wHours, wMins, wSecs, wMilliSecs: WORD;
begin
	// Start analysis

	// Reset statistics
	ZeroMemory(@m_Stats, SizeOf(AW_STATS));
	m_Stats.dwStartTicks := GetTickCount();
	m_Stats.wShortestGame := 65535;

	// Set up the game state
	m_Setup.byType := BYTE(ddlBoardSize.ItemIndex);
	if (m_Setup.byType = cnBoard4x4) then
		begin
		m_Setup.byRows := 4;
		m_Setup.byCols := 4;
		m_Setup.byWinByNumber := 7;
		m_Setup.byWinByDifference := 4;
		end
	else
		begin
		m_Setup.byRows := 6;
		m_Setup.byCols := 6;
		m_Setup.byWinByNumber := 12;
		m_Setup.byWinByDifference := 12;	// Does not apply in the 6x6 version
		end;

	// Set the number of tiles needed to claim a plot for Builder / Destroyer
	m_Setup.abyClaim[eBuilder] := 8;
	m_Setup.abyClaim[eDestroyer] := 0;

	// Set up the components for each move
	m_Setup.abyComponents[e11111] := 5;
	m_Setup.abyTiles[e11111][1] := 1;
	m_Setup.abyTiles[e11111][2] := 1;
	m_Setup.abyTiles[e11111][3] := 1;
	m_Setup.abyTiles[e11111][4] := 1;
	m_Setup.abyTiles[e11111][5] := 1;

	m_Setup.abyComponents[e211] := 3;
	m_Setup.abyTiles[e211][1] := 2;
	m_Setup.abyTiles[e211][2] := 1;
	m_Setup.abyTiles[e211][3] := 1;

	m_Setup.abyComponents[e22] := 2;
	m_Setup.abyTiles[e22][1] := 2;
	m_Setup.abyTiles[e22][2] := 2;

	m_Setup.abyComponents[e31] := 2;
	m_Setup.abyTiles[e31][1] := 3;
	m_Setup.abyTiles[e31][2] := 1;

	m_Setup.abyComponents[e4] := 1;
	m_Setup.abyTiles[e4][1] := 4;

	m_Setup.bAlwayTake11111 := (tbAlwaysTake11111.Checked);

	// Seed the random number generator
	DecodeTime(Now(), wHours, wMins, wSecs, wMilliSecs);
	RandSeed := wMilliSecs;

	// Updates
	m_bAnalysing := True;
	m_bForceUpdate := True;

	// Start the analysis!
	RunAnalysis();
end;

procedure TfrmAWSimulator.btnStopAnalysisClick(Sender: TObject);
begin
	// Stop analysis
	m_bAnalysing := False;
	m_bForceUpdate := True;
end;

procedure TfrmAWSimulator.OnUpdateTimerTick(Sender: TObject);
var
	dwElapsed: DWORD;
	fTmp: Single;
	byTile: BYTE;
	AComponent: TComponent;
begin
	// Disable the timer
	UpdateTimer.Enabled := False;

	// Update the Results once-off when analysis is started or stopped
	if (m_bForceUpdate) then
		begin
		if (m_bAnalysing) then
			begin
			// Settings
			lblBoardSize.Enabled := False;
			ddlBoardSize.Enabled := False;
			tbAlwaysTake11111.Enabled := False;

			// Results
			gbResults.Caption := 'Results (Analysing)';
			btnStartAnalysis.Enabled := False;
			btnStopAnalysis.Enabled := True;
			end
		else
			begin
			// Settings
			lblBoardSize.Enabled := True;
			ddlBoardSize.Enabled := True;
			tbAlwaysTake11111.Enabled := True;

			// Results
			gbResults.Caption := 'Results';
			btnStartAnalysis.Enabled := True;
			btnStopAnalysis.Enabled := False;
			end;
		end;

	// Update stats
	if (m_bAnalysing) or (m_bForceUpdate) then
		begin
		// Total number of games played (and games per second)
		dwElapsed := (GetTickCount() - m_Stats.dwStartTicks);
		//fTmp := (Single(m_Stats.dwNumberOfGames)/Single(dwElapsed));
		fTmp := (m_Stats.dwNumberOfGames/dwElapsed);
		lblNumberOfGames.Caption := Format('%s (%.1f per ms)', [
			FloatToStrF(m_Stats.dwNumberOfGames, ffNumber, 10, 0),
			fTmp]);

		{
		lblMinValue.Caption := Format('%s: %s', [
					CachedString(cszMinOnly),
					FloatToStrF(m_nMinValue, ffNumber, 10, 0)]);
		}

		// Wins by each player
		fTmp := 50.0;
		if (m_Stats.dwNumberOfGames > 0) then
			fTmp := 100.0 * (m_Stats.adwWinsForPlayer[eBuilder] / m_Stats.dwNumberOfGames);

		lblWinsForPlayer.Caption := Format('B = %d (%.2f%%); D = %d (%.2f%%)', [
			m_Stats.adwWinsForPlayer[eBuilder], fTmp,
			m_Stats.adwWinsForPlayer[eDestroyer], (100.0 - fTmp)]);

		// Win by type
		lblLine.Caption := IntToStr(m_Stats.adwWinByType[eLine]);
		lblDiagonal.Caption := IntToStr(m_Stats.adwWinByType[eDiagonal]);
		lblBlock.Caption := IntToStr(m_Stats.adwWinByType[eBlock]);
		lblCount.Caption := IntToStr(m_Stats.adwWinByType[eCount]);
		if (m_Setup.byType = cnBoard4x4) then
			lblDifference.Caption := IntToStr(m_Stats.adwWinByType[eDifference])
		else
			lblDifference.Caption := 'NA';

		// Game length
		lblGameLength.Caption := Format('Min = %d; Max = %d', [
			m_Stats.wShortestGame, m_Stats.wLongestGame]);

		// Tiles required
		for byTile:=0 to 8 do
			begin
			AComponent := FindComponent(Format('lblTile%d', [byTile]));
			if (Assigned(AComponent)) then
				TLabel(AComponent).Caption := IntToStr(m_Stats.abyMaxTilesRequired[byTile]);
			end;
		end;

	// Reset the "Force Update" flag
	m_bForceUpdate := False;

	// Restart the timer
	UpdateTimer.Enabled := True;
end;

end.
