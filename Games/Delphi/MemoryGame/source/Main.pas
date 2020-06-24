{ This program uses these C++ DLLs:
	* ..\..\..\Languages\CPP\Utils\Imaging
	* ..\..\..\Languages\CPP\Utils\JPGTools
If the DLLs are not in the target folder, rebuild those projects and (if required) manually copy
the DLLs to this folder.

Note: Conditional definitions from CoreOptions.inc alter the way this program functions. }
unit Main;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Buttons, Controls, Classes, Dialogs, ExtCtrls, Forms, Graphics, ShellAPI,
  StdCtrls, SysUtils,
  FormUtils, Imaging, JPGTools, CoreTypes, Machine, MemoryGameTypes, SystemUtils, WinSkinData;

const
	cdwBlockDrawTime: DWORD = 500;

type
  // Enumeration
  TSquareState = (eFaceDown, eFaceUpTentative, eFaceUpComplete);

  // Timer settings
  TIMER_SETTINGS = record
	dwRefreshRequest, dwRefreshDone: DWORD;
	dwLastLabelsUpdate: DWORD;
  end;

  // Cache settings for speeig up calculations
  CACHE_SETTINGS = record
	// Cache values for resizing the form (currently not active)
	nBackPanelWidthOffset, nBackPanelHeightOffset: Integer;
	fInitFormRatioWidth, fInitFormRatioHeight: Single;
	nSpaceRqdButtons: Integer;

	// Size of each image (with border) for working out which image was clicked
	nImgWidthWithBorder, nImgHeightWithBorder: Integer;

	// When the game starts...the total grid size and images required
	nGridSize, nImagesRqd: Integer
  end;

  // Game state setings
  GAME_STATE = record
	// Current state of a square in the grid
	aSquares: array[1..MAX_SIZE, 1..MAX_SIZE] of TSquareState;

	// Bounding rectangle for each square
	nImgWidth, nImgHeight: Integer;
	arctImages: array[1..MAX_SIZE, 1..MAX_SIZE] of TRect;

	// Matching squares for each pair of cards
	anMatchSquare_Row, anMatchSquare_Col: array[1..MAX_SIZE, 1..MAX_SIZE] of Integer;

	// Whether the game is running or not
	bRunning, bTimeIsRunning: Boolean;

	// Stats about the current cards selected
	anSelectedRow, anSelectedCol: array[1..2] of Integer;
	byUnmatchedCardsFaceUp: BYTE;

	// Stats about how long the current game has taken
	dwGameStart: DWORD;
	dwLastCompletedTime, dwBestCompletedTime: DWORD;
	wClicks, wCardsTurnedOver, wCardsCompleted: WORD;
	wLastGameClicks, wLastGameCardsTurnedOver: WORD;

	// Number of games completed
	wGamesCompleted: WORD;
  end;

  TfrmMain = class(TForm)
	SystemClock: TTimer;
	SkinData: TSkinData;

	NameBackPanel: TPanel;
	lblGameName: TStaticText;

	GameBackPanel: TPanel;
	GamePanel: TPanel;
	GameImageBox: TPaintBox;

	btnNewGame: TBitBtn;
	btnSettings: TBitBtn;
	btnExit: TBitBtn;

	gbCurrentGame: TGroupBox;
	lblRows: TLabel;
	lblColumns: TLabel;
	lblTimeStartedTitle: TLabel;
	lblElapsedTimeTitle: TLabel;
	lblTimeStarted: TLabel;
	lblElapsedTime: TLabel;
	lblGameClicksTitle: TLabel;
	lblGameClicks: TLabel;
	lblCardsComplete_TurnedOverTitle: TLabel;
	lblCardsComplete_TurnedOver: TLabel;

	gbLastGame: TGroupBox;
	lblLastGameTimeTitle: TLabel;
	lblLastGameTime: TLabel;
	lblBestGameTimeTitle: TLabel;
	lblBestGameTime: TLabel;

	gbDebugging: TGroupBox;
	lblScreenSizeTitle: TLabel;
	lblScreenSize: TLabel;
	lblFormSizeTitle: TLabel;
	lblFormSize: TLabel;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure FormActivate(Sender: TObject);
	procedure FormKeyDown(Sender: TObject; var Key: WORD; Shift: TShiftState);
	procedure FormResize(Sender: TObject);

	procedure btnNewGameClick(Sender: TObject);
	procedure btnSettingsClick(Sender: TObject);
	procedure btnExitClick(Sender: TObject);

	procedure OnSystemClockTick(Sender: TObject);

	procedure GameImageBoxPaint(Sender: TObject);
	procedure GameImageBoxClick(Sender: TObject);

  private
	{ Private declarations }
	// Initialisation and system-state flags
	m_bFirstTimeShow, m_bExiting, m_bAllowDraw: Boolean;
	m_dwBlockDrawTime: DWORD;

	// Images
	m_pStartImageOriginal, m_pStartImageCorrected: PTIMAGECELL;
	m_pImgWholeGame, m_pImgLastCompletedGame, m_pImgBlank: PTIMAGECELL;
	m_apImages: array[1..MAX_SIZE, 1..MAX_SIZE] of PTIMAGECELL;
	m_astrGameImages: TStringList;		// For the "Internal" and "Folder" options
	m_bLastGameImage: Boolean;

	// GDI objects
	m_hFont: TFont;
	m_hBorderTentativePen, m_hBorderPen: TPen;

	// Game state
	m_game: GAME_STATE;

	// Timers and cache
	m_timers: TIMER_SETTINGS;
	m_cache: CACHE_SETTINGS;

	// Debugging
	m_dwRedrawCount, m_dwResizeCount: DWORD;

	// Initialisation
	procedure FirstTimeInit();
	procedure CacheSettings();
	procedure InitialiseGame();
	procedure GenerateGameImages();
	procedure LoadGameImages();
	procedure AssignRandomLocations(anImages: TList; nRequired: Integer);

	// Settings and game status
	procedure RefreshSettings();
	procedure ResetLastBestGameStats();
	procedure ResetGameStats();
	procedure SetGameControls();
	procedure TurnOverCards();
	procedure NotifyGameComplete();

	// Drawing functions
	procedure DrawBorderTentative(nRow, nCol: Integer);
	procedure DrawGrid();

	// Buttons
	procedure EnableButtons(bEnable: Boolean);

	// Utility functions
	procedure GetRowColumnFromPosition(const cnPos: Integer;
		var nRow: Integer; var nColumn: Integer);
	function GetTimeString(dwTime: DWORD; bShowMilliSeconds: Boolean) : String;
	procedure ScreenShot(nX, nY: Integer; nWidth, nHeight: Integer; bmScreen: TBitMap);

  public
	{ Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Settings;

{$R *.dfm}

// Private functions: Start
procedure TfrmMain.FirstTimeInit();
var
	nOldWidth, nWidthChange: Integer;
	nOldHeight, nHeightChange: Integer;
	strStartImagePath: String;
begin
	// Cache settings
	CacheSettings();

	// Load main game settings
	MemoryGame := TSystem.Create();
	MemoryGame.InitSystem();
	RefreshSettings();

	// Before we change the screen size (if required) backup the current sizes
	nOldWidth := Self.ClientWidth;
	nOldHeight := Self.ClientHeight;

	// How big is the screen and how big is the form?
	{$IFDEF DBG}
	lblScreenSize.Caption := Format('W: %d, H: %d', [Screen.Width, Screen.Height]);
	lblFormSize.Caption := Format('W: %d, H: %d', [Self.ClientWidth, Self.ClientHeight]);
	{$ENDIF}

	// Set the resize constraints for the main form
	Self.Constraints.MaxHeight := (Screen.Height - 50);
	Self.Constraints.MaxWidth := Round(Self.Constraints.MaxHeight * (Screen.Width / Screen.Height));

	// If we have had to make the screen smaller (this might happens when we try and display the
	// game on a 800x600 screen, for example), move components to fill the available space.
	if (nOldWidth <> Self.ClientWidth) or (nOldHeight <> Self.ClientHeight) then
		begin
		nWidthChange := (Self.ClientWidth - nOldWidth);
		nHeightChange := (Self.ClientHeight - nOldHeight);

		lblGameName.Width := (lblGameName.Width + nWidthChange);
		NameBackPanel.Width := (NameBackPanel.Width + nWidthChange);

		GamePanel.Width := (GamePanel.Width + nWidthChange);
		GamePanel.Height := (GamePanel.Height + nHeightChange);

		gbCurrentGame.Left := (gbCurrentGame.Left + nWidthChange);
		gbLastGame.Left := (gbLastGame.Left + nWidthChange);

		btnNewGame.Left := (btnNewGame.Left + nWidthChange);
		btnSettings.Left := (btnSettings.Left + nWidthChange);
		btnExit.Left := (btnExit.Left + nWidthChange);

		// Also resize our "whole game" image
		PrepareImageCell(m_pImgWholeGame, GameImageBox.Width, GameImageBox.Height);
		Expand24Coloured(m_pImgWholeGame, COLOUR_BACKGROUND);
		PrepareImageCell(m_pImgLastCompletedGame, GameImageBox.Width, GameImageBox.Height);
		Expand24Coloured(m_pImgLastCompletedGame, COLOUR_BACKGROUND);
		end;

	// Load the starting sample image
	strStartImagePath := (MemoryGame.GameCache.szAppPath + 'StartingImage.jpg');
	LoadJpgNoComments24(m_pStartImageOriginal, strStartImagePath);

	ExtractImageCell24(m_pStartImageCorrected, m_pStartImageOriginal, nil);
	ImageCellCorrectAspect24(@m_pStartImageCorrected, COLOUR_BACKGROUND,
		GameImageBox.Width, GameImageBox.Height);

	// Set main GUI elements (English only)
	lblGameName.Caption := 'Memory Game';
	gbCurrentGame.Caption := 'Current';
	gbLastGame.Caption := 'Last';
	lblLastGameTimeTitle.Caption := 'Last Time';
	lblBestGameTimeTitle.Caption := 'Best Time';

	// Refresh all stats
	ResetLastBestGameStats();
	ResetGameStats();
	SetGameControls();
end;

procedure TfrmMain.CacheSettings();
begin
	// Cache some settings to help speed up drawing the game
	m_cache.nBackPanelWidthOffset := (Self.ClientWidth - GameBackPanel.Width);
	m_cache.nBackPanelHeightOffset := (Self.ClientHeight - GameBackPanel.Height);
	m_cache.fInitFormRatioWidth := (Self.ClientWidth / GamePanel.Width);
	m_cache.fInitFormRatioHeight := (Self.ClientHeight / GamePanel.Height);
	m_cache.nSpaceRqdButtons := (Self.ClientWidth - GamePanel.Width);
end;

procedure TfrmMain.InitialiseGame();
var
	nRow, nCol: Integer;
	byCard: BYTE;
begin
	// Initialise a new game

	// Set all cards initially face down
	for nRow:=1 to MAX_SIZE do
		begin
		for nCol:=1 to MAX_SIZE do
			m_game.aSquares[nRow, nCol] := eFaceDown;
		end;

	for byCard:=1 to 2 do
		begin
		m_game.anSelectedRow[byCard] := -1;
		m_game.anSelectedCol[byCard] := -1;
		end;

	m_game.byUnmatchedCardsFaceUp := 0;
	m_game.wCardsCompleted := 0;
end;

procedure TfrmMain.GenerateGameImages();
var
	nRow, nCol: Integer;
begin
	// How big can each image be? Leave a few pixels either side of each image for a border.
	m_game.nImgWidth :=
		((GameImageBox.Width - MemoryGame.GameSettings.nColumns * MemoryGame.GameSettings.nGridWidth) div
			MemoryGame.GameSettings.nColumns);
	m_game.nImgHeight :=
		((GameImageBox.Height - MemoryGame.GameSettings.nRows * MemoryGame.GameSettings.nGridWidth) div
			MemoryGame.GameSettings.nRows);

	// What is the total grid size and images required?
	m_cache.nGridSize := (MemoryGame.GameSettings.nRows * MemoryGame.GameSettings.nColumns);
	m_cache.nImagesRqd := (m_cache.nGridSize div 2);

	// Set the size each image will be on-screen (with border)
	m_cache.nImgWidthWithBorder := (GameImageBox.Width div MemoryGame.GameSettings.nColumns);
	m_cache.nImgHeightWithBorder := (GameImageBox.Height div MemoryGame.GameSettings.nRows);

	// Set up the bounding rectangles for all images
	for nRow:=1 to MemoryGame.GameSettings.nRows do
		begin
		for nCol:=1 to MemoryGame.GameSettings.nColumns do
			begin
			m_game.arctImages[nRow, nCol].Left :=
				((nCol * MemoryGame.GameSettings.nGridWidth) + (nCol-1) * m_game.nImgWidth);
			m_game.arctImages[nRow, nCol].Right :=
				(m_game.arctImages[nRow, nCol].Left + m_game.nImgWidth);

			m_game.arctImages[nRow, nCol].Top :=
				((nRow * MemoryGame.GameSettings.nGridWidth) + (nRow-1) * m_game.nImgHeight);
			m_game.arctImages[nRow, nCol].Bottom :=
				(m_game.arctImages[nRow, nCol].Top + m_game.nImgHeight);
			end;
		end;

	// Prepare the blank (face down) image
	DeleteImageCell(m_pImgBlank);
	m_pImgBlank := CreateImageCell(1,1,0,0);
	PrepareImageCell(m_pImgBlank, m_game.nImgWidth, m_game.nImgHeight);
	Expand24Coloured(m_pImgBlank, COLOUR_BACKGROUND);

	// Prepare the game images
	for nRow:=1 to MemoryGame.GameSettings.nRows do
		begin
		for nCol:=1 to MemoryGame.GameSettings.nColumns do
			begin
			// Create images the correct size
			PrepareImageCell(m_apImages[nRow, nCol], m_game.nImgWidth, m_game.nImgHeight);
			ClearImageCell(m_apImages[nRow, nCol], COLOUR_WHITE);
			Expand24(m_apImages[nRow, nCol]);
			end;
		end;

	// What type of image are we going to load? We could be loading:
	// * "Internal" > Personal images
	// * "Internal" > Numbers
	// * "Internal" > Letters
	// * "Folder" > User selected folder of images
	LoadGameImages();
end;

procedure TfrmMain.LoadGameImages();
var
	strFolder, strFile: String;
	nImageCount: Integer;
	nImage, nPosition, nRow, nColumn, nPartnerRow, nPartnerCol: Integer;
	anImageNumbers, anImagesToUse, anImageLocations: TList;
	wHours, wMins, wSecs, wMilliSecs: WORD;
	pTmpImg: PTIMAGECELL;
	rct: TRect;
begin
	// Called when generating the game images for "Internal" or "Folder"
	strFolder := MemoryGame.GetSourceFolder(MemoryGame.GameSettings);

	m_astrGameImages.Clear();
	GetFolderMultipleListing(strFolder, '*.bmp;*.jpg', m_astrGameImages);
	nImageCount := m_astrGameImages.Count;
	if (nImageCount >= m_cache.nImagesRqd) then
		begin
		// We have enough images...randomly choose "nImagesRequired" to use for the game

		// Create some lists to use
		anImageNumbers := TList.Create();
		anImagesToUse := TList.Create();
		anImageLocations := TList.Create();
		anImageNumbers.Clear();
		anImagesToUse.Clear();
		anImageLocations.Clear();

		// Create a list with the numbers 1,2,3,...,nImageCount
		for nImage:=1 to nImageCount do
			anImageNumbers.Add(Pointer(nImage));

		// Random selection so that each game is unique!
		DecodeTime(Now(), wHours, wMins, wSecs, wMilliSecs);
		RandSeed := wMilliSecs;
		RandSeed := (1 + Random(21474843647));
		while (anImagesToUse.Count < m_cache.nImagesRqd) do
			begin
			nImage := (1 + Random(anImageNumbers.Count));
			anImagesToUse.Add(Pointer(anImageNumbers[nImage-1]));
			anImageNumbers.Delete(nImage-1);
			end;

		// Assign the images to random locations in the grid
		AssignRandomLocations(anImageLocations, m_cache.nImagesRqd);

		// Now load the images
		if (	(anImagesToUse.Count = m_cache.nImagesRqd) and
				(anImageLocations.Count = m_cache.nGridSize)) then
			begin
			pTmpImg := CreateImageCell(1,1,0,0);
			nPosition := 1;
			for nImage:=1 to anImagesToUse.Count do
				begin
				// Load the image...
				strFile := m_astrGameImages[Integer(anImagesToUse[nImage-1])-1];
				if (IsFileExtType(strFile, '.bmp')) then
					LoadBMP(pTmpImg, 0, PAnsiChar(strFile))
				else if (IsFileExtType(strFile, '.jpg')) then
					LoadJPG(pTmpImg, strFile, nil, 0);

				// ...correct its aspect ratio...
				ImageCellCorrectAspect24(@pTmpImg, COLOUR_BACKGROUND,
					m_game.nImgWidth, m_game.nImgHeight);

				// ...and stretch it onto our sample image
				GetRowColumnFromPosition(Integer(anImageLocations[nPosition-1]), nRow, nColumn);
				rct.Left := 0;
				rct.Right := m_game.nImgWidth;
				rct.Top := 0;
				rct.Bottom := m_game.nImgHeight;
				StretchImageCellBitmap(pTmpImg, m_apImages[nRow, nColumn].handleDC, @rct);

				// Stretch the same image into the partner square
				nPartnerRow := m_game.anMatchSquare_Row[nRow, nColumn];
				nPartnerCol:= m_game.anMatchSquare_Col[nRow, nColumn];
				StretchImageCellBitmap(pTmpImg, m_apImages[nPartnerRow, nPartnerCol].handleDC, @rct);

				// Move onto the next position (we've just dealt with two)
				nPosition := (nPosition + 2);
				end;

			// Clean up
			DeleteImageCell(pTmpImg);
			end;

		// Clean up
		anImageNumbers.Free();
		anImagesToUse.Free();
		anImageLocations.Free();
		end;
end;

procedure TfrmMain.AssignRandomLocations(anImages: TList; nRequired: Integer);
var
	anLocations: TList;
	nImage, nPos, nLocation1, nLocation2: Integer;
	anLocation_Row, anLocation_Col: array[1..2] of Integer;
begin
	// Assume the grid size is 4x4 (16 locations). Since each image is duplicated, the total number
	// of images is therefore 8. This function attempts to assign each number to the grid twice
	// (each instance is one of a pair of matching cards). For example:
	// 3 1 8 3		[Locations 1 to 4]
	// 2 4 7 6		[Locations 5 to 8]
	// 1 2 6 8		[Locations 9 to 12]
	// 5 4 5 7		[Locations 13 to 16]

	// The above grid generates this list:
	//		2,9,5,10,1,4,6,14,13,15,8,11,7,16,3,12
	// The first two elements (2,9) represents the location of image "1", etc.

	// Generate the list of possible locations
	anLocations := TList.Create();
	anLocations.Clear();
	for nImage:=1 to (2 * nRequired) do
		anLocations.Add(Pointer(nImage));

	// Assign our numbers to random locations in the grid
	anImages.Clear();
	for nImage:=1 to (nRequired-1) do
		begin
		// Assign first card of the pair
		nPos := (1 + Random(anLocations.Count));
		nLocation1 := Integer(anLocations[nPos-1]);
		anImages.Add(Pointer(nLocation1));
		anLocations.Delete(nPos-1);

		// Assign second card of the pair
		nPos := (1 + Random(anLocations.Count));
		nLocation2 := Integer(anLocations[nPos-1]);
		anImages.Add(Pointer(nLocation2));
		anLocations.Delete(nPos-1);

		// Link the two locations together
		GetRowColumnFromPosition(nLocation1, anLocation_Row[1], anLocation_Col[1]);
		GetRowColumnFromPosition(nLocation2, anLocation_Row[2], anLocation_Col[2]);
		m_game.anMatchSquare_Row[anLocation_Row[1], anLocation_Col[1]] := anLocation_Row[2];
		m_game.anMatchSquare_Col[anLocation_Row[1], anLocation_Col[1]] := anLocation_Col[2];
		m_game.anMatchSquare_Row[anLocation_Row[2], anLocation_Col[2]] := anLocation_Row[1];
		m_game.anMatchSquare_Col[anLocation_Row[2], anLocation_Col[2]] := anLocation_Col[1];
		end;

	// Assign the last image to the two remaining locations
	nLocation1 := Integer(anLocations[0]);
	anImages.Add(Pointer(nLocation1));
	nLocation2 := Integer(anLocations[1]);
	anImages.Add(Pointer(nLocation2));

	// Link the two locations together
	GetRowColumnFromPosition(nLocation1, anLocation_Row[1], anLocation_Col[1]);
	GetRowColumnFromPosition(nLocation2, anLocation_Row[2], anLocation_Col[2]);
	m_game.anMatchSquare_Row[anLocation_Row[1], anLocation_Col[1]] := anLocation_Row[2];
	m_game.anMatchSquare_Col[anLocation_Row[1], anLocation_Col[1]] := anLocation_Col[2];
	m_game.anMatchSquare_Row[anLocation_Row[2], anLocation_Col[2]] := anLocation_Row[1];
	m_game.anMatchSquare_Col[anLocation_Row[2], anLocation_Col[2]] := anLocation_Col[1];

	// Clean up
	anLocations.Free();
end;

// Settings
procedure TfrmMain.RefreshSettings();
begin
	// Show the current game settings
	lblRows.Caption := Format('Rows: %d', [MemoryGame.GameSettings.nRows]);
	lblColumns.Caption := Format('Columns: %d', [MemoryGame.GameSettings.nColumns]);
end;

procedure TfrmMain.ResetLastBestGameStats();
begin
	// Statistics for the last (completed) and best game times
	m_game.dwLastCompletedTime := 0;
	m_game.dwBestCompletedTime := 999999999;
end;

procedure TfrmMain.ResetGameStats();
begin
	// Refresh data about a new game
	m_game.dwGameStart := GetTickCount();
	m_game.wClicks := 0;
	m_game.wCardsTurnedOver := 0;
end;

procedure TfrmMain.SetGameControls();
begin
	// If the game is running, show the game status
	lblTimeStartedTitle.Enabled := (m_game.bRunning);
	lblTimeStarted.Enabled := (m_game.bRunning);
	lblElapsedTimeTitle.Enabled := (m_game.bRunning);
	lblElapsedTime.Enabled := (m_game.bRunning);
	lblGameClicksTitle.Enabled := (m_game.bRunning);
	lblGameClicks.Enabled := (m_game.bRunning);
	lblCardsComplete_TurnedOverTitle.Enabled := (m_game.bRunning);
	lblCardsComplete_TurnedOver.Enabled := (m_game.bRunning);
end;

procedure TfrmMain.TurnOverCards();
var
	byCard: BYTE;
begin
	// When two tentative cards are face up (but don't match), turn them back to face down
	m_game.aSquares[m_game.anSelectedRow[1], m_game.anSelectedCol[1]] := eFaceDown;
	m_game.aSquares[m_game.anSelectedRow[2], m_game.anSelectedCol[2]] := eFaceDown;

	m_game.byUnmatchedCardsFaceUp := 0;
	Inc(m_game.wCardsTurnedOver, 2);
	for byCard:=1 to 2 do
		begin
		m_game.anSelectedRow[byCard] := -1;
		m_game.anSelectedCol[byCard] := -1;
		end;
end;

procedure TfrmMain.NotifyGameComplete();
var
	bNewRecord: Boolean;
begin
	// The user has finished a game!
	GameImageBoxPaint(Self);

	// Save the final image so that it can be displayed on-screen if we need to repaint
	ExtractImageCell24(m_pImgLastCompletedGame, m_pImgWholeGame, nil);
	m_bLastGameImage := True;

	// Set the stats for a finished game and see if a new record has been set
	m_game.dwLastCompletedTime := (GetTickCount() - m_game.dwGameStart);
	bNewRecord := False;
	if (m_game.dwLastCompletedTime < m_game.dwBestCompletedTime) then
		begin
		m_game.dwBestCompletedTime := m_game.dwLastCompletedTime;
		bNewRecord := True;
		end;

	m_game.wLastGameClicks := m_game.wClicks;
	m_game.wLastGameCardsTurnedOver := m_game.wCardsTurnedOver;

	if (bNewRecord) then
		MessageDlg(Format('Finished, you took %s. Thats a new record! Congratulations !!', [
			GetTimeString(m_game.dwLastCompletedTime, True)]), mtInformation, [mbOK], 0)
	else
		MessageDlg(Format('Finished, you took %s. Congratulations !!', [
			GetTimeString(m_game.dwLastCompletedTime, True)]), mtInformation, [mbOK], 0);

	m_dwBlockDrawTime := (GetTickCount() + cdwBlockDrawTime);
	GameImageBoxPaint(Self);
end;

// Drawing functions
procedure TfrmMain.DrawBorderTentative(nRow, nCol: Integer);
var
	rct: TRect;
begin
	// Draw a border around an image when its not yet matched with another (termed "tentative")
	m_hBorderTentativePen.Width := 1;
	SelectObject(m_pImgWholeGame.handleDC, m_hBorderTentativePen.Handle);
	rct := m_game.arctImages[nRow, nCol];

	MoveToEx(m_pImgWholeGame.handleDC,
		(rct.Left + MemoryGame.GameSettings.nGridWidth),
		(rct.Top + MemoryGame.GameSettings.nGridWidth + 1), nil);
	LineTo(m_pImgWholeGame.handleDC,
		(rct.Right - (2*MemoryGame.GameSettings.nGridWidth)),
		(rct.Top + MemoryGame.GameSettings.nGridWidth + 1));
	if (nRow = MemoryGame.GameSettings.nRows) then
		begin
		LineTo(m_pImgWholeGame.handleDC,
			(rct.Right - (2*MemoryGame.GameSettings.nGridWidth)),
			(rct.Bottom - MemoryGame.GameSettings.nGridWidth - 1));
		LineTo(m_pImgWholeGame.handleDC,
			(rct.Left + MemoryGame.GameSettings.nGridWidth),
			(rct.Bottom - MemoryGame.GameSettings.nGridWidth - 1));
		end
	else
		begin
		LineTo(m_pImgWholeGame.handleDC,
			(rct.Right - (2*MemoryGame.GameSettings.nGridWidth)),
			(rct.Bottom - MemoryGame.GameSettings.nGridWidth - 2));
		LineTo(m_pImgWholeGame.handleDC,
			(rct.Left + MemoryGame.GameSettings.nGridWidth),
			(rct.Bottom - MemoryGame.GameSettings.nGridWidth - 2));
		end;

	LineTo(m_pImgWholeGame.handleDC,
		(rct.Left + MemoryGame.GameSettings.nGridWidth),
		(rct.Top + MemoryGame.GameSettings.nGridWidth + 1));
end;

procedure TfrmMain.DrawGrid();
var
	nRow, nCol: Integer;
	nTmp: Integer;
begin
	// Draw the grid (borders around each object)
	m_hBorderPen.Color := MemoryGame.GameSettings.clGridColour;
	m_hBorderPen.Width := MemoryGame.GameSettings.nGridWidth;
	SelectObject(m_pImgWholeGame.handleDC, m_hBorderPen.Handle);

	// Rows
	for nRow:=1 to MemoryGame.GameSettings.nRows do
		begin
		nTmp := (m_game.arctImages[nRow, 1].Top - (MemoryGame.GameSettings.nGridWidth div 2));
		MoveToEx(m_pImgWholeGame.handleDC, 0, nTmp, nil);
		LineTo(m_pImgWholeGame.handleDC, m_pImgWholeGame.nWidth, nTmp);
		end;

	nTmp := (m_pImgWholeGame.nHeight - (MemoryGame.GameSettings.nGridWidth div 2));
	MoveToEx(m_pImgWholeGame.handleDC, 0, nTmp, nil);
	LineTo(m_pImgWholeGame.handleDC, m_pImgWholeGame.nWidth, nTmp);

	// Columns
	for nCol:=1 to MemoryGame.GameSettings.nColumns do
		begin
		nTmp := (m_game.arctImages[1, nCol].Left - (MemoryGame.GameSettings.nGridWidth div 2));
		MoveToEx(m_pImgWholeGame.handleDC, nTmp, 0, nil);
		LineTo(m_pImgWholeGame.handleDC, nTmp, m_pImgWholeGame.nHeight);
		end;

	nTmp := (m_pImgWholeGame.nWidth - (MemoryGame.GameSettings.nGridWidth div 2));
	MoveToEx(m_pImgWholeGame.handleDC, nTmp, 0, nil);
	LineTo(m_pImgWholeGame.handleDC, nTmp, m_pImgWholeGame.nHeight);
end;

// Buttons
procedure TfrmMain.EnableButtons(bEnable: Boolean);
begin
	btnNewGame.Enabled := (bEnable);
	btnSettings.Enabled := (bEnable);
	btnExit.Enabled := (bEnable);
end;

// Utility functions
procedure TfrmMain.GetRowColumnFromPosition(const cnPos: Integer;
	var nRow: Integer; var nColumn: Integer);
begin
	// Given the position in the grid, return its row and column. Assume a 3x4 grid (meaning 3 rows,
	// 4 columns). On the left we show the positions and on the right we show the corresponding
	// co-ordinate pair (row,column):
	// 01  02  03  04			1,1  1,2  1,3  1,4
	// 05  06  07  08			2,1  2,2  2,3  2,4
	// 09  10  11  12			3,1  3,2  3,3  3,4
	nColumn := cnPos;
	nRow := 1;
	while (nColumn > MemoryGame.GameSettings.nColumns) do
		begin
		nColumn := (nColumn - MemoryGame.GameSettings.nColumns);
		Inc(nRow);
		end;
end;

function TfrmMain.GetTimeString(dwTime: DWORD; bShowMilliSeconds: Boolean) : String;
var
	dwLocal, dwMinutes, dwSeconds, dwMilliSeconds: DWORD;
	strTime: String;
begin
	// Show a time in minutes and seconds (with optional milliseconds)
	dwLocal := dwTime;
	dwMinutes := (dwLocal div 60000);
	dwLocal := (dwLocal - 60000*dwMinutes);
	dwSeconds := (dwLocal div 1000);
	dwMilliSeconds := (dwLocal - 1000*dwSeconds);

	// More than 100 (3-digits) minutes?
	if (dwMinutes < 100) then
		strTime := Format('%.2d:%.2d', [dwMinutes, dwSeconds])
	else
		strTime := Format('%.3d:%.2d', [dwMinutes, dwSeconds]);

	// Show milliseconds?
	if (bShowMilliSeconds) then
		strTime := (strTime + Format('.%.3d', [dwMilliSeconds]));

	Result := strTime;
end;

procedure TfrmMain.ScreenShot(nX, nY: Integer; nWidth, nHeight: Integer; bmScreen: TBitMap);
var
	dcScreen: HDC;
	lpPalette: PLOGPALETTE;
begin
	// This code to grab the screen comes from About.com: Delphi Programming by Zarko Gajic

	// Test width and height
	if (nWidth = 0) or (nHeight = 0) or (bmScreen = nil) then
		Exit;

	// Set bitmap size
	bmScreen.Width := nWidth;
	bmScreen.Height := nHeight;

	// Get the screen DC
	dcScreen := GetDC(0);
	if (dcScreen = 0) then
		Exit;

	// Do we have a palette device?
	if ((GetDeviceCaps(dcScreen, RASTERCAPS) and RC_PALETTE) = RC_PALETTE) then
		begin
		// Allocate memory for a logical palette
		GetMem(lpPalette, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)));

		// Zero it out to be neat
		FillChar(lpPalette^, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)), #0);

		// Fill in the palette version
		lpPalette^.palVersion := $300;

		// Grab the system palette entries
		lpPalette^.palNumEntries := GetSystemPaletteEntries(
			dcScreen, 0, 256, lpPalette^.palPalEntry);

		// Create the palette?
		if (lpPalette^.PalNumEntries <> 0) then
			bmScreen.Palette := CreatePalette(lpPalette^);

		// Clean up memory
		FreeMem(lpPalette, sizeof(TLOGPALETTE) + (255 * sizeof(TPALETTEENTRY)));
	end;

	// Copy the screen to the bitmap
	BitBlt(bmScreen.Canvas.Handle, 0, 0, nWidth, nHeight, dcScreen, nX, nY, SRCCOPY);

	// Release the screen DC
	ReleaseDC(0, dcScreen);
end;
// Private functions: End

procedure TfrmMain.FormCreate(Sender: TObject);
var
	nRow, nCol: Integer;
begin
	// Initialise main form
	m_bFirstTimeShow := True;
	m_bExiting := False;
	m_bAllowDraw := True;
	m_dwBlockDrawTime := 0;

	// Create images
	m_pStartImageOriginal := CreateImageCell(1,1,0,0);
	m_pStartImageCorrected := CreateImageCell(1,1,0,0);

	m_pImgWholeGame := CreateImageCell(1,1,0,0);
	PrepareImageCell(m_pImgWholeGame, GameImageBox.Width, GameImageBox.Height);
	Expand24Coloured(m_pImgWholeGame, COLOUR_BACKGROUND);
	m_pImgLastCompletedGame := CreateImageCell(1,1,0,0);
	PrepareImageCell(m_pImgLastCompletedGame, GameImageBox.Width, GameImageBox.Height);
	Expand24Coloured(m_pImgLastCompletedGame, COLOUR_BACKGROUND);

	m_pImgBlank := CreateImageCell(1,1,0,0);
	// The blank image will be prepared when we start the game

	for nRow:=1 to MAX_SIZE do
		begin
		for nCol:=1 to MAX_SIZE do
			m_apImages[nRow, nCol] := CreateImageCell(1,1,0,0);
		end;

	m_astrGameImages := TStringList.Create();
	m_bLastGameImage := False;

	// GDI objects
	m_hFont := TFont.Create();
	m_hFont.Size := 32;
	m_hBorderTentativePen := TPen.Create();
	m_hBorderTentativePen.Width := 1;
	m_hBorderTentativePen.Color := clBlue;
	m_hBorderTentativePen.Style := psDash;
	m_hBorderPen := TPen.Create();
	m_hBorderPen.Width := GRID_WIDTH_DEFAULT;
	m_hBorderPen.Color := clRed;

	// Game state
	ZeroMemory(@m_game, SizeOf(GAME_SETTINGS));
	m_game.bRunning := False;
	m_game.dwBestCompletedTime := 999999999;

	// Timers and cache
	ZeroMemory(@m_timers, SizeOf(TIMER_SETTINGS));
	m_timers.dwRefreshDone := GetTickCount();
	m_timers.dwLastLabelsUpdate := (GetTickCount() - 1000);
	ZeroMemory(@m_cache, SizeOf(CACHE_SETTINGS));

	// Initialise system and form utilities
	InitialiseSystemUtils();
	InitialiseFormUtils();

	// Debugging
	gbDebugging.Visible := {$IFDEF DBG} True {$ELSE} False {$ENDIF};
	m_dwRedrawCount := 0;
	m_dwResizeCount := 0;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
	nRow, nCol: Integer;
begin
	// Exiting!
	m_bExiting := True;

	// Stop the system clock
	SystemClock.Enabled := False;

	// Clean up images
	DeleteImageCell(m_pStartImageOriginal);
	DeleteImageCell(m_pStartImageCorrected);
	DeleteImageCell(m_pImgWholeGame);
	DeleteImageCell(m_pImgBlank);
	for nRow:=1 to MAX_SIZE do
		begin
		for nCol:=1 to MAX_SIZE do
			DeleteImageCell(m_apImages[nRow, nCol]);
		end;

	m_astrGameImages.Free();

	// Clean up GDI objects
	m_hFont.Free();
	m_hBorderTentativePen.Free();
	m_hBorderPen.Free();

	// Close system and form utilities
	CloseSystemUtils();
	CloseFormUtils();

	// Close machine object (which will save settings)
	MemoryGame.PrepareToExit();
	MemoryGame.Free();
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
	if (m_bFirstTimeShow) then
		begin
		// Initialise the system
		FirstTimeInit();
		m_bFirstTimeShow := False;

		// Set focus to the "Exit" button
		btnExit.SetFocus();

		// Set the top position to be close to the top of the screen
		Self.Top := 5;
		Self.Left := ((Screen.Width - Self.ClientWidth) div 2);

		// Start the update timer
		SystemClock.Enabled := True;
	end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
	// Nothing to do
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: WORD; Shift: TShiftState);
var
	bmScreenShot: TBitmap;
begin
	// Most forms will propagate key presses back to the main form for processing, such as loading
	// images. Be careful about whether to propagate the VK_ESC key back because that shuts the
	// application down!

	// Note: In order to trap key press events on the form, do the following:
	// (1) Set the KeyPreview property to True
	// (2) Handle the form OnKeyDown event
	case Key of
		VK_ESCAPE:
			begin
			// Esc key: Exit the application
			m_game.bRunning := False;
			m_bExiting := True;
			MemoryGame.PrepareToExit();
			Application.Terminate();
			end;
		VK_F1:
			begin
			// F1 key: ??
			end;
		VK_F11:
			begin
			// F11 key: Save the entire screen (desktop)
			bmScreenShot := TBitmap.Create();
			ScreenShot(0, 0, Screen.Width, Screen.Height, bmScreenShot);
			MemoryGame.SaveBitmapToUSB('Screenshot', 'SoftwareScreenshot', bmScreenShot);
			bmScreenShot.FreeImage();
			FreeAndNil(bmScreenShot);
			end;
	end;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
	// TODO: Handle resizing the main form, currently the border is fixed

	// Handles form resizing
	Inc(m_dwResizeCount);
	m_timers.dwRefreshRequest := (GetTickCount() + 2000);

	{$IFDEF DBG}
	// Show the new form size
	lblFormSize.Caption := Format('W: %d, H: %d', [Self.ClientWidth, Self.ClientHeight]);
	{$ENDIF}

	// Calculate which dimension (vertical or horizontal) needs to be extended

	// Resize the controls on the form
	GameBackPanel.Width := (Self.ClientWidth - m_cache.nBackPanelWidthOffset);
	GameBackPanel.Height := (Self.ClientHeight - m_cache.nBackPanelHeightOffset);
end;

procedure TfrmMain.btnNewGameClick(Sender: TObject);
begin
	// Start or stop the game
	if (m_game.bRunning) then
		begin
		// Stop the game
		m_game.bRunning := False;
		m_game.bTimeIsRunning := False;
		m_timers.dwRefreshRequest := (GetTickCount() + 1000);

		// Change the text to "New"
		btnNewGame.Caption := 'New';

		// Enable settings
		btnSettings.Enabled := True;
		end
	else
		begin
		// Disable the "New" button while we initialise. This can be slow on removable media (such
		// as CDs or pen drives).
		btnNewGame.Enabled := False;

		// Start a new game
		InitialiseGame();

		// Generate the images that will be used in the game
		GenerateGameImages();

		// Now start running the game
		m_game.bRunning := True;
		m_game.bTimeIsRunning := True;
		lblTimeStarted.Caption := FormatDateTime('hh:nn:ss', Now());

		// Reset some stats about the current game
		ResetGameStats();
		SetGameControls();

		// Change the text and re-enable the "New" button
		btnNewGame.Caption := 'Stop';
		btnNewGame.Enabled := True;

		// Enable settings
		btnSettings.Enabled := True;
		end;
end;

procedure TfrmMain.btnSettingsClick(Sender: TObject);
begin
	// Display game settings
	m_bAllowDraw := False;
	EnableButtons(False);
	frmSettings := TfrmSettings.Create(Self);
	frmSettings.settings := MemoryGame.GameSettings;
	frmSettings.SetGameRunning(m_game.bRunning);
	if (frmSettings.ShowModal() = mrOk) then
		begin
		// Has the game size changed? If so, reset the last completed and best game times.
		if (	(MemoryGame.GameSettings.nRows <> frmSettings.settings.nRows) or
				(MemoryGame.GameSettings.nColumns <> frmSettings.settings.nColumns)) then
			ResetLastBestGameStats();

		// Settings have been changed, copy them back
		MemoryGame.GameSettings := frmSettings.settings;
		RefreshSettings();
		end;

	// Clean up
	frmSettings.Free();
	frmSettings := nil;
	EnableButtons(True);
	m_bAllowDraw := True;
	GameImageBoxPaint(Self);
end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
	// Exit
	if (m_bExiting) then
		Exit;

	m_game.bRunning := False;
	m_bExiting := True;
	MemoryGame.PrepareToExit();
	Application.Terminate();
end;

procedure TfrmMain.OnSystemClockTick(Sender: TObject);
var
	dwCurrentTime, dwElapsed: DWORD;
begin
	// Disable the timer
	SystemClock.Enabled := False;
	if (m_bExiting) then
		Exit;

	// Get the current time
	dwCurrentTime := GetTickCount();

	// Only update labels once a second
	dwElapsed := (dwCurrentTime - m_timers.dwLastLabelsUpdate);
	if (dwElapsed > 1000) then
		begin
		// Update labels
		m_timers.dwLastLabelsUpdate := dwCurrentTime;
		btnSettings.Enabled := (not m_game.bRunning);

		// Show statistics about the current game
		if (m_game.bRunning) then
			begin
			// Game is running

			// Elapsed time
			if (m_game.bTimeIsRunning) then
				begin
				dwElapsed := (dwCurrentTime - m_game.dwGameStart);
				lblElapsedTime.Caption := GetTimeString(dwElapsed, False);
				end;

			// Game clicks and cards turned over
			lblGameClicks.Caption := IntToStr(m_game.wClicks);
			lblCardsComplete_TurnedOver.Caption := Format('%d (%d)', [
				m_game.wCardsCompleted, m_game.wCardsTurnedOver]);
			end
		else
			begin
			// Game is not running
			if (m_game.wGamesCompleted > 0) then
				begin
				// At least one game has been completed

				// Elapsed time
				lblElapsedTime.Caption := GetTimeString(m_game.dwLastCompletedTime, False);

				// Game clicks and cards turned over
				lblGameClicks.Caption := IntToStr(m_game.wLastGameClicks);
				lblCardsComplete_TurnedOver.Caption := IntToStr(m_game.wLastGameCardsTurnedOver);
				end
			else
				begin
				// No games have been completed yet
				lblTimeStarted.Caption := 'None';
				lblElapsedTime.Caption := 'None';
				lblGameClicks.Caption := 'None';
				lblCardsComplete_TurnedOver.Caption := 'None';
				end;
			end;

		// Show stats about last completed game times
		if (m_game.wGamesCompleted = 0) then
			begin
			lblLastGameTime.Caption := 'None';
			lblBestGameTime.Caption := 'None';
			end
		else
			begin
			lblLastGameTime.Caption := GetTimeString(m_game.dwLastCompletedTime, True);
			lblBestGameTime.Caption := GetTimeString(m_game.dwBestCompletedTime, True);
			end;
		end;	// if (dwElapsed > 1000) then

	// Refresh the game (eg. after a resize)?
	if (m_timers.dwRefreshDone <= m_timers.dwRefreshRequest) then
		begin
		// Refresh the game!
		GameImageBoxPaint(Self);
		m_timers.dwRefreshDone := dwCurrentTime;
		end
	else
		begin
		// Refresh the game each cycle of the clock if we are running
		if (m_game.bRunning) then
			GameImageBoxPaint(Self);
		end;

	// Re-enable the system timer
	SystemClock.Enabled := True;
end;

procedure TfrmMain.GameImageBoxPaint(Sender: TObject);
var
	nRow, nCol: Integer;
	rct: TRect;
begin
	// Normally we'll be allowed to redraw the screen
	if (not m_bAllowDraw) then
		Exit;

	// Block drawing for a short time? This is often required when the end-of-game confirmation
	// message is displayed, which forces a repaint of the screen as the dialog disappears.
	if (GetTickCount() < m_dwBlockDrawTime) then
		begin
		BitBlt(GameImageBox.Canvas.Handle, 0, 0, GameImageBox.Width, GameImageBox.Height,
			m_pImgWholeGame.handleDC, 0, 0, SRCCOPY);
		Exit;
		end;

	// Paint the game
	Inc(m_dwRedrawCount);
	if (m_game.bRunning) then
		begin
		// Game is running!

		// Draw images to our "whole game" image
		for nRow:=1 to MemoryGame.GameSettings.nRows do
			begin
			for nCol:=1 to MemoryGame.GameSettings.nColumns do
				begin
				// Blt the image to the screen
				rct := m_game.arctImages[nRow, nCol];
				if (m_game.aSquares[nRow, nCol] = eFaceDown) then
					ExtractImageCellToPosition24(m_pImgWholeGame, m_pImgBlank, rct.Left, rct.Top)
				else if (m_game.aSquares[nRow, nCol] = eFaceUpTentative) then
					begin
					ExtractImageCellToPosition24(m_pImgWholeGame, m_apImages[nRow, nCol], rct.Left, rct.Top);
					DrawBorderTentative(nRow, nCol);
					end
				else if (m_game.aSquares[nRow, nCol] = eFaceUpComplete) then
					ExtractImageCellToPosition24(m_pImgWholeGame, m_apImages[nRow, nCol], rct.Left, rct.Top);
				end;
			end;

		// Draw game grid
		DrawGrid();

		// Blt the image to the screen
		BitBlt(GameImageBox.Canvas.Handle, 0, 0, GameImageBox.Width, GameImageBox.Height,
			m_pImgWholeGame.handleDC, 0, 0, SRCCOPY);
		end
	else
		begin
		// Game is not running, show the sample image or the image of the last completed game
		if (m_bLastGameImage) then
			begin
			// We have an image for the last completed game, so show that
			BitBlt(GameImageBox.Canvas.Handle, 0, 0, GameImageBox.Width, GameImageBox.Height,
				m_pImgLastCompletedGame.handleDC, 0, 0, SRCCOPY);
			end
		else
			begin
			// Show the sample image
			rct.Left := 0;
			rct.Top := 0;
			rct.Right := (GameImageBox.Width - 1);
			rct.Bottom := (GameImageBox.Height - 1);
			StretchImageCellBitmap(m_pStartImageCorrected, GameImageBox.Canvas.Handle, @rct);
			end;
		end;
end;

procedure TfrmMain.GameImageBoxClick(Sender: TObject);
var
	pt: TPoint;
	nThisRow, nThisCol: Integer;
	bMatchedRow, bMatchedCol: Boolean;
	byCard: BYTE;
begin
	// Clicked the game...
	if (m_game.bRunning) then
		begin
		// Another click
		Inc(m_game.wClicks);

		// Which image was clicked?
		pt := GameImageBox.ScreenToClient(Mouse.CursorPos);
		nThisRow := ((pt.Y div m_cache.nImgHeightWithBorder) + 1);
		nThisCol := ((pt.X div m_cache.nImgWidthWithBorder) + 1);
		if (m_game.aSquares[nThisRow, nThisCol] = eFaceDown) then
			begin
			// Card is face down. If two tentative cards are face up, turn them both face down. If
			// one or none tentative cards are face up, turn this card over and (if applicable)
			// check whether it matches the other tentative card.
			// Note: There should only ever be 0, 1 or 2 tentative cards face up!
			if (m_game.byUnmatchedCardsFaceUp < 2) then
				begin
				// One or no tentative cards are face up
				m_game.aSquares[nThisRow, nThisCol] := eFaceUpTentative;
				Inc(m_game.byUnmatchedCardsFaceUp);
				Inc(m_game.wCardsTurnedOver);

				// If this is the first tentative card, set the selected row / column
				if (m_game.byUnmatchedCardsFaceUp = 1) then
					begin
					// Set the first selected card
					m_game.anSelectedRow[1] := nThisRow;
					m_game.anSelectedCol[1] := nThisCol;
					end;

				// If there are now two tentative cards face up, check for a match
				if (m_game.byUnmatchedCardsFaceUp = 2) then
					begin
					// Set the second selected card
					m_game.anSelectedRow[2] := nThisRow;
					m_game.anSelectedCol[2] := nThisCol;

					// Check for a match with the first card
					bMatchedRow :=
						(m_game.anMatchSquare_Row[nThisRow, nThisCol] = m_game.anSelectedRow[1]);
					bMatchedCol :=
						(m_game.anMatchSquare_Col[nThisRow, nThisCol] = m_game.anSelectedCol[1]);
					if (bMatchedRow) and (bMatchedCol) then
						begin
						// Matched cards!
						m_game.aSquares[nThisRow, nThisCol] := eFaceUpComplete;
						m_game.aSquares[
							m_game.anMatchSquare_Row[nThisRow, nThisCol],
							m_game.anMatchSquare_Col[nThisRow, nThisCol]] := eFaceUpComplete;

						for byCard:=1 to 2 do
							begin
							m_game.anSelectedRow[byCard] := -1;
							m_game.anSelectedCol[byCard] := -1;
							end;

						m_game.byUnmatchedCardsFaceUp := 0;
						Inc(m_game.wCardsCompleted, 2);

						// Have we finished?
						if (m_game.wCardsCompleted = m_cache.nGridSize) then
							begin
							// Finished!

							// Stop the time and tell the user they have finished
							m_game.bTimeIsRunning := False;
							NotifyGameComplete();

							// Now stop the game
							m_game.bRunning := False;
							Inc(m_game.wGamesCompleted);
							SetGameControls();
							btnNewGame.Caption := 'New';
							end;
						end;
					end;
				end		// if (m_game.byUnmatchedCardsFaceUp < 2) then
			else
				begin
				// Two or more tentative cards are face up, but did not match. Put them face down.
				// Note: There should never be more than two tentative cards face up!
				TurnOverCards();

				// Now select the new card
				m_game.aSquares[nThisRow, nThisCol] := eFaceUpTentative;
				Inc(m_game.byUnmatchedCardsFaceUp);
				Inc(m_game.wCardsTurnedOver);

				// Set the first selected card
				m_game.anSelectedRow[1] := nThisRow;
				m_game.anSelectedCol[1] := nThisCol;
				end;
			end
		else if (m_game.aSquares[nThisRow, nThisCol] = eFaceUpTentative) then
			begin
			// Card is face up, tentative. If two tentaive cards are face up, turn them both face
			// down. If the count is one, do nothing.
			if (m_game.byUnmatchedCardsFaceUp = 2) then
				TurnOverCards();
			end
		else if (m_game.aSquares[nThisRow, nThisCol] = eFaceUpComplete) then
			begin
			// Card is already complete, take no action unless two tentative cards are face up
			if (m_game.byUnmatchedCardsFaceUp = 2) then
				TurnOverCards();
			end;
		end;	// if (m_game.bRunning) then
end;

end.
