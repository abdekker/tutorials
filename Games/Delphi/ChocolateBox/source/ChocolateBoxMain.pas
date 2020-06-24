unit ChocolateBoxMain;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Contnrs, Controls, ExtCtrls, Forms, Graphics, StdCtrls, SysUtils,
  CoreFormClasses, CoreTypes, FormUtils, GameMachine, SystemUtils;

type
  CACHE_GAME_BOARD = record
	// Main board cache
	bInitialisedBoxes: Boolean;
	aBoxes: TObjectList;
  end;

  // Main form
  TfrmChocolateBox = class(TGeneralBaseForm)

	UpdateTimer: TTimer;
	imgBackground: TImage;

	imgSettings: TImage;
	lblSettings: TStaticText;
	imgExit: TImage;
	lblExit: TStaticText;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure imgSettingsClick(Sender: TObject);
	procedure imgExitClick(Sender: TObject);
	procedure imgChocolateBoxClick(Sender: TObject);

	procedure OnUpdateTimer(Sender: TObject);

  private
	{ Private declarations }
	// Initialisation and system-state flags
	m_bFirstTimeShow, m_bExiting, m_bAllowDraw: Boolean;

	// Cache
	m_cache: CACHE_GAME_BOARD;

	// Initialisation
	procedure FirstTimeInit();
	procedure UpdateGameBoard();

  public
	{ Public declarations }
  end;

var
  frmChocolateBox: TfrmChocolateBox;

implementation

uses
  Dialogs, Math, GameSettings;

const
  DUMMY_CONST = 0;

type
  // Available actions for each category
  TDummy = (
	eDummy1
  );

{$R *.dfm}

procedure TfrmChocolateBox.FormCreate(Sender: TObject);
begin
	// Initialise main form
	m_bFirstTimeShow := True;
	m_bExiting := False;
	m_bAllowDraw := True;

	// Seed the random number generator (7919 is the 1000th prime number)
	RandSeed := Integer(GetTickCount() mod 7919);

	// Initialise system and form utilities
	InitialiseSystemUtils();
	InitialiseFormUtils();
end;

procedure TfrmChocolateBox.FormDestroy(Sender: TObject);
begin
	// Clean up

	// Close system and form utilities
	CloseSystemUtils();
	CloseFormUtils();

	// Close machine object (which will save settings)
	ChocolateBox.PrepareToExit();
	ChocolateBox.Free();
end;

procedure TfrmChocolateBox.FormShow(Sender: TObject);
begin
	// Show form
	if (m_bFirstTimeShow) then
		begin
		// Initialise the system
		FirstTimeInit();
		m_bFirstTimeShow := False;

		// Set the form size
		Self.Width := ChocolateBox.GameCache.nScreenWidth;
		Self.Height := ChocolateBox.GameCache.nScreenHeight;

		// Set the top position to be close to the top of the screen
		Self.Top := 5;
		Self.Left := ((Screen.Width - Self.ClientWidth) div 2);

		// Load the starting background image
		imgBackground.Width := Self.Width;
		imgBackground.Height := Self.Height;
		imgBackground.Picture.LoadFromFile(ChocolateBox.GameCache.szAppPath + 'ChocolateBox-2.jpg');
		//imgBackground.Picture.LoadFromFile(ChocolateBox.GameCache.szAppPath + 'ChocolateBox-1.jpg');

		// Start the update timer
		UpdateTimer.Enabled := True;
	end;
end;

procedure TfrmChocolateBox.imgSettingsClick(Sender: TObject);
begin
	// Display game settings
	m_bAllowDraw := False;
	frmGameSettings := TfrmGameSettings.Create(Self);
	frmGameSettings.settings := ChocolateBox.GameSettings;
	frmGameSettings.SetGameRunning(False);
	if (frmGameSettings.ShowModal() = mrOk) then
		begin
		// Settings have been changed, copy them back
		ChocolateBox.GameSettings := frmGameSettings.settings;
		UpdateGameBoard();
		end;

	// Clean up
	frmGameSettings.Free();
	frmGameSettings := nil;
	m_bAllowDraw := False;
end;

procedure TfrmChocolateBox.imgExitClick(Sender: TObject);
begin
	// Close form
	m_bExiting := True;
	ChocolateBox.PrepareToExit();
	Close();
end;

procedure TfrmChocolateBox.imgChocolateBoxClick(Sender: TObject);
var
	nRow, nCol: Integer;
begin
	// One of the chocolates has been clicked! The tag encodes the row and column.
	nCol := (TImage(Sender).Tag mod ChocolateBox.GameSettings.nColumns);
	if (nCol = 0) then
		nCol := ChocolateBox.GameSettings.nColumns;

	nRow := (((TImage(Sender).Tag - nCol) div ChocolateBox.GameSettings.nColumns) + 1);

	// Debug: Pop up a message
	MessageDlg(Format('Tag: %d, row: %d, col: %d', [TImage(Sender).Tag, nRow, nCol]), mtWarning, [mbOK], 0);
end;

procedure TfrmChocolateBox.OnUpdateTimer(Sender: TObject);
begin
	// An update timer event...
	if (m_bExiting) then
		Exit;

	// Disable timer while we perform the task for that timer
	UpdateTimer.Enabled := False;

	// Restart the timer
	UpdateTimer.Enabled := True;
end;

// Private functions: Start
procedure TfrmChocolateBox.FirstTimeInit();
begin
	// See the random number generator
	RandSeed := Integer(GetTickCount() mod 1009);	// 1009 is the first prime > 1000

	// Initialise the local cache
	ZeroMemory(@m_cache, SizeOf(CACHE_GAME_BOARD));
	m_cache.bInitialisedBoxes := False;

	// Load main game settings
	ChocolateBox := TGameMachine.Create();
	ChocolateBox.InitSystem();
	UpdateGameBoard();
end;

procedure TfrmChocolateBox.UpdateGameBoard();
const
	BOXES_LEFT: Integer		= 60;
	BOXES_RIGHT: Integer	= 40;
	BOXES_TOP: Integer		= 150;
	BOXES_BOTTOM: Integer	= 40;
	BOXES_WIDTH: Integer	= 64;
	BOXES_HEIGHT: Integer	= 64;
var
	nAvailableSpace, nGapHorizontal, nGapVertical: Integer;
	nBox, nRow, nCol: Integer;
	strImagePath: String;
	astrImages: TStringList;
	imgBox: TImage;
begin
	// Clear the current boxes
	if (	(m_cache.bInitialisedBoxes) and
			(m_cache.aBoxes <> nil)) then
		begin
		for nBox:=0 to (m_cache.aBoxes.Count-1) do
			begin
			TImage(m_cache.aBoxes[nBox]).Visible := False;
			Self.RemoveControl(TImage(m_cache.aBoxes[nBox]));
			end;

		m_cache.aBoxes.Free();
		m_cache.aBoxes := nil;
		m_cache.bInitialisedBoxes := False;
		end;

	// Calculate where the new boxes should go to spread them nicely over the available space
	if (ChocolateBox.GameSettings.nColumns > 1) then
		begin
		nAvailableSpace := (
			ChocolateBox.GameCache.nScreenWidth -
			BOXES_LEFT -
			BOXES_RIGHT -
			(BOXES_WIDTH * ChocolateBox.GameSettings.nColumns));
		nGapHorizontal := (nAvailableSpace div (ChocolateBox.GameSettings.nColumns - 1));
		end
	else
		nGapHorizontal := 0;

	if (ChocolateBox.GameSettings.nRows > 1) then
		begin
		nAvailableSpace := (
			ChocolateBox.GameCache.nScreenHeight -
			BOXES_TOP -
			BOXES_BOTTOM -
			(BOXES_HEIGHT * ChocolateBox.GameSettings.nRows));
		nGapVertical := (nAvailableSpace div (ChocolateBox.GameSettings.nRows - 1));
		end
	else
		nGapVertical := 0;

	// Get a listing of available image files
	astrImages := TStringList.Create();
	strImagePath := (ChocolateBox.GameCache.szAppPath + 'Icons');
	GetFolderListing(strImagePath, '*.ico', astrImages);

	// Create a new grid of boxes
	m_cache.aBoxes := TObjectList.Create();
	for nRow:=1 to ChocolateBox.GameSettings.nRows do
		begin
		for nCol:=1 to ChocolateBox.GameSettings.nColumns do
			begin
			imgBox := TImage.Create(Self);
			imgBox.Visible := False;

			// Set the properties for each label
			with imgBox do
				begin
				// Main properties
				Tag := (nCol + (nRow - 1)*ChocolateBox.GameSettings.nColumns);
				Name := Format('imgBox%d', [Tag]);
				Parent := Self;
				if (astrImages.Count > 0) then
					Picture.LoadFromFile(astrImages[Random(astrImages.Count)])
				else
					Picture := imgSettings.Picture;

				OnClick := imgChocolateBoxClick;

				// Image position
				Left := (BOXES_LEFT + (BOXES_WIDTH + nGapHorizontal)*(nCol - 1));
				Top := (BOXES_TOP + (BOXES_HEIGHT + nGapVertical)*(nRow - 1));
				end;

			// Add box to list
			m_cache.aBoxes.Add(imgBox);
			end;
		end;

	// Make all the boxes visible
	for nBox:=0 to (m_cache.aBoxes.Count-1) do
		TLabel(m_cache.aBoxes[nBox]).Visible := True;

	// Clean up
	astrImages.Free();

	// Boxes are initialised!
	m_cache.bInitialisedBoxes := True;
end;
// Private functions: Start

end.
