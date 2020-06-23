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
  Math, GameSettings;

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

		// Start the update timer
		UpdateTimer.Enabled := True;
	end;
end;

procedure TfrmChocolateBox.imgExitClick(Sender: TObject);
begin
	// Close form
	m_bExiting := True;
	ChocolateBox.PrepareToExit();
	Close();
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
	// Initialise the local cache
	ZeroMemory(@m_cache, SizeOf(CACHE_GAME_BOARD));
	m_cache.bInitialisedBoxes := False;

	// Load main game settings
	ChocolateBox := TGameMachine.Create();
	ChocolateBox.InitSystem();
end;

procedure TfrmChocolateBox.UpdateGameBoard();
const
	BOXES_LEFT: Integer		= 70;
	BOXES_TOP: Integer		= 90;
	BOXES_WIDTH: Integer	= 64;
	BOXES_HEIGHT: Integer	= 64;
	BOXES_GAP: Integer		= 18;
var
	nBox, nRow, nCol: Integer;
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
				Picture := imgSettings.Picture;

				// Image position
				Left := (BOXES_LEFT + (BOXES_WIDTH + BOXES_GAP)*(nCol - 1));
				Top := (BOXES_TOP + (BOXES_HEIGHT + BOXES_GAP)*(nRow - 1));
				end;

			// Add box to list
			m_cache.aBoxes.Add(imgBox);
			end;
		end;

	// Make all the boxes visible
	for nBox:=0 to (m_cache.aBoxes.Count-1) do
		TLabel(m_cache.aBoxes[nBox]).Visible := True;

	// Boxes are initialised!
	m_cache.bInitialisedBoxes := True;
end;
// Private functions: Start

end.
