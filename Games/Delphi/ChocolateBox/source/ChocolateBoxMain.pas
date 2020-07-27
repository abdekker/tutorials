unit ChocolateBoxMain;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Contnrs, Controls, ExtCtrls, Forms, Graphics, StdCtrls, SysUtils,
  CoreFormClasses, CoreTypes, FormUtils, GameMachine, GameTypes, SystemUtils;

type
  CACHE_GAME_BOARD = record
	// Main board cache
	bInitialisedBoxes: Boolean;
	aBoxes: TObjectList;

	// Settings
	nLastGridRows, nLastGridColumns: Integer;
	szLastBackground: String;
	tLastBackgroundColour: Integer;
	eLastIconSet: TGameIconSet;

	// Last selected item
	nLastSelectedTag, nLastSelectedRow, nLastSelectedColumn: Integer;
  end;

  // Main form
  TfrmChocolateBox = class(TGeneralBaseForm)

	UpdateTimer: TTimer;
	imgBackground: TImage;

	imgSettings: TImage;
	lblSettings: TStaticText;
	imgMedia: TImage;
	lblMedia: TStaticText;
	imgExit: TImage;
	lblExit: TStaticText;
	imgAbout: TImage;
	imgEngineer: TImage;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure imgBackgroundClick(Sender: TObject);
	procedure imgSettingsClick(Sender: TObject);
	procedure imgMediaClick(Sender: TObject);
	procedure imgExitClick(Sender: TObject);
	procedure imgEngineerClick(Sender: TObject);
	procedure imgAboutClick(Sender: TObject);
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
	procedure UpdateGameBackground(settings: GAME_SETTINGS);
	procedure UpdateGameBoard(settings: GAME_SETTINGS);

	// Feedback for settings
	procedure SettingsCallback(pSettings: Pointer);

  public
	{ Public declarations }
  end;

var
  frmChocolateBox: TfrmChocolateBox;

implementation

uses
  Dialogs, Math,
  GameAbout, SettingsAdvanced, SettingsMedia, SettingsUserInterface;

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
var
	strTest1, strTest2: String;
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

		// Set up the game background (which is the full screen)
		imgBackground.Width := Self.Width;
		imgBackground.Height := Self.Height;

		// Show Help and Engineer (advanced) icons in the upper right
		imgEngineer.Top := imgSettings.Top;
		imgEngineer.Left := (imgBackground.Width - imgEngineer.Width - imgSettings.Left);
		imgAbout.Top := imgEngineer.Top;
		imgAbout.Left := (imgEngineer.Left - imgAbout.Width - 15);
			//(imgMedia.Left - imgSettings.Left - imgSettings.Width));

		// Start the update timer
		UpdateTimer.Enabled := True;

		RegGetString(HKEY_CLASSES_ROOT, '.txt', strTest1);
		RegGetExpandString(HKEY_CLASSES_ROOT, strTest1 + '\shell\open\command\', strTest2);

		// Completed once-off initialisation
		m_bFirstTimeShow := False;
		end;
end;

procedure TfrmChocolateBox.imgBackgroundClick(Sender: TObject);
var
	nControl, nImageBackground, nControlCloseClick: Integer;
begin
	// Find the index of the background image (because we ignore clicks on this image)
	nImageBackground := -1;
	for nControl:=0 to (Self.ControlCount - 1) do
		begin
		if (Self.Controls[nControl].Name = 'imgBackground') then
			begin
			nImageBackground := nControl;
			break;
			end;
		end;

	// Find a nearby TImage (or sub-class of TImage) control...and click it! We also look for
	// TStaticText controls
	nControlCloseClick := GetCloseControlClick(Self,
		(CONTROL_TIMAGE + CONTROL_TSTATICTEXT), nImageBackground);
	if (nControlCloseClick > -1) then
		begin
		// Close to a TImage control
		if (IsControlType(Self.Controls[nControlCloseClick], CONTROL_TIMAGE)) then
			TImage(Self.Controls[nControlCloseClick]).OnClick(Self.Controls[nControlCloseClick])
		else if (IsControlType(Self.Controls[nControlCloseClick], CONTROL_TSTATICTEXT)) then
			TStaticText(Self.Controls[nControlCloseClick]).OnClick(Self.Controls[nControlCloseClick]);
		end;
end;

procedure TfrmChocolateBox.imgSettingsClick(Sender: TObject);
var
	settingsBackup: GAME_SETTINGS;
begin
	// Display user interface settings
	m_bAllowDraw := False;
	settingsBackup := ChocolateBox.GameSettings;
	frmSettingsUserInterface := TfrmSettingsUserInterface.Create(Self);
	frmSettingsUserInterface.settings := ChocolateBox.GameSettings;
	frmSettingsUserInterface.RegisterSettingsCallback(SettingsCallback);
	if (frmSettingsUserInterface.ShowModal() = mrOk) then
		begin
		// Settings have been changed, copy them back
		ChocolateBox.GameSettings := frmSettingsUserInterface.settings;
		end
	else
		begin
		// User cancelled, so use the backup settings
		ChocolateBox.GameSettings := settingsBackup;
		end;

	// Redraw the board
	UpdateGameBackground(ChocolateBox.GameSettings);
	UpdateGameBoard(ChocolateBox.GameSettings);

	// Clean up
	frmSettingsUserInterface.Free();
	frmSettingsUserInterface := nil;
	m_bAllowDraw := False;
end;

procedure TfrmChocolateBox.imgMediaClick(Sender: TObject);
var
	settingsBackup: GAME_SETTINGS;
begin
	// Display media settings
	m_bAllowDraw := False;
	settingsBackup := ChocolateBox.GameSettings;
	frmSettingsMedia := TfrmSettingsMedia.Create(Self);
	frmSettingsMedia.settings := ChocolateBox.GameSettings;
	// No need to register a callback for media settings
	if (frmSettingsMedia.ShowModal() = mrOk) then
		begin
		// Settings have been changed, copy them back
		ChocolateBox.GameSettings := frmSettingsMedia.settings;
		end
	else
		begin
		// User cancelled, so use the backup settings
		ChocolateBox.GameSettings := settingsBackup;
		end;

	// Clean up
	frmSettingsMedia.Free();
	frmSettingsMedia := nil;
	m_bAllowDraw := False;
end;

procedure TfrmChocolateBox.imgExitClick(Sender: TObject);
begin
	// Close form
	m_bExiting := True;
	ChocolateBox.PrepareToExit();
	Close();
end;

procedure TfrmChocolateBox.imgAboutClick(Sender: TObject);
begin
	// Show a Help > About box
	frmGameAbout := TfrmGameAbout.Create(Self);
	frmGameAbout.ShowModal();
	frmGameAbout.Free();
	frmGameAbout := nil;
end;

procedure TfrmChocolateBox.imgEngineerClick(Sender: TObject);
var
	settingsBackup: GAME_SETTINGS;
begin
	// Display advanced settings (where the chocolate box is wrapped up and exported)
	m_bAllowDraw := False;
	settingsBackup := ChocolateBox.GameSettings;
	frmSettingsAdvanced := TfrmSettingsAdvanced.Create(Self);
	frmSettingsAdvanced.settings := ChocolateBox.GameSettings;
	// No need to registry a callback for media settings
	if (frmSettingsAdvanced.ShowModal() = mrOk) then
		begin
		// Settings have been changed, copy them back
		ChocolateBox.GameSettings := frmSettingsAdvanced.settings;
		end
	else
		begin
		// User cancelled, so use the backup settings
		ChocolateBox.GameSettings := settingsBackup;
		end;

	// Clean up
	frmSettingsAdvanced.Free();
	frmSettingsAdvanced := nil;
	m_bAllowDraw := False;
end;

procedure TfrmChocolateBox.imgChocolateBoxClick(Sender: TObject);
var
	nRow, nColumn, nBox: Integer;
begin
	// One of the chocolates has been clicked! Make sure this is a new selection...
	if (TRectangleImage(Sender).Tag = m_cache.nLastSelectedTag) then
		Exit;

	// Clear the selection of the previously selected box
	if (m_cache.nLastSelectedTag > -1) then
		begin
		for nBox:=0 to (m_cache.aBoxes.Count-1) do
			begin
			if (TRectangleImage(m_cache.aBoxes[nBox]).Tag = m_cache.nLastSelectedTag) then
				TRectangleImage(m_cache.aBoxes[nBox]).ShowSelected := False;
			end;
		end;

	// The tag encodes the row and column
	nColumn := (TRectangleImage(Sender).Tag mod ChocolateBox.GameSettings.nColumns);
	if (nColumn = 0) then
		nColumn := ChocolateBox.GameSettings.nColumns;

	nRow := (((TRectangleImage(Sender).Tag - nColumn) div ChocolateBox.GameSettings.nColumns) + 1);

	// Save this index
	m_cache.nLastSelectedTag := TRectangleImage(Sender).Tag;
	m_cache.nLastSelectedRow := nRow;
	m_cache.nLastSelectedColumn := nColumn;

	// Show the selection
	TRectangleImage(Sender).ShowSelected := True;

	// Debug: Pop up a message
	MessageDlg(
		Format('Tag: %d, row: %d, col: %d', [TRectangleImage(Sender).Tag, nRow, nColumn]),
		mtWarning, [mbOK], 0);
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

	m_cache.nLastGridRows := -1;
	m_cache.nLastGridColumns := -1;
	//m_cache.eLastBackground := eBackgroundNone;
	m_cache.tLastBackgroundColour := -1;

	m_cache.nLastSelectedTag := -1;
	m_cache.nLastSelectedRow := -1;
	m_cache.nLastSelectedColumn := -1;

	// Load main game settings
	ChocolateBox := TGameMachine.Create();
	ChocolateBox.InitSystem();
	UpdateGameBackground(ChocolateBox.GameSettings);
	UpdateGameBoard(ChocolateBox.GameSettings);
end;

procedure TfrmChocolateBox.UpdateGameBackground(settings: GAME_SETTINGS);
var
	bmp : TBitmap;
begin
	// Change in the background?
	//if (	(m_cache.eLastBackground = settings.eBackground) and
	//		(m_cache.tLastBackgroundColour = settings.tBackgroundColour)) then
	//	Exit;

	// Image or solid colour?
	if (settings.szBackground = 'SolidColour') then
		begin
		imgBackground.Picture := nil;
		bmp := TBitmap.Create;
			try
				bmp.PixelFormat := pf24bit;
				bmp.Width := imgBackground.Width;
				bmp.Height := imgBackground.Height;
				bmp.Canvas.Brush.Color := TColor(settings.tBackgroundColour);
				bmp.Canvas.FillRect(Rect(0, 0, Width, Height));
				imgBackground.Picture.Bitmap := bmp;
			finally
				bmp.Free();
			end;
		end
	else
		begin
		imgBackground.Picture.LoadFromFile(ChocolateBox.GameCache.szAppPath + settings.szBackground);
		end;
	//imgBackground.Picture.LoadFromFile(ChocolateBox.GameCache.szAppPath + 'ChocolateBox-3.bmp');

	// Background has been changed
	//m_cache.eLastBackground := settings.eBackground;
	//m_cache.tLastBackgroundColour := settings.tBackgroundColour;
end;

procedure TfrmChocolateBox.UpdateGameBoard(settings: GAME_SETTINGS);
const
	BOXES_TOP: Integer		= 120;
var
	nIconSetSize, nAvailableSpace, nGapHorizontal, nGapVertical: Integer;
	nBox, nRow, nCol: Integer;
	strImagePath: String;
	astrImages: TStringList;
	imgBox: TImage;
begin
	// Only create the boxes if there is a change in settings
	if (	(m_cache.nLastGridRows = settings.nRows) and
			(m_cache.nLastGridColumns = settings.nColumns) and
			(m_cache.eLastIconSet = settings.eIconSet)) then
		Exit;

	// Clear the current boxes
	if (	(m_cache.bInitialisedBoxes) and
			(m_cache.aBoxes <> nil)) then
		begin
		for nBox:=0 to (m_cache.aBoxes.Count-1) do
			begin
			TRectangleImage(m_cache.aBoxes[nBox]).Visible := False;
			Self.RemoveControl(TRectangleImage(m_cache.aBoxes[nBox]));
			end;

		m_cache.aBoxes.Free();
		m_cache.aBoxes := nil;
		m_cache.bInitialisedBoxes := False;
		end;

	// Calculate where the new boxes should go to spread them nicely over the available space. Use
	// the rectangle (0,150 to ScreenWidth,ScreenHeight).
	nIconSetSize := ChocolateBox.GetIconSetSize(settings.eIconSet);
	if (settings.nColumns > 1) then
		begin
		// Put icons slightly closer to the left/right edges
		nAvailableSpace := (
			ChocolateBox.GameCache.nScreenWidth -
			(nIconSetSize * settings.nColumns));
		nGapHorizontal := (nAvailableSpace div (settings.nColumns * 2));
		end
	else
		nGapHorizontal := 0;

	if (settings.nRows > 1) then
		begin
		nAvailableSpace := (
			ChocolateBox.GameCache.nScreenHeight -
			BOXES_TOP -
			(nIconSetSize * settings.nRows));
		nGapVertical := (nAvailableSpace div (settings.nRows + 1));
		end
	else
		nGapVertical := 0;

	// Get a listing of available image files
	astrImages := TStringList.Create();
	case settings.eIconSet of
		eIconSetStd_64x64:
			strImagePath := (ChocolateBox.GameCache.szAppPath + 'Icons\Std-64x64');

		eIconSetStd_128x128:
			strImagePath := (ChocolateBox.GameCache.szAppPath + 'Icons\Std-128x128');

		eIconSetFruitSalad_64x64:
			strImagePath := (ChocolateBox.GameCache.szAppPath + 'Icons\FruitSalad-64x64');

		eIconSetFruitSalad_128x128:
			strImagePath := (ChocolateBox.GameCache.szAppPath + 'Icons\FruitSalad-128x128');

		eIconSetFuturama_128x128:
			strImagePath := (ChocolateBox.GameCache.szAppPath + 'Icons\Futurama-128x128');
	end;

	GetFolderListing(strImagePath, '*.ico', astrImages);

	// Create a new grid of boxes
	m_cache.aBoxes := TObjectList.Create();
	for nRow:=1 to settings.nRows do
		begin
		for nCol:=1 to settings.nColumns do
			begin
			imgBox := TRectangleImage.Create(Self);
			imgBox.Visible := False;

			// Set the properties for each label
			with imgBox do
				begin
				// Main properties
				Tag := (nCol + (nRow - 1)*settings.nColumns);
				Name := Format('imgBox%d', [Tag]);
				Parent := Self;
				Width := nIconSetSize;
				Height := nIconSetSize;

				// Load image
				if (astrImages.Count > 0) then
					Picture.LoadFromFile(astrImages[Random(astrImages.Count)])
				else
					Picture := imgSettings.Picture;

				// Set a click event handler
				// Note: Prevent the TImage control from generating OnDblClick events
				ControlStyle := (ControlStyle - [csDoubleClicks]);
				OnClick := imgChocolateBoxClick;

				// Image position
				Left := (nGapHorizontal + ((nIconSetSize + (nGapHorizontal * 2))*(nCol - 1)));
				Top := (BOXES_TOP + nGapVertical + (nIconSetSize + nGapVertical)*(nRow - 1));

				// Budge the positions slightly (so they don't look too much like a fixed grid!)
				Left := (Left + Random(21) - 10);
				Top := (Top + Random(11) - 5);
				end;

			// Add box to list
			m_cache.aBoxes.Add(imgBox);
			end;
		end;

	// In debug mode, show "not selected" boxes, then make all the boxes visible
	for nBox:=0 to (m_cache.aBoxes.Count-1) do
		begin
		{$IFDEF DBG} TRectangleImage(m_cache.aBoxes[nBox]).ShowNotSelected := True; {$ENDIF}
		TRectangleImage(m_cache.aBoxes[nBox]).Visible := True;
		end;

	// Clean up
	astrImages.Free();

	// Boxes are initialised!
	m_cache.bInitialisedBoxes := True;
	m_cache.nLastGridRows := settings.nRows;
	m_cache.nLastGridColumns := settings.nColumns;
	m_cache.eLastIconSet := settings.eIconSet
end;

// Feedback for settings
procedure TfrmChocolateBox.SettingsCallback(pSettings: Pointer);
begin
	// Callback for when settings are changed
	UpdateGameBackground(GAME_SETTINGS(pSettings^));
	UpdateGameBoard(GAME_SETTINGS(pSettings^));
end;
// Private functions: Start

end.
