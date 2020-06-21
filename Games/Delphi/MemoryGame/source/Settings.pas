unit Settings;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Buttons, Classes, ComCtrls, Controls, ExtCtrls, FileCtrl, Forms, Graphics,
  StdCtrls, StrUtils, SysUtils,
  CoreTypes, Disclaimer, Imaging, JPGTools, Machine, Main, MemoryGameTypes, SystemUtils;

const
	// Number of images used in the sample game (which is always a 2x2 grid)
	SAMPLE_GAME_SIZE = 4;

type
  // Sample game setings (always just show a 2x2 grid)
  SAMPLE_GAME_STATE = record
	arctImages: array[1..(SAMPLE_GAME_SIZE div 2), 1..(SAMPLE_GAME_SIZE div 2)] of TRect;
  end;

  TfrmSettings = class(TForm)
	btnSetDefaults: TBitBtn;
	btnDisclaimer: TBitBtn;
	btnCancel: TBitBtn;
	btnOk: TBitBtn;
	SettingsTimer: TTimer;

	gbImages: TGroupBox;
	lblImageSource: TLabel;
	cbImageSource: TComboBox;
	lblFolder: TLabel;
	ebFolder: TEdit;
	btnBrowseForFolder: TButton;
	lblFolderDetails: TLabel;

	gbGraphics: TGroupBox;
	gbGrid: TGroupBox;
	lblGridColour: TLabel;
	panelGridColour: TPanel;
	lblGridWidth: TLabel;
	cbGridWidth: TComboBox;

	gbSampleGame: TGroupBox;
	SampleGameBox: TPaintBox;

	gbGame: TGroupBox;
	lblRows: TLabel;
	cbRows: TComboBox;
	lblColumns: TLabel;
	cbColumns: TComboBox;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnOkClick(Sender: TObject);
	procedure btnCancelClick(Sender: TObject);
	procedure btnSetDefaultsClick(Sender: TObject);
	procedure btnDisclaimerClick(Sender: TObject);

	procedure cbDropDown(Sender: TObject);
	procedure cbCloseUp(Sender: TObject);

	procedure cbImageSourceChange(Sender: TObject);
	procedure ebFolderClick(Sender: TObject);
	procedure btnBrowseForFolderClick(Sender: TObject);

	procedure panelGridColourClick(Sender: TObject);
	procedure cbGridWidthChange(Sender: TObject);

	procedure cbRowsChange(Sender: TObject);
	procedure cbColumnsChange(Sender: TObject);

	procedure SampleGameBoxPaint(Sender: TObject);

	procedure OnSettingsTimerTick(Sender: TObject);

  private
	{ Private declarations }
	// Status flags
	m_bSettingUp, m_bExiting, m_bGameRunning: Boolean;

	// Folder listing
	m_nJPGs, m_nBMPs: Integer;
	m_bInitalImageCountCheck: Boolean;

	// Sample game
	m_gameSample: SAMPLE_GAME_STATE;
	m_bAllowDraw: Boolean;
	m_pImgWholeGame, m_pImgBlank: PTIMAGECELL;
	m_apImages: array[1..4] of PTIMAGECELL;

	// GDI objects
	m_hFont: TFont;
	m_hBorderPen: TPen;

	procedure WMNCHitTest(var Msg: TMessage); message WM_NCHITTEST;

	procedure RefreshSettings();
	procedure EnableDisableControls();
	procedure ResetSettings();
	function ValidateGridSize() : Boolean;

	procedure UpdateFolderListing();
	function ValidateDrive(strFolder : String) : Boolean;
	procedure CheckImageCount();

	procedure RefreshSampleImages();
	procedure DrawGrid();

  public
	{ Public declarations }
	// Local copy of system settings
	settings: GAME_SETTINGS;

	procedure SetGameRunning(bRunning: Boolean);
  end;

var
  frmSettings: TfrmSettings;

implementation

uses
  Dialogs, FormUtils;

{$R *.dfm}

// Private functions: Start
procedure TfrmSettings.WMNCHitTest(var Msg: TMessage);
begin
	// Prevent Settings from being moved around which causes refresh problems
	DefaultHandler(Msg);
	if (Msg.Result = Windows.HTCAPTION) or (Msg.Result = Windows.HTNOWHERE) then
		Msg.Result := Windows.HTCLIENT;
end;

procedure TfrmSettings.RefreshSettings();
begin
	// Set all controls to the correct system settings
	m_bSettingUp := True;

	// Image source (or type)
	cbImageSource.ItemIndex := BYTE(settings.eSource);

	// Source folder (for the "Folder" option)
	ebFolder.Text := settings.strSourceFolder;

	// Graphics
	panelGridColour.Color := settings.clGridColour;
	cbGridWidth.ItemIndex := ((settings.nGridWidth div 2) - 1);

	// Game, rows and columns
	cbRows.ItemIndex := (settings.nRows - 2);
	cbColumns.ItemIndex := (settings.nColumns - 2);

	// Enable/disable related controls
	EnableDisableControls();

	// If the game is running, show this in the title of the form
	if (m_bGameRunning) then
		Caption := 'Memory Game Settings [Game is running !]'
	else
		Caption := 'Memory Game Settings';

	// No longer setting up
	m_bSettingUp := False;
end;

procedure TfrmSettings.EnableDisableControls();
begin
	// Enable controls based on other settings or whether we are currently playing a game

	// Disable "Folder" control if "Image source" is not set to "Folder"
	lblFolder.Enabled := (settings.eSource = eFolder);

	// Some settings must not be altered while the game is in progress
	if (m_bGameRunning) then
		begin
		lblImageSource.Enabled := False;
		lblFolder.Enabled := False;

		lblGridWidth.Enabled := False;
		cbGridWidth.Enabled := False;

		lblRows.Enabled := False;
		cbRows.Enabled := False;
		lblColumns.Enabled := False;
		cbColumns.Enabled := False;

		btnSetDefaults.Enabled := False;
		end;

	// Set "child" controls...
	cbImageSource.Enabled := (lblImageSource.Enabled);

	ebFolder.Enabled := (lblFolder.Enabled);
	btnBrowseForFolder.Enabled := (lblFolder.Enabled);
	UpdateFolderListing();
end;

procedure TfrmSettings.ResetSettings();
begin
	// Set everything to the default

	// Image Location
	settings.eSource := eInternal;
	settings.strSourceFolder :=
		(ExtractFilePath(Application.ExeName) + 'Internal sample pics\Food Images');

	// Graphics
	settings.clGridColour := clRed;

	// Grid size
	settings.nRows := 4;
	settings.nColumns := 4;

	// Refresh settings and images
	RefreshSettings();
	RefreshSampleImages();
end;

function TfrmSettings.ValidateGridSize() : Boolean;
var
	nSuggestedRows, nSuggestedColumns: Integer;
	strMsg: String;
begin
	// We cannot allow a grid that has both an odd number of rows and an odd number of columns.
	// Warn the user if this is the case.
	Result := True;
	if (Odd(settings.nRows)) and (Odd(settings.nColumns)) then
		begin
		if (settings.nRows < settings.nColumns) then
			begin
			nSuggestedRows := (settings.nRows + 1);
			nSuggestedColumns := settings.nColumns;
			end
		else
			begin
			nSuggestedRows := settings.nRows;
			nSuggestedColumns := (settings.nColumns + 1);
			end;

		strMsg := Format('Odd-shaped grids are not allowed! Suggest %d rows and %d columns. Is that OK ?', [
			nSuggestedRows, nSuggestedColumns]);
		if (MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
			begin
			settings.nRows := nSuggestedRows;
			settings.nColumns := nSuggestedColumns;
			end
		else
			Result := False;
		end;
end;

procedure TfrmSettings.UpdateFolderListing();
var
	strFolder: String;
	astrImages: TStringList;
begin
	// Show the number of JPG and BMP files in the selected folder. Currently only JPG and BMP are
	// supported!
	m_nJPGs := 0;
	m_nBMPs := 0;

	// Internal pictures or a user-selected folder
	strFolder := MemoryGame.GetSourceFolder(settings);

	// Valid drive?
	if (ValidateDrive(strFolder)) then
		begin
		// JPGs
		astrImages := TStringList.Create();
		astrImages.Clear();
		if (DirectoryExists(strFolder)) then
			begin
			GetFolderListing(strFolder, '*.jpg', astrImages);
			m_nJPGs := astrImages.Count;
			end;

		// BMPs
		astrImages.Clear();
		m_nBMPs := 0;
		if (DirectoryExists(strFolder)) then
			begin
			GetFolderListing(strFolder, '*.bmp', astrImages);
			m_nBMPs := astrImages.Count;
			end;

		// Clean up
		astrImages.Free();
		end;

	// Show the results
	lblFolderDetails.Caption := Format('(Images: %d JPGs and %d BMPs)', [m_nJPGs, m_nBMPs]);
end;

function TfrmSettings.ValidateDrive(strFolder : String) : Boolean;
begin
	Result := False;
	if (Length(strFolder) > 0) then
		Result := CheckDriveIsValid(strFolder[1]);
end;

procedure TfrmSettings.CheckImageCount();
var
	nImagesRequired, nImagesToDisplay: Integer;
	strWarning: String;
begin
	// Check that we have enough images to cover the grid size
	if (settings.eSource = eInternal) or
			(settings.eSource = eFolder) then
		begin
		nImagesRequired :=
			((settings.nRows * settings.nColumns) div 2);
		nImagesToDisplay := (m_nJPGs + m_nBMPs);
		if (nImagesToDisplay < nImagesRequired) then
			begin
			strWarning :=
				Format('Not enough images! %d required for a %dx%d grid. Add more images or reduce the grid size.', [
					nImagesRequired, settings.nRows, settings.nColumns]);
			MessageDlg(strWarning, mtWarning, [mbOK], 0);
			end;
		end;
end;

procedure TfrmSettings.RefreshSampleImages();
var
	nImgWidth, nImgHeight: Integer;
	nRow, nCol, nImage: Integer;
	astrImages: TStringList;
	nImgCount: Integer;
	anImgToLoad: array[1..SAMPLE_GAME_SIZE] of Integer;
	pTmpImg: PTIMAGECELL;
	rct: TRect;
	strFolder, strMsg: String;
	wHours, wMins, wSecs, wMilliSecs: WORD;
begin
	// Create sample images from the user selection

	// How big can each image be? Leave a few pixels either side of each image for a border.
	nImgWidth := ((SampleGameBox.Width - 2 * settings.nGridWidth) div 2);
	nImgHeight := ((SampleGameBox.Height - 2 * settings.nGridWidth) div 2);

	// Set up the bounding rectangles for all images
	for nRow:=1 to (SAMPLE_GAME_SIZE div 2) do
		begin
		for nCol:=1 to (SAMPLE_GAME_SIZE div 2) do
			begin
			m_gameSample.arctImages[nRow, nCol].Left :=
				((nCol * settings.nGridWidth) + (nCol-1) * nImgWidth);
			m_gameSample.arctImages[nRow, nCol].Right :=
				(m_gameSample.arctImages[nRow, nCol].Left + nImgWidth);

			m_gameSample.arctImages[nRow, nCol].Top :=
				((nRow * settings.nGridWidth) + (nRow-1) * nImgHeight);
			m_gameSample.arctImages[nRow, nCol].Bottom :=
				(m_gameSample.arctImages[nRow, nCol].Left + nImgHeight);
			end;
		end;

	// Prepare the blank image
	DeleteImageCell(m_pImgBlank);
	m_pImgBlank := CreateImageCell(1,1,0,0);
	PrepareImageCell(m_pImgBlank, nImgWidth, nImgHeight);
	Expand24Coloured(m_pImgBlank, COLOUR_BACKGROUND);

	// Create sample game images
	for nImage:=1 to SAMPLE_GAME_SIZE do
		begin
		PrepareImageCell(m_apImages[nImage], nImgWidth, nImgHeight);
		ClearImageCell(m_apImages[nImage], COLOUR_WHITE);
		Expand24(m_apImages[nImage]);
		end;

	// Internal pitures or a user-slected folder (the default folder shows some pictures of food!)
	strFolder := MemoryGame.GetSourceFolder(settings);

	astrImages := TStringList.Create();
	astrImages.Clear();
	GetFolderMultipleListing(strFolder, '*.bmp;*.jpg', astrImages);
	nImgCount := astrImages.Count;
	if (nImgCount > (SAMPLE_GAME_SIZE - 1)) then
		begin
		// Seed the random number generator
		DecodeTime(Now(), wHours, wMins, wSecs, wMilliSecs);
		RandSeed := wMilliSecs;

		// Set two random image to be used
		anImgToLoad[1] := Random(nImgCount);
		anImgToLoad[2] := anImgToLoad[1];
		while (anImgToLoad[2] = anImgToLoad[1]) do
			anImgToLoad[2] := Random(nImgCount);

		anImgToLoad[3] := anImgToLoad[2];
		while (		(anImgToLoad[3] = anImgToLoad[1]) or
					(anImgToLoad[3] = anImgToLoad[2])) do
			anImgToLoad[3] := Random(nImgCount);

		anImgToLoad[4] := anImgToLoad[3];
		while (		(anImgToLoad[4] = anImgToLoad[1]) or
					(anImgToLoad[4] = anImgToLoad[2]) or
					(anImgToLoad[4] = anImgToLoad[3])) do
			anImgToLoad[4] := Random(nImgCount);

		// Load the images
		pTmpImg := CreateImageCell(1,1,0,0);
		for nImage:=1 to SAMPLE_GAME_SIZE do
			begin
			// Load the image...
			strMsg := astrImages[anImgToLoad[nImage]];
			if (IsFileExtType(strMsg, '.bmp')) then
				LoadBMP(pTmpImg, 0, PAnsiChar(strMsg))
			else if (IsFileExtType(strMsg, '.jpg')) then
				LoadJPG(pTmpImg, strMsg, nil, 0);

			// ...correct its aspect ratio...
			ImageCellCorrectAspect24(@pTmpImg, COLOUR_BACKGROUND,
				nImgWidth, nImgHeight);

			// ...and stretch it onto our sample image
			rct.Left := 0;
			rct.Right := (m_apImages[nImage].nWidth - 1);
			rct.Top := 0;
			rct.Bottom := (m_apImages[nImage].nHeight - 1);
			StretchImageCellBitmap(pTmpImg, m_apImages[nImage].handleDC, @rct);
			end;

		DeleteImageCell(pTmpImg);
		end;

	// Clean up
	astrImages.Free();
end;

procedure TfrmSettings.DrawGrid();
var
	byRow, byCol: BYTE;
	nTmp: Integer;
begin
	// Draw the borders around each object
	m_hBorderPen.Color := settings.clGridColour;
	m_hBorderPen.Width := settings.nGridWidth;
	SelectObject(m_pImgWholeGame.handleDC, m_hBorderPen.Handle);

	// Rows
	for byRow:=1 to (SAMPLE_GAME_SIZE div 2) do
		begin
		nTmp := (m_gameSample.arctImages[byRow, 1].Top - (settings.nGridWidth div 2));
		MoveToEx(m_pImgWholeGame.handleDC, 0, nTmp, nil);
		LineTo(m_pImgWholeGame.handleDC, m_pImgWholeGame.nWidth, nTmp);
		end;

	nTmp := (m_pImgWholeGame.nHeight - (settings.nGridWidth div 2));
	MoveToEx(m_pImgWholeGame.handleDC, 0, nTmp, nil);
	LineTo(m_pImgWholeGame.handleDC, m_pImgWholeGame.nWidth, nTmp);

	// Columns
	for byCol:=1 to (SAMPLE_GAME_SIZE div 2) do
		begin
		nTmp := (m_gameSample.arctImages[1, byCol].Left - (settings.nGridWidth div 2));
		MoveToEx(m_pImgWholeGame.handleDC, nTmp, 0, nil);
		LineTo(m_pImgWholeGame.handleDC, nTmp, m_pImgWholeGame.nHeight);
		end;

	nTmp := (m_pImgWholeGame.nWidth - (settings.nGridWidth div 2));
	MoveToEx(m_pImgWholeGame.handleDC, nTmp, 0, nil);
	LineTo(m_pImgWholeGame.handleDC, nTmp, m_pImgWholeGame.nHeight);
end;
// Private functions: End

// Public functions: Start
procedure TfrmSettings.SetGameRunning(bRunning: Boolean);
begin
	// When the game is running, certain settings should not be changed
	m_bGameRunning := bRunning;
end;
// Public functions: End

procedure TfrmSettings.FormCreate(Sender: TObject);
var
	nImage: Integer;
begin
	// Initialise form

	// Status flags
	m_bSettingUp := False;
	m_bExiting := False;
	m_bGameRunning := False;

	// Folder listing
	m_nJPGs := 0;
	m_nBMPs := 0;
	m_bInitalImageCountCheck := False;

	// Sample game
	m_bAllowDraw := True;
	m_pImgWholeGame := CreateImageCell(1,1,0,0);
	PrepareImageCell(m_pImgWholeGame, SampleGameBox.Width, SampleGameBox.Height);
	Expand24Coloured(m_pImgWholeGame, clWhite);
	m_pImgBlank := CreateImageCell(1,1,0,0);
	for nImage:=1 to SAMPLE_GAME_SIZE do
		m_apImages[nImage] := CreateImageCell(1,1,0,0);

	// GDI objects
	m_hFont := TFont.Create();
	m_hFont.Size := 32;
	m_hBorderPen := TPen.Create();
	m_hBorderPen.Width := GRID_WIDTH_DEFAULT;
	m_hBorderPen.Color := clRed;
end;

procedure TfrmSettings.FormDestroy(Sender: TObject);
var
	nImage: Integer;
begin
	// Clean up...
	DeleteImageCell(m_pImgWholeGame);
	DeleteImageCell(m_pImgBlank);
	for nImage:=1 to SAMPLE_GAME_SIZE do
		DeleteImageCell(m_apImages[nImage]);

	m_hFont.Destroy();
	m_hBorderPen.Destroy();
end;

procedure TfrmSettings.FormShow(Sender: TObject);
var
	byTmp: BYTE;
begin
	// Exiting system?
	if (MemoryGame.bySystemExitRequest > 0) then
		Exit;

	// Set up the various combo boxes on the form
	// Images location
	cbImageSource.Items.Clear();
	cbImageSource.Items.AddObject('Internal (Default)', TObject(eInternal));
	cbImageSource.Items.AddObject('Numbers', TObject(eNumbers));
	cbImageSource.Items.AddObject('Letters', TObject(eLetters));
	cbImageSource.Items.AddObject('Folder', TObject(eFolder));

	// Grid width
	cbGridWidth.Items.Clear();
	for byTmp:=1 to (GRID_WIDTH_MAX div 2) do
		cbGridWidth.Items.AddObject(IntToStr(2*byTmp), TObject(2*byTmp));

	// Game rows / columns
	cbRows.Items.Clear();
	cbColumns.Items.Clear();
	for byTmp:=2 to GRID_WIDTH_MAX do
		begin
		cbRows.Items.AddObject(IntToStr(byTmp), TObject(byTmp));
		cbColumns.Items.AddObject(IntToStr(byTmp), TObject(byTmp));
		end;

	// If the game is running, show this in the title of the form
	if (m_bGameRunning) then
		Caption := 'Memory Game Settings [Game is running!]'
	else
		Caption := 'Memory Game Settings';

	// Update folder list (only relevant for "Internal" and "Folder")
	UpdateFolderListing();

	// Show the game settings
	RefreshSettings();

	// Set the initial bounding rectangles for images displayed on the sample game
	RefreshSampleImages();

	// Set focus to the OK button
	btnOk.SetFocus();

	// Start the timer
	SettingsTimer.Enabled := True;
end;

procedure TfrmSettings.btnOkClick(Sender: TObject);
begin
	// Save settings
	if (ValidateGridSize()) then
		begin
		m_bExiting := True;
		ModalResult := mrOk;
		end;
end;

procedure TfrmSettings.btnCancelClick(Sender: TObject);
begin
	// Cancel any changes ?
	m_bExiting := True;
end;

procedure TfrmSettings.btnDisclaimerClick(Sender: TObject);
begin
	// Show a disclaimer form showing details of who wrote this program and some notes on how to
	// generate new images
	frmDisclaimer := TfrmDisclaimer.Create(Self);
	frmDisclaimer.ShowModal();
	frmDisclaimer.Free();
	frmDisclaimer := nil;
end;

procedure TfrmSettings.btnSetDefaultsClick(Sender: TObject);
begin
	if (MessageDlg('This will reset all settings to default. Are you sure you wish to continue?',
			mtConfirmation, [mbYes, mbCancel], 0) = mrYes) then
		ResetSettings();
end;

procedure TfrmSettings.cbDropDown(Sender: TObject);
begin
	// Change the colour of the dropdown
	TComboBox(Sender).Color := clSkyBlue;
end;

procedure TfrmSettings.cbCloseUp(Sender: TObject);
begin
	TComboBox(Sender).Color := clInfoBk;
end;

procedure TfrmSettings.cbImageSourceChange(Sender: TObject);
var
	eOldSource: IMAGE_SOURCE;
begin
	if (m_bSettingUp) then
		Exit;

	// Get the new image source
	eOldSource := settings.eSource;
	settings.eSource := IMAGE_SOURCE(cbImageSource.Items.Objects[cbImageSource.ItemIndex]);
	EnableDisableControls();

	// Has it changed?
	if (eOldSource <> settings.eSource) then
		begin
		// Yes, so refresh the images
		m_bAllowDraw := False;
		RefreshSampleImages();
		m_bAllowDraw := True;

		// Update folder listing
		UpdateFolderListing();
		CheckImageCount();
		end;
end;

procedure TfrmSettings.ebFolderClick(Sender: TObject);
begin
	// The user should only enter values via the browse button
	MessageDlg('Please use the Browse button on the right !', mtWarning, [mbOK], 0);
end;

procedure TfrmSettings.btnBrowseForFolderClick(Sender: TObject);
var
	strChosenDirectory: String;
	options : TSelectDirOpts;
begin
	strChosenDirectory := 'C:\';
	if (SelectDirectory(strChosenDirectory, options, 0)) then
		begin
		settings.strSourceFolder := strChosenDirectory;
		ebFolder.Text := settings.strSourceFolder;
		UpdateFolderListing();
		CheckImageCount();
		end;
end;

procedure TfrmSettings.panelGridColourClick(Sender: TObject);
var
	colourDlg: TColorDialog;
begin
	if (not panelGridColour.Enabled) then
		Exit;

	// Allow the user to select and change the grid colour
	colourDlg := TColorDialog.Create(Self);
	colourDlg.Color := settings.clGridColour;
	if (colourDlg.Execute()) then
		begin
		settings.clGridColour := colourDlg.Color;
		panelGridColour.Color := settings.clGridColour;
		end;
end;

procedure TfrmSettings.cbGridWidthChange(Sender: TObject);
begin
	if (m_bSettingUp) then
		Exit;

	settings.nGridWidth := (2 * (cbGridWidth.ItemIndex + 1));
	RefreshSampleImages();
end;

procedure TfrmSettings.cbRowsChange(Sender: TObject);
begin
	if (m_bSettingUp) then
		Exit;

	settings.nRows := (cbRows.ItemIndex + 2);
	CheckImageCount();
end;

procedure TfrmSettings.cbColumnsChange(Sender: TObject);
begin
	if (m_bSettingUp) then
		Exit;

	settings.nColumns := (cbColumns.ItemIndex + 2);
	CheckImageCount();
end;

procedure TfrmSettings.SampleGameBoxPaint(Sender: TObject);
var
	byRow, byCol: BYTE;
	rct: TRect;
begin
	if (not m_bAllowDraw) then
		Exit;

	// Paint the sample game

	// Draw images to our "whole game" image
	for byRow:=1 to (SAMPLE_GAME_SIZE div 2) do
		begin
		for byCol:=1 to (SAMPLE_GAME_SIZE div 2) do
			begin
			rct := m_gameSample.arctImages[byRow, byCol];
			ExtractImageCellToPosition24(
				m_pImgWholeGame, m_apImages[2*(byRow-1) + byCol], rct.Left, rct.Top);
			end;
		end;

	// Draw grid
	DrawGrid();

	// Blt the image to the screen
	BitBlt(SampleGameBox.Canvas.Handle, 0, 0,
		SampleGameBox.Width, SampleGameBox.Height,
		m_pImgWholeGame.handleDC, 0, 0, SRCCOPY);
end;

procedure TfrmSettings.OnSettingsTimerTick(Sender: TObject);
begin
	// Disable the timer
	SettingsTimer.Enabled := False;
	if (m_bExiting) or (MemoryGame.bySystemExitRequest > 0) then
		Exit;

	// Do an initial check on the image count?
	if (not m_bInitalImageCountCheck) then
		begin
		CheckImageCount();
		m_bInitalImageCountCheck := True;
		end;

	// Change the background colour of controls if they are disabled
	SetSubBackColour(gbImages);
	SetSubBackColour(gbGraphics);
	SetSubBackColour(gbGame);

	// Redraw the grid
	SampleGameBoxPaint(Self);

	// Re-enable the timer
	SettingsTimer.Enabled := True;
end;

end.
