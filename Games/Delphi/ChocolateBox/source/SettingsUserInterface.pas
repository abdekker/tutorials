unit SettingsUserInterface;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Buttons, Classes, ComCtrls, Controls, Dialogs, ExtCtrls, FileCtrl, Forms, Graphics,
  StdCtrls, StrUtils, SysUtils,
  CoreFormClasses, CoreTypes, GameMachine, ChocolateBoxMain, SystemUtils;

type
  // Enumeration
  TMouseClickClientArea = (
	eMouseClickClientAreaUnknown = 0,
	eMouseClickClientArea,
	eMouseClickNonClientArea);

  TfrmSettingsUserInterface = class(TGeneralBaseForm)
	btnSetDefaults: TBitBtn;
	btnCancel: TBitBtn;
	btnOk: TBitBtn;

	SettingsTimer: TTimer;
	colours: TColorDialog;

	gbGrid: TGroupBox;
	lblRows: TLabel;
	lblColumns: TLabel;
	ebRows: TEdit;
	ebColumns: TEdit;

	gbGraphics: TGroupBox;
	lblBackground: TLabel;
	cbBackground: TComboBox;
	pnlBackgroundColour: TPanel;
	lblIconSet: TLabel;
	cbIconSet: TComboBox;
	lblIconSetSize: TLabel;
    CheckBox1: TCheckBox;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnOkClick(Sender: TObject);
	procedure btnCancelClick(Sender: TObject);

	procedure ebGridSizeChange(Sender: TObject);
	procedure cbBackgroundChange(Sender: TObject);
	procedure pnlBackgroundColourClick(Sender: TObject);
	procedure cbIconSetChange(Sender: TObject);

	procedure btnSetDefaultsClick(Sender: TObject);

	procedure OnSettingsTimerTick(Sender: TObject);

  private
	{ Private declarations }
	// Parent form
	m_pParent: TForm;

	// Status flags
	m_bSettingUp, m_bExiting: Boolean;

	// Callback
	m_callbackSettings: TProcedureCallbackPointer;

	// Mouse hook (to change the form transparency)
	m_eMouseClickClientArea: TMouseClickClientArea;
	m_bHookStarted: Boolean;
	procedure MouseClickEvent(bClientArea: Boolean);

	procedure InitialiseControls();
	procedure EnableDisableControls();
	procedure RefreshSettings();
	procedure SetBackgroundColourPanel();
	procedure SetIconSetSize();
	procedure ResetSettings();

  public
	{ Public declarations }
	// Local copy of system settings
	settings: GAME_SETTINGS;

	procedure RegisterSettingsCallback(callBack: TProcedureCallbackPointer);
  end;

var
  frmSettingsUserInterface: TfrmSettingsUserInterface;
  hMouseHook: THandle;

implementation

uses
  Messages, FormUtils, GameTypes;

{$R *.dfm}

// Hook functions: Start
function MouseHookProc(nCode: Integer; wPar: WParam; lPar: LParam) : Integer; stdcall;
begin
	// CallNextHookEX is not really needed since it is not really in a hook chain, but it's
	// standard for a Hook.
	Result := CallNextHookEx(hMouseHook, nCode, wPar, lPar);
	if (nCode < 0) then
		Exit;

	if (nCode = HC_ACTION) then
		begin
		// A mouse event! We are only interested in these two CLICK messages:
		// WM_NCLBUTTONUP ($00A0)	Mouse in the non-client area of the window (ie. outside)
		// WM_LBUTTONUP ($0202)		Mouse in the client area of the window (ie. inside)

		// Other mouse events of interest might include WM_NCMOUSEMOVE and WM_MOUSEMOVE.

		// If you want to get detailed information about the mouse message, declare a variable of
		// type PMOUSEHOOKSTRUCT and then call:
		//		pVariable := PMOUSEHOOKSTRUCT(lPar);
		// In there you will find the position of the mouse event and the hWnd of the main window
		// receiving the event.
		if (wPar = WM_NCLBUTTONUP) then
			frmSettingsUserInterface.MouseClickEvent(False)
		else if (wPar = WM_LBUTTONUP) then
			frmSettingsUserInterface.MouseClickEvent(True);
		end;
end;
// Hook functions: End

// Private functions: Start
procedure TfrmSettingsUserInterface.MouseClickEvent(bClientArea: Boolean);
var
	eLocalMouseClickClientArea: TMouseClickClientArea;
begin
	// Mouse click event on the form
	if (bClientArea) then
		eLocalMouseClickClientArea := eMouseClickClientArea
	else
		eLocalMouseClickClientArea := eMouseClickNonClientArea;

	// Change in the client area where the last click occurred ?
	if (m_eMouseClickClientArea <> eLocalMouseClickClientArea) then
		begin
		m_eMouseClickClientArea := eLocalMouseClickClientArea;
		if (bClientArea) then
			AlphaBlendValue := 255
		else
			AlphaBlendValue := 224;
		end;
end;

procedure TfrmSettingsUserInterface.InitialiseControls();
begin
	// Set up controls for user settings

	// Game rows / columns
	cbBackground.Items.Clear();
	cbBackground.Items.AddObject('Background 1', TObject(eBackgroundImg1));
	cbBackground.Items.AddObject('Background 2', TObject(eBackgroundImg2));
	cbBackground.Items.AddObject('Solid colour', TObject(eBackgroundSolidColour));

	// Graphics
	cbIconSet.Items.Clear();
	cbIconSet.Items.AddObject('Standard (small)', TObject(eIconSetStd_64x64));
	cbIconSet.Items.AddObject('Standard (large)', TObject(eIconSetStd_128x128));
	cbIconSet.Items.AddObject('Fruit Salad (small)', TObject(eIconSetFruitSalad_64x64));
	cbIconSet.Items.AddObject('Fruit Salad (large)', TObject(eIconSetFruitSalad_128x128));
	cbIconSet.Items.AddObject('Futurama (large)', TObject(eIconSetFuturama_128x128));
end;

procedure TfrmSettingsUserInterface.SetBackgroundColourPanel();
begin
	// If the background is set to a solid colour, the user can update this
	if (settings.eBackground = eBackgroundSolidColour) then
		pnlBackgroundColour.Color := TColor(settings.tBackgroundColour)
	else
		pnlBackgroundColour.Color := clBtnFace;
end;

procedure TfrmSettingsUserInterface.SetIconSetSize();
var
	nIconSetSize: Integer;
begin
	// Show the size of the icon size
	nIconSetSize := ChocolateBox.GetIconSetSize(settings.eIconSet);
	lblIconSetSize.Caption := Format('[ Size: %dx%d ]', [nIconSetSize, nIconSetSize]);
end;

procedure TfrmSettingsUserInterface.EnableDisableControls();
begin
	// Some settings should not be altered while the game is in progress
	{if (m_bGameRunning) then
		begin
		SetSubControlsEnabled(
			gbGrid,
			(CONTROL_TLABEL + CONTROL_TEDIT), False);
		SetSubControlsEnabled(
			gbGraphics,
			(CONTROL_TLABEL + CONTROL_TCOMBOBOX), False);

		btnSetDefaults.Enabled := False;
		end;}
end;

procedure TfrmSettingsUserInterface.RefreshSettings();
begin
	// Set all controls to the correct system settings
	m_bSettingUp := True;

	// Game, rows and columns
	ebRows.Text := IntToStr(settings.nRows);
	ebColumns.Text := IntToStr(settings.nColumns);

	// Graphics
	cbBackground.ItemIndex := cbBackground.Items.IndexOfObject(TObject(settings.eBackground));
	pnlBackgroundColour.Color := TColor(settings.tBackgroundColour);
	cbIconSet.ItemIndex := cbIconSet.Items.IndexOfObject(TObject(settings.eIconSet));
	SetIconSetSize();

	// Enable or disable related controls
	EnableDisableControls();

	// No longer setting up
	m_bSettingUp := False;
end;

procedure TfrmSettingsUserInterface.ResetSettings();
begin
	// Set everything to the default
	ChocolateBox.ResetSettings(settings);

	// Refresh settings
	RefreshSettings();
end;
// Private functions: End

// Public functions: Start
procedure TfrmSettingsUserInterface.RegisterSettingsCallback(callBack: TProcedureCallbackPointer);
begin
	// Callback used when the settings changes
	m_callbackSettings := callBack;
end;
// Public functions: End

procedure TfrmSettingsUserInterface.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_pParent := TForm(Sender);
	m_bSettingUp := False;
	m_bExiting := False;

	// Start a local Windows hook procedure for peeking mouse messages that have been removed from
	// the message queue for this thread
	m_eMouseClickClientArea := eMouseClickClientAreaUnknown;
	m_bHookStarted := False;
	hMouseHook := SetWindowsHookEx(WH_MOUSE, @MouseHookProc, 0, GetCurrentThreadID());
	if (hMouseHook > 0) then
		m_bHookStarted := True;
end;

procedure TfrmSettingsUserInterface.FormDestroy(Sender: TObject);
begin
	// Exiting form, so unhook the mouse message hook
	if (m_bHookStarted) then
		UnhookWindowsHookEx(hMouseHook);
end;

procedure TfrmSettingsUserInterface.FormShow(Sender: TObject);
begin
	// Exiting system?
	if (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// Set the form to the top right (so that the user can see the effect on the main grid)
	Self.Top := 0;
	Self.Left := (ChocolateBox.GameCache.nScreenWidth - Self.Width);

	// Initialise controls for game settings
	SetChildComboHandlers(gbGraphics);
	InitialiseControls();

	// Show the game settings
	RefreshSettings();
	SetBackgroundColourPanel();

	// Set focus to the OK button
	btnOk.SetFocus();

	// Start the timer
	SettingsTimer.Enabled := True;
end;

procedure TfrmSettingsUserInterface.btnOkClick(Sender: TObject);
var
	bEntryValid: Boolean;
begin
	// Save settings
	SettingsTimer.Enabled := False;

	// Grid size
	bEntryValid := TryStrToInt(ebRows.Text, settings.nRows);
	if (bEntryValid) then
		begin
		if (settings.nRows < 1) or (settings.nRows > GRID_SIZE_MAX) then
			bEntryValid := False;
		end;

	if (not bEntryValid) then
		begin
		MessageDlg(
			Format('Please enter a valid value. Range is 1 to %d', [GRID_SIZE_MAX]), mtWarning, [mbOK], 0);
		SelectAllText(ebRows);
		ModalResult := mrNone;
		Exit;
		end;

	bEntryValid := TryStrToInt(ebColumns.Text, settings.nColumns);
	if (bEntryValid) then
		begin
		if (settings.nColumns < 1) or (settings.nColumns > GRID_SIZE_MAX) then
			bEntryValid := False;
		end;

	if (not bEntryValid) then
		begin
		MessageDlg(
			Format('Please enter a valid value. Range is 1 to %d', [GRID_SIZE_MAX]), mtWarning, [mbOK], 0);
		SelectAllText(ebColumns);
		ModalResult := mrNone;
		Exit;
		end;

	// Graphics
	settings.eBackground := TGameBackground(cbBackground.Items.Objects[cbBackground.ItemIndex]);
	if (settings.eBackground = eBackgroundSolidColour) then
		settings.tBackgroundColour := Integer(pnlBackgroundColour.Color);

	settings.eIconSet := TGameIconSet(cbIconSet.Items.Objects[cbIconSet.ItemIndex]);

	// If we get here, then settings are valid!
	m_bExiting := True;
	ModalResult := mrOk;
end;

procedure TfrmSettingsUserInterface.btnCancelClick(Sender: TObject);
begin
	// Cancel any changes ?
	m_bExiting := True;
end;

procedure TfrmSettingsUserInterface.ebGridSizeChange(Sender: TObject);
var
	bRowsValid, bColumnsValid: Boolean;
begin
	// Check whether the gride size values are valid
	bRowsValid := TryStrToInt(ebRows.Text, settings.nRows);
	if (bRowsValid) then
		begin
		if (settings.nRows < 1) or (settings.nRows > GRID_SIZE_MAX) then
			bRowsValid := False;
		end;

	bColumnsValid := TryStrToInt(ebColumns.Text, settings.nColumns);
	if (bColumnsValid) then
		begin
		if (settings.nColumns < 1) or (settings.nColumns > GRID_SIZE_MAX) then
			bColumnsValid := False;
		end;

	// If the rows/columns are valid, update the main form
	if (not m_bSettingUp) then
		begin
		if (bRowsValid and bColumnsValid) then
			m_callbackSettings(@settings);
		end;
end;

procedure TfrmSettingsUserInterface.cbBackgroundChange(Sender: TObject);
begin
	// When the background changes, we may allow the user to set a solid background colour
	settings.eBackground := TGameBackground(cbBackground.Items.Objects[cbBackground.ItemIndex]);
	SetBackgroundColourPanel();
	if (not m_bSettingUp) then
		m_callbackSettings(@settings);
end;

procedure TfrmSettingsUserInterface.cbIconSetChange(Sender: TObject);
begin
	// Show the size of the icons in the icon set
	settings.eIconSet := TGameIconSet(cbIconSet.Items.Objects[cbIconSet.ItemIndex]);
	SetIconSetSize();
	if (not m_bSettingUp) then
		m_callbackSettings(@settings);
end;

procedure TfrmSettingsUserInterface.pnlBackgroundColourClick(Sender: TObject);
begin
	if (settings.eBackground = eBackgroundSolidColour) then
		begin
		colours.Color := pnlBackgroundColour.Color;
		if (colours.Execute()) then
			begin
			pnlBackgroundColour.Color := colours.Color;
			settings.tBackgroundColour := Integer(pnlBackgroundColour.Color);
			m_callbackSettings(@settings);
			end;
		end;
end;

procedure TfrmSettingsUserInterface.btnSetDefaultsClick(Sender: TObject);
begin
	if (MessageDlg('This will reset all settings to default. Are you sure you wish to continue?',
			mtConfirmation, [mbYes, mbCancel], 0) = mrYes) then
		begin
		ResetSettings();
		m_callbackSettings(@settings);
		end;
end;

procedure TfrmSettingsUserInterface.OnSettingsTimerTick(Sender: TObject);
begin
	// Disable the timer
	SettingsTimer.Enabled := False;
	if (m_bExiting) or (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// Change the background colour of controls if they are disabled
	SetSubBackColour(gbGrid);

	// Re-enable the timer
	SettingsTimer.Enabled := True;
end;

end.
