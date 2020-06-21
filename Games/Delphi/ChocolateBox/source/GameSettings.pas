unit GameSettings;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Buttons, Classes, ComCtrls, Controls, ExtCtrls, FileCtrl, Forms, Graphics,
  StdCtrls, StrUtils, SysUtils,
  CoreTypes, GameMachine, ChocolateBoxMain, SystemUtils;

const
	// Number of images used in the sample game (which is always a 2x2 grid)
	SAMPLE_GAME_SIZE = 4;

type
  // Sample game setings (always just show a 2x2 grid)
  SAMPLE_GAME_STATE = record
	arctImages: array[1..(SAMPLE_GAME_SIZE div 2), 1..(SAMPLE_GAME_SIZE div 2)] of TRect;
  end;

  TfrmGameSettings = class(TForm)
	btnSetDefaults: TBitBtn;
	btnDisclaimer: TBitBtn;
	btnCancel: TBitBtn;
	btnOk: TBitBtn;
	SettingsTimer: TTimer;

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
	procedure cbGridSizeChange(Sender: TObject);

	procedure OnSettingsTimerTick(Sender: TObject);

  private
	{ Private declarations }
	// Status flags
	m_bSettingUp, m_bExiting, m_bGameRunning: Boolean;

	procedure RefreshSettings();
	procedure EnableDisableControls();
	procedure ResetSettings();

  public
	{ Public declarations }
	// Local copy of system settings
	settings: GAME_SETTINGS;

	procedure SetGameRunning(bRunning: Boolean);
  end;

var
  frmGameSettings: TfrmGameSettings;

implementation

uses
  Dialogs, FormUtils, GameAbout, GameTypes;

{$R *.dfm}

// Private functions: Start
procedure TfrmGameSettings.RefreshSettings();
begin
	// Set all controls to the correct system settings
	m_bSettingUp := True;

	// Game, rows and columns
	cbRows.ItemIndex := (settings.nRows - 1);
	cbColumns.ItemIndex := (settings.nColumns - 1);

	// Enable/disable related controls
	EnableDisableControls();

	// No longer setting up
	m_bSettingUp := False;
end;

procedure TfrmGameSettings.EnableDisableControls();
begin
	// Enable conrols based on other settings or whether we are currently playing a game

	// Some settings must not be altered while the game is in progress
	if (m_bGameRunning) then
		begin
		lblRows.Enabled := False;
		cbRows.Enabled := False;
		lblColumns.Enabled := False;
		cbColumns.Enabled := False;

		btnSetDefaults.Enabled := False;
		end;
end;

procedure TfrmGameSettings.ResetSettings();
begin
	// Set everything to the default

	// Grid size
	settings.nRows := GRID_SIZE_DEFAULT;
	settings.nColumns := GRID_SIZE_DEFAULT;

	// Refresh settings
	RefreshSettings();
end;
// Private functions: End

// Public functions: Start
procedure TfrmGameSettings.SetGameRunning(bRunning: Boolean);
begin
	// When the game is running, certain settings should not be changed
	m_bGameRunning := bRunning;
end;
// Public functions: End

procedure TfrmGameSettings.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_bSettingUp := False;
	m_bExiting := False;
	m_bGameRunning := False;
end;

procedure TfrmGameSettings.FormDestroy(Sender: TObject);
begin
	// Clean up...
end;

procedure TfrmGameSettings.FormShow(Sender: TObject);
var
	byTmp: BYTE;
begin
	// Exiting system?
	if (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// If the game is running, show this in the title of the form
	if (m_bGameRunning) then
		Caption := 'Chocolate Box Settings [Busy !]'
	else
		Caption := 'Chocolate Box Settings';

	// Game rows / columns
	cbRows.Items.Clear();
	cbColumns.Items.Clear();
	for byTmp:=1 to GRID_SIZE_MAX do
		begin
		cbRows.Items.AddObject(IntToStr(byTmp), TObject(byTmp));
		cbColumns.Items.AddObject(IntToStr(byTmp), TObject(byTmp));
		end;

	// Show the game settings
	RefreshSettings();

	// Set focus to the OK button
	btnOk.SetFocus();

	// Start the timer
	SettingsTimer.Enabled := True;
end;

procedure TfrmGameSettings.btnOkClick(Sender: TObject);
begin
	// Save settings
	m_bExiting := True;
	ModalResult := mrOk;
end;

procedure TfrmGameSettings.btnCancelClick(Sender: TObject);
begin
	// Cancel any changes ?
	m_bExiting := True;
end;

procedure TfrmGameSettings.btnDisclaimerClick(Sender: TObject);
begin
	// Show a Help > About box
	frmGameAbout := TfrmGameAbout.Create(Self);
	frmGameAbout.ShowModal();
	frmGameAbout.Free();
	frmGameAbout := nil;
end;

procedure TfrmGameSettings.btnSetDefaultsClick(Sender: TObject);
begin
	if (MessageDlg('This will reset all settings to default. Are you sure you wish to continue?',
			mtConfirmation, [mbYes, mbCancel], 0) = mrYes) then
		ResetSettings();
end;

procedure TfrmGameSettings.cbDropDown(Sender: TObject);
begin
	// Change the colour of the dropdown
	TComboBox(Sender).Color := clSkyBlue;
end;

procedure TfrmGameSettings.cbCloseUp(Sender: TObject);
begin
	TComboBox(Sender).Color := clInfoBk;
end;

procedure TfrmGameSettings.cbGridSizeChange(Sender: TObject);
begin
	if (m_bSettingUp) then
		Exit;

	settings.nRows := (cbRows.ItemIndex + 1);
	settings.nColumns := (cbColumns.ItemIndex + 1);
end;

procedure TfrmGameSettings.OnSettingsTimerTick(Sender: TObject);
begin
	// Disable the timer
	SettingsTimer.Enabled := False;
	if (m_bExiting) or (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// Change the background colour of controls if they are disabled
	SetSubBackColour(gbGame);

	// Re-enable the timer
	SettingsTimer.Enabled := True;
end;

end.
