unit GameMediaSettings;
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

  TfrmGameMediaSettings = class(TGeneralBaseForm)
	btnCancel: TBitBtn;
	btnOk: TBitBtn;

	SettingsTimer: TTimer;
	gbMedia: TGroupBox;
	lblMediaFolder: TLabel;
	ebMediaFolder: TEdit;
    lblFolderInformation: TLabel;
    btnBrowse: TBitBtn;
    imgRefresh: TImage;
    imgWarning: TImage;
    imgMedia: TImage;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnOkClick(Sender: TObject);
	procedure btnCancelClick(Sender: TObject);
	procedure imgRefreshClick(Sender: TObject);

	procedure OnSettingsTimerTick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);

  private
	{ Private declarations }
	// Parent form
	m_pParent: TForm;

	// Status flags
	m_bSettingUp, m_bExiting, m_bGameRunning: Boolean;

	procedure EnableDisableControls();
	procedure RefreshSettings();
	procedure UpdateMediaDetails();

  public
	{ Public declarations }
	// Local copy of system settings
	settings: GAME_SETTINGS;

	procedure SetGameRunning(bRunning: Boolean);
  end;

var
  frmGameMediaSettings: TfrmGameMediaSettings;

implementation

uses
  Messages, FormUtils, GameAbout, GameTypes;

{$R *.dfm}

// Private functions: Start
procedure TfrmGameMediaSettings.EnableDisableControls();
begin
	// Enable controls based on other settings or whether we are currently playing a game

	// Some settings should not be altered while the game is in progress
	if (m_bGameRunning) then
		begin
		SetSubControlsEnabled(
			gbMedia,
			(CONTROL_TLABEL + CONTROL_TEDIT + CONTROL_TBUTTON), False);
		end;
end;

procedure TfrmGameMediaSettings.RefreshSettings();
begin
	// Set all controls to the correct system settings
	m_bSettingUp := True;

	// Media (where files are loaded from)
	ebMediaFolder.Text := settings.strMediaFolder;
	UpdateMediaDetails();

	// Enable or disable related controls
	EnableDisableControls();

	// No longer setting up
	m_bSettingUp := False;
end;

procedure TfrmGameMediaSettings.UpdateMediaDetails();
var
	strMediaPath: String;
	astrFiles: TStringList;
begin
	// Display some information about the folder the user has selected for media samples
	strMediaPath := ChocolateBox.GetMediaPath(ebMediaFolder.Text);
	astrFiles := TStringList.Create();
	GetFolderListing(strMediaPath, '*.*', astrFiles, True);

	// Any files found?
	if (astrFiles.Count > 0) then
		begin
		// Yes!
		imgWarning.Visible := False;
		imgMedia.Visible := True;
		lblFolderInformation.Font.Style := [];
		lblFolderInformation.Caption :=
			Format('%d file(s) found at that location', [astrFiles.Count]);
		end
	else
		begin
		// No, the user may have entered the name incorrectly or still needs to add files
		imgMedia.Visible := False;
		imgWarning.Visible := True;
		lblFolderInformation.Font.Style := [fsBold];
		lblFolderInformation.Caption := 'Folder empty or not found!';
		end;

	// Clean up
	astrFiles.Free();
end;
// Private functions: End

// Public functions: Start
procedure TfrmGameMediaSettings.SetGameRunning(bRunning: Boolean);
begin
	// When the game is running, certain settings should not be changed
	m_bGameRunning := bRunning;
end;
// Public functions: End

procedure TfrmGameMediaSettings.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_pParent := TForm(Sender);
	m_bSettingUp := False;
	m_bExiting := False;
	m_bGameRunning := False;

	imgWarning.Top := imgMedia.Top;
	imgWarning.Left := imgMedia.Left;
end;

procedure TfrmGameMediaSettings.FormDestroy(Sender: TObject);
begin
	// Exiting form
end;

procedure TfrmGameMediaSettings.FormShow(Sender: TObject);
begin
	// Exiting system?
	if (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// If the game is running, show this in the title of the form
	if (m_bGameRunning) then
		Caption := 'Chocolate Box Media [Busy !]'
	else
		Caption := 'Chocolate Box Media';

	// Initialise controls for game settings
	SetChildComboHandlers(gbMedia);

	// Show the game settings
	RefreshSettings();

	// Set focus to the OK button
	btnOk.SetFocus();

	// Start the timer
	SettingsTimer.Enabled := True;
end;

procedure TfrmGameMediaSettings.btnOkClick(Sender: TObject);
begin
	// Save settings
	SettingsTimer.Enabled := False;

	// Media
	settings.strMediaFolder := ebMediaFolder.Text;

	// If we get here, then settings are valid!
	m_bExiting := True;
	ModalResult := mrOk;
end;

procedure TfrmGameMediaSettings.btnCancelClick(Sender: TObject);
begin
	// Cancel any changes ?
	m_bExiting := True;
end;

procedure TfrmGameMediaSettings.btnBrowseClick(Sender: TObject);
var
	strDirectory: String;
begin
	SelectDirectory(strDirectory, [], 0);
	SelectDirectory('Select a directory', 'C:\', strDirectory);
	{dlgOpenFile.InitialDir := ebMediaFolder.Text;
	dlgOpenFile.FileName := '';
	strChosenDirectory := ebMediaFolder.Text;
	if (SelectDirectory(strChosenDirectory, options, 0)) then
		begin
		settings.strMediaFolder := strChosenDirectory;
		ebMediaFolder.Text := settings.strMediaFolder;
		UpdateMediaDetails();
		end; }

	// SelectDirectory
end;

procedure TfrmGameMediaSettings.imgRefreshClick(Sender: TObject);
begin
	// Update media details (user may have modified the path, added or removed files, etc)
	UpdateMediaDetails();
end;

procedure TfrmGameMediaSettings.OnSettingsTimerTick(Sender: TObject);
begin
	// Disable the timer
	SettingsTimer.Enabled := False;
	if (m_bExiting) or (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// Change the background colour of controls if they are disabled
	SetSubBackColour(gbMedia);

	// Re-enable the timer
	SettingsTimer.Enabled := True;
end;

end.
