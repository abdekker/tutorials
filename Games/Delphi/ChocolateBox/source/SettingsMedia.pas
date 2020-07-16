unit SettingsMedia;
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

  TfrmSettingsMedia = class(TGeneralBaseForm)
	btnCancel: TBitBtn;
	btnOk: TBitBtn;

	SettingsTimer: TTimer;
	gbMedia: TGroupBox;
	lblMediaFolder: TLabel;
	ebMediaFolder: TEdit;
	btnBrowse: TBitBtn;
	lblFolderInformation: TLabel;
	imgRefresh: TImage;
	imgMedia: TImage;
	imgWarning: TImage;

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
	m_bSettingUp, m_bExiting: Boolean;

	procedure EnableDisableControls();
	procedure RefreshSettings();
	procedure UpdateMediaDetails();

  public
	{ Public declarations }
	// Local copy of system settings
	settings: GAME_SETTINGS;
  end;

var
  frmSettingsMedia: TfrmSettingsMedia;

implementation

uses
  Messages, FormUtils, GameAbout, GameTypes;

{$R *.dfm}

// Private functions: Start
procedure TfrmSettingsMedia.EnableDisableControls();
begin
	// Some settings should not be altered while the game is in progress
	{if (m_bGameRunning) then
		begin
		SetSubControlsEnabled(
			gbMedia,
			(CONTROL_TLABEL + CONTROL_TEDIT + CONTROL_TBUTTON), False);
		end; }
end;

procedure TfrmSettingsMedia.RefreshSettings();
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

procedure TfrmSettingsMedia.UpdateMediaDetails();
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
// Public functions: End

procedure TfrmSettingsMedia.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_pParent := TForm(Sender);
	m_bSettingUp := False;
	m_bExiting := False;

	imgWarning.Top := imgMedia.Top;
	imgWarning.Left := imgMedia.Left;
end;

procedure TfrmSettingsMedia.FormDestroy(Sender: TObject);
begin
	// Exiting form
end;

procedure TfrmSettingsMedia.FormShow(Sender: TObject);
begin
	// Exiting system?
	if (ChocolateBox.bySystemExitRequest > 0) then
		Exit;

	// Initialise controls for game settings
	SetChildComboHandlers(gbMedia);

	// Show the game settings
	RefreshSettings();

	// Set focus to the OK button
	btnOk.SetFocus();

	// Start the timer
	SettingsTimer.Enabled := True;
end;

procedure TfrmSettingsMedia.btnOkClick(Sender: TObject);
begin
	// Save settings
	SettingsTimer.Enabled := False;

	// Media
	settings.strMediaFolder := ebMediaFolder.Text;

	// If we get here, then settings are valid!
	m_bExiting := True;
	ModalResult := mrOk;
end;

procedure TfrmSettingsMedia.btnCancelClick(Sender: TObject);
begin
	// Cancel any changes ?
	m_bExiting := True;
end;

procedure TfrmSettingsMedia.btnBrowseClick(Sender: TObject);
var
	strMsgLn1, strMsgLn2, strSelectedDir, strTest: String;
	nAppPathPos: Integer;

	//ADAD
	nTest: Integer;
begin
	// Ask the user user to select a directory. Alternative is:
	//		SelectDirectory(strDirectory, [], 0);
	// Note: The "410" used (which is the available width of the FileCtrl.TSelectDirDlg component)
	// was determined empirically
	SetCurrentDir(ChocolateBox.GameCache.szAppPath);
	strMsgLn1 := 'Select media folder. Current full path is:';
	strMsgLn2 := LimitStringLength(
		Format('%s', [ChocolateBox.GameCache.szAppPath]),
		lblFolderInformation.Canvas.Handle, 410, 10);
	if (SelectDirectory(strMsgLn1 + #13#10 + strMsgLn2, 'C:\', strSelectedDir)) then
		begin
		// New folder selected! If this folder is relative to the application directory, keep only
		// the relative information. Otherwise use the full path.
		nAppPathPos := AnsiPos(ChocolateBox.GameCache.szAppPath, strSelectedDir);
		if (nAppPathPos > 0) then
			begin
			// Path relative to the application
			settings.strMediaFolder := AnsiRightStr(strSelectedDir,
				Length(strSelectedDir) - (nAppPathPos + Length(ChocolateBox.GameCache.szAppPath) - 1));
			end
		else
			begin
			// Path separate from the application
			settings.strMediaFolder := strSelectedDir;
			end;

		RefreshSettings();
		end;
end;

procedure TfrmSettingsMedia.imgRefreshClick(Sender: TObject);
begin
	// Update media details (the user may have added or removed files)
	UpdateMediaDetails();
end;

procedure TfrmSettingsMedia.OnSettingsTimerTick(Sender: TObject);
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
