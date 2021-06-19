unit SampleApplicationForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Buttons, Classes, Controls, ExtCtrls, Forms, SysUtils, StdCtrls,
  CoreFormClasses, CoreTypes, FormUtils, SystemUtils;

type
  // Enumerations
  TCategory = (
	eCategoryNone,
	eCategoryWindows,
	eCategoryFiles,
	eCategoryHardware,
	eCategoryRegistry,
	eCategoryStrings,
	eCategoryControls
  );

  TBrowseExtraction = (
	eBrowseFullPath,	// eg. C:\Tmp\MyFile.exe
	eBrowseFileDrive	// eg. C:
  );

  CACHE_CONTROL_UPDATES_BROWSE = record
	// Options when the Browse button is displayed
	strInitialDir, strFilter: String;
	eFileOption: TBrowseExtraction;
  end;

  CACHE_CONTROL_UPDATES = record
	// Cache to assist control updates
	alblSampleTitle: array[1..3] of TLabel;
	aebSampleText: array[1..3] of TEdit;
	astrSampleTitleDefault, astrSampleTextDefault: array[1..3] of String;

	// Edit box and output list size (which can change depending on the selections made)
	nEditBoxWidthInitial, nEditBoxWidthMax: Integer;
	nOutputWidthInitial, nOutputWidthMax: Integer;
	bInitControlsGroup: Boolean;

	// Explanation text position (at design-time)
	nExplanationTop, nExplanationHeight: Integer;

	// Maximum size to set sample control
	nMaxControlWidth: Integer;

	// Whether to display some advanced explanation text
	strLastExplanationText: String;

	// Options when the Browse button is displayed
	browse: CACHE_CONTROL_UPDATES_BROWSE;
  end;

  CONTROL_UPDATES = record
	// Updates to the user interface
	astrSampleTitle, astrSampleText: array [1..3] of String;
	strExplanationText: String;
	bExplanationFixedWidth, bExplanationSmallFont, bExplanationExtraSize: Boolean;
	bShowBrowseButton: Boolean;
  end;

  TfrmSampleApplication = class(TGeneralBaseForm)
	gbSettings: TGroupBox;
	btnProcess: TBitBtn;
	btnBrowse: TButton;
	btnExit: TButton;
	btnClearOutput: TButton;

	lblCategory: TLabel;
	ddlCategory: TComboBox;
	lblAction: TLabel;
	ddlAction: TComboBox;
	lblSample1: TLabel;
	ebSample1: TEdit;
	lblSample2: TLabel;
	ebSample2: TEdit;
	lblSample3: TLabel;
	ebSample3: TEdit;
	lblExplanationText: TStaticText;
	listOutput: TListBox;

	gbSampleControls: TGroupBox;
	lblChildControls: TLabel;
	lblStatic: TStaticText;
	lblSingleLine: TLabel;
	ebEditBox: TEdit;
	cbComboBox: TComboBox;
	lblMultiLine: TLabel;

	UpdateTimer: TTimer;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure btnClearOutputClick(Sender: TObject);

	procedure ddlCategoryChange(Sender: TObject);
	procedure ddlActionClick(Sender: TObject);
	procedure btnProcessClick(Sender: TObject);
	procedure btnBrowseClick(Sender: TObject);

	procedure OnUpdateTimer(Sender: TObject);

  private
	{ Private declarations }
	m_bExiting: Boolean;

	m_eCategoryCurrent, m_eCategoryLastUpdate: TCategory;
	m_nActionCurrent, m_nActionLastUpdate: Integer;

	m_cache: CACHE_CONTROL_UPDATES;

	procedure CacheSettings();

	procedure PopulateCategories();

	procedure PopulateActions();
	procedure PopulateActions_Windows();
	procedure PopulateActions_Files();
	procedure PopulateActions_Hardware();
	procedure PopulateActions_Registry();
	procedure PopulateActions_Strings();
	procedure PopulateActions_Controls();

	procedure UpdateControls(updates: CONTROL_UPDATES);
	procedure UpdateControls_Windows();
	procedure UpdateControls_Files();
	procedure UpdateControls_Hardware();
	procedure UpdateControls_Registry();
	procedure UpdateControls_Strings();
	procedure UpdateControls_Controls();

	procedure PerformAction_Windows();
	procedure PerformAction_Files();
	procedure PerformAction_Hardware();
	procedure PerformAction_Registry();
	procedure PerformAction_Strings();
	procedure PerformAction_Controls();

	procedure AddOutputText(const cstrTxt: String);

  public
	{ Public declarations }
  end;

var
  frmSampleApplication: TfrmSampleApplication;

implementation

uses
  Dialogs, Graphics, Math;

const
  DUMMY_CONST = 0;

type
  // Available actions for each category
  TCategoryWindows = (
	eWindowsAction_Is64Bit,
	eWindowsAction_IsWindows10,
	eWindowsAction_GetWindowsLocale,
	eWindowsAction_ExpandEnvironment,
	eWindowsAction_IsProcessRunning,
	eWindowsAction_RunWindowsUtility,
	eWindowsAction_GetProcessThreads,
	eWindowsAction_GetSystemThreads,
	eWindowsAction_FindWindowByTitle,
	eWindowsAction_GetDiskSpace,
	eWindowsAction_GetDriveFileSystem,
	eWindowsAction_GetSystemDrives,
	eWindowsAction_CheckDriveIsValid,
	eWindowsAction_SaveToClipboard,
	eWindowsAction_BreakIfScrollLock
  );

  TCategoryFiles = (
	eFilesAction_GetImageSize,
	eFilesAction_FileHasData,
	eFilesAction_GetFolderListing,
	eFilesAction_GetParentFolder,
	eFilesAction_ShowFilenameParts
  );

  TCategoryHardware = (
	eHardwareAction_EnumSerialPorts
  );

  TCategoryRegistry = (
	eRegistryAction_GetString,			// REG_SZ
	eRegistryAction_GetMultiString,		// REG_MULTI_SZ
	eRegistryAction_GetExpandString,	// REG_EXPAND_SZ
	eRegistryAction_GetBinary,			// REG_BINARY
	eRegistryAction_GetDWORD			// REG_DWORD
	// REG_QWORD (not supported)
  );

  TCategoryStrings = (
	eStringsAction_IsNumber,
	eStringsAction_TryStringToInteger,
	eStringsAction_ExtractNumber,
	eStringsAction_ConvertThousands,
	eStringsAction_ConvertSpaces,
	eStringsAction_ConvertSignificantFigures,
	eStringsAction_TimeStringFromSeconds,
	eStringsAction_GetIsoDateTime,
	eStringsAction_ConvertTitleCase,
	eStringsAction_InsertFormattingCharacter,
	eStringsAction_ParseString,
	eStringsAction_GetRandomString
  );

  TCategoryControls = (
	eControls_DumpToFile,
	eControls_WinControlSize,
	eControls_GraphicControlSize,
	eControls_MultiLineLabel,
	eControls_SaveScreenshot,
	eControls_SelectAllText,
	eControls_GetListVisibleRows
  );

{$R *.dfm}

procedure TfrmSampleApplication.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_bExiting := False;

	m_eCategoryCurrent := eCategoryNone;
	m_eCategoryLastUpdate := eCategoryNone;

	m_nActionCurrent := -1;
	m_nActionLastUpdate := -1;

	// Seed the random number generator (7919 is the 1000th prime number)
	RandSeed := Integer(GetTickCount() mod 7919);

	// Initialise system and form utilities
	InitialiseSystemUtils(Application.Handle);
	InitialiseFormUtils();
end;

procedure TfrmSampleApplication.FormDestroy(Sender: TObject);
begin
	// Clean up
	CloseSystemUtils();
	CloseFormUtils();
end;

procedure TfrmSampleApplication.FormShow(Sender: TObject);
begin
	// Show form
	CacheSettings();
	SetChildComboHandlers(gbSettings);
	PopulateCategories();
	UpdateTimer.Enabled := True;
end;

procedure TfrmSampleApplication.btnExitClick(Sender: TObject);
begin
	// Close form
	m_bExiting := True;
	Close();
end;

procedure TfrmSampleApplication.btnClearOutputClick(Sender: TObject);
begin
	// Clear the output!
	listOutput.Items.Clear();
end;

procedure TfrmSampleApplication.ddlCategoryChange(Sender: TObject);
var
	eCategory: TCategory;
begin
	// Set the actions for this category
	eCategory := TCategory(ddlCategory.Items.Objects[ddlCategory.ItemIndex]);
	if (eCategory <> m_eCategoryCurrent) then
		begin
		m_eCategoryCurrent := eCategory;
		PopulateActions();
		end;
end;

procedure TfrmSampleApplication.ddlActionClick(Sender: TObject);
begin
	// If appropriate, put some starting text into the sample edit box
	m_nActionLastUpdate := -1;
end;

procedure TfrmSampleApplication.btnProcessClick(Sender: TObject);
begin
	// Perform some action...
	m_nActionCurrent := ddlAction.ItemIndex;
	case m_eCategoryCurrent of
		eCategoryWindows:
			PerformAction_Windows();

		eCategoryFiles:
			PerformAction_Files();

		eCategoryHardware:
			PerformAction_Hardware();

		eCategoryRegistry:
			PerformAction_Registry();

		eCategoryStrings:
			PerformAction_Strings();

		eCategoryControls:
			PerformAction_Controls();
		end;
end;

procedure TfrmSampleApplication.btnBrowseClick(Sender: TObject);
var
	dlgOpenFile: TOpenDialog;
begin
	// Browse for a file or folder and populate the first sample box
	// Note: The caller should set the initial directory and filter
	dlgOpenFile := TOpenDialog.Create(Self);
	dlgOpenFile.InitialDir := m_cache.browse.strInitialDir;
	dlgOpenFile.Filter := m_cache.browse.strFilter;
	dlgOpenFile.FileName := '';
	if (dlgOpenFile.Execute) then
		begin
		if (m_cache.browse.eFileOption = eBrowseFullPath) then
			ebSample1.Text := dlgOpenFile.FileName
		else if (m_cache.browse.eFileOption = eBrowseFileDrive) then
			ebSample1.Text := ExtractFileDrive(dlgOpenFile.FileName);
		end;

	dlgOpenFile.Free();
end;

procedure TfrmSampleApplication.OnUpdateTimer(Sender: TObject);
var
	eCategory: TCategory;
	nAction: Integer;
begin
	// An update timer event...
	if (m_bExiting) then
		Exit;

	// Disable timer while we perform the task for that timer
	UpdateTimer.Enabled := False;

	// Any change in the category or action?
	eCategory := TCategory(ddlCategory.Items.Objects[ddlCategory.ItemIndex]);
	nAction := ddlAction.ItemIndex;
	if (	(eCategory <> m_eCategoryLastUpdate) or
			(nAction <> m_nActionLastUpdate)) then
		begin
		m_eCategoryLastUpdate := eCategory;
		m_nActionLastUpdate := nAction;
		case m_eCategoryCurrent of
			eCategoryWindows:
				UpdateControls_Windows();

			eCategoryFiles:
				UpdateControls_Files();

			eCategoryHardware:
				UpdateControls_Hardware();

			eCategoryRegistry:
				UpdateControls_Registry();

			eCategoryStrings:
				UpdateControls_Strings();

			eCategoryControls:
				UpdateControls_Controls();
			end;
		end;

	// Restart the timer
	UpdateTimer.Enabled := True;
end;

// Private functions: Start
procedure TfrmSampleApplication.CacheSettings();
var
	nSample: Integer;
begin
	// Cached settings used by this form
	ZeroMemory(@m_cache, SizeOf(CACHE_CONTROL_UPDATES));

	// Sample text
	for nSample:=1 to 3 do
		begin
		m_cache.alblSampleTitle[nSample] :=
			TLabel(FindComponent(Format('lblSample%d', [nSample])));
		m_cache.aebSampleText[nSample] :=
			TEdit(FindComponent(Format('ebSample%d', [nSample])));

		m_cache.astrSampleTitleDefault[nSample] := Format('Variable %d', [nSample]);
		m_cache.astrSampleTextDefault[nSample] := Format('sample text %d', [nSample]);
		end;

	// Initial and maximum width of edit boxes and the output list
	m_cache.nEditBoxWidthInitial := ebSample1.Width;
	m_cache.nEditBoxWidthMax := (gbSettings.Width - ebSample1.Left - listOutput.Left);

	m_cache.nOutputWidthInitial := listOutput.Width;
	m_cache.nOutputWidthMax := (gbSettings.Width - (2 * listOutput.Left));

	m_cache.bInitControlsGroup := True;

	// Explanation text position (at design-time)
	m_cache.nExplanationTop := lblExplanationText.Top;
	m_cache.nExplanationHeight := lblExplanationText.Height;

	// Maximum size to set sample controls
	m_cache.nMaxControlWidth := lblChildControls.Width;

	// Last explanation text
	m_cache.strLastExplanationText := 'z';
end;

procedure TfrmSampleApplication.PopulateCategories();
begin
	// Add categories
	ddlCategory.Items.Clear();
	ddlCategory.Items.AddObject('Windows', TObject(eCategoryWindows));
	ddlCategory.Items.AddObject('Files', TObject(eCategoryFiles));
	ddlCategory.Items.AddObject('Hardware', TObject(eCategoryHardware));
	ddlCategory.Items.AddObject('Registry', TObject(eCategoryRegistry));
	ddlCategory.Items.AddObject('Strings', TObject(eCategoryStrings));
	ddlCategory.Items.AddObject('Controls', TObject(eCategoryControls));
	ddlCategory.DropDownCount := ddlCategory.Items.Count;
	ddlCategory.ItemIndex := 0;

	// Set actions for the initial category
	m_eCategoryCurrent := TCategory(ddlCategory.Items.Objects[ddlCategory.ItemIndex]);
	PopulateActions();
end;

procedure TfrmSampleApplication.PopulateActions();
begin
	// Population the Actions based on the category
	ddlAction.Items.Clear();
	case m_eCategoryCurrent of
		eCategoryWindows:
			PopulateActions_Windows();

		eCategoryFiles:
			PopulateActions_Files();

		eCategoryHardware:
			PopulateActions_Hardware();

		eCategoryRegistry:
			PopulateActions_Registry();

		eCategoryStrings:
			PopulateActions_Strings();

		eCategoryControls:
			PopulateActions_Controls();
		end;

	// Set the width of the output window and controls
	if (	(m_eCategoryCurrent = eCategoryWindows) or
			(m_eCategoryCurrent = eCategoryHardware) or
			(m_eCategoryCurrent = eCategoryRegistry) or
			(m_eCategoryCurrent = eCategoryStrings)) then
		begin
		gbSampleControls.Visible := False;
		listOutput.Width := m_cache.nOutputWidthMax;
		end
	else if (m_eCategoryCurrent = eCategoryControls) then
		begin
		// Set the output window width to reveal some sample controls
		listOutput.Width := m_cache.nOutputWidthInitial;
		if (m_cache.bInitControlsGroup) then
			begin
			m_cache.bInitControlsGroup := False;
			lblChildControls.Caption := Format('There are %d child controls in this group', [
				gbSampleControls.ControlCount]);
			gbSampleControls.Caption := 'Sample controls';
			end;

		gbSampleControls.Visible := True;
		end;

	// Set dropdown count and force an update
	ddlAction.DropDownCount := ddlAction.Items.Count;
	ddlAction.ItemIndex := 0;
	m_nActionLastUpdate := -1;
end;

procedure TfrmSampleApplication.PopulateActions_Windows();
begin
	// Windows: Populate actions for this category
	ddlAction.Items.AddObject('Is Windows 64-bit ?', TObject(eWindowsAction_Is64Bit));
	ddlAction.Items.AddObject('Running Windows 10 ?', TObject(eWindowsAction_IsWindows10));
	ddlAction.Items.AddObject('Get Windows locale', TObject(eWindowsAction_GetWindowsLocale));
	ddlAction.Items.AddObject('Expand enviroment variable', TObject(eWindowsAction_ExpandEnvironment));
	ddlAction.Items.AddObject('Is process running ?', TObject(eWindowsAction_IsProcessRunning));
	ddlAction.Items.AddObject('Run Windows utililty', TObject(eWindowsAction_RunWindowsUtility));
	ddlAction.Items.AddObject('Get process threads', TObject(eWindowsAction_GetProcessThreads));
	ddlAction.Items.AddObject('Get system threads', TObject(eWindowsAction_GetSystemThreads));
	ddlAction.Items.AddObject('Find windows by title', TObject(eWindowsAction_FindWindowByTitle));
	ddlAction.Items.AddObject('Get disk free space (in GB)', TObject(eWindowsAction_GetDiskSpace));
	ddlAction.Items.AddObject('Get drive filesystem', TObject(eWindowsAction_GetDriveFileSystem));
	ddlAction.Items.AddObject('Get system drives', TObject(eWindowsAction_GetSystemDrives));
	ddlAction.Items.AddObject('Check drive is valid', TObject(eWindowsAction_CheckDriveIsValid));
	ddlAction.Items.AddObject('Save output to clipboard', TObject(eWindowsAction_SaveToClipboard));
	ddlAction.Items.AddObject(
		{$IFDEF DBG} 'Break if Scroll Lock is pressed'
		{$ELSE} 'Break if Scroll Lock is pressed (DEBUG only)'
		{$ENDIF}, TObject(eWindowsAction_BreakIfScrollLock));
end;

procedure TfrmSampleApplication.PopulateActions_Files();
begin
	// Files: Populate actions for this category
	ddlAction.Items.AddObject('Get image file size', TObject(eFilesAction_GetImageSize));
	ddlAction.Items.AddObject('Does file have data ?', TObject(eFilesAction_FileHasData));
	ddlAction.Items.AddObject('Get folder listing', TObject(eFilesAction_GetFolderListing));
	ddlAction.Items.AddObject('Get parent folder', TObject(eFilesAction_GetParentFolder));
	ddlAction.Items.AddObject('File path details', TObject(eFilesAction_ShowFilenameParts));
end;

procedure TfrmSampleApplication.PopulateActions_Hardware();
begin
	// Hardware: Populate actions for this category
	ddlAction.Items.AddObject('Enumerate serial (COM) ports', TObject(eHardwareAction_EnumSerialPorts));
end;

procedure TfrmSampleApplication.PopulateActions_Registry();
begin
	// Registry: Populate actions for this category
	ddlAction.Items.AddObject('Read string (REG_SZ)', TObject(eRegistryAction_GetString));
	ddlAction.Items.AddObject('Read sequence of strings (REG_MULTI_SZ)',
		TObject(eRegistryAction_GetMultiString));
	ddlAction.Items.AddObject('String with environment variables (REG_EXPAND_SZ)',
		TObject(eRegistryAction_GetExpandString));
	ddlAction.Items.AddObject('Binary data (REG_BINARY)', TObject(eRegistryAction_GetBinary));
	ddlAction.Items.AddObject('32-bit integer (REG_DWORD)', TObject(eRegistryAction_GetDWORD));
end;

procedure TfrmSampleApplication.PopulateActions_Strings();
begin
	// Strings: Populate actions for this category
	ddlAction.Items.AddObject('Is this a number ?', TObject(eStringsAction_IsNumber));
	ddlAction.Items.AddObject('Try convert to integer', TObject(eStringsAction_TryStringToInteger));
	ddlAction.Items.AddObject('Extract number', TObject(eStringsAction_ExtractNumber));
	ddlAction.Items.AddObject('Convert with thousand commas', TObject(eStringsAction_ConvertThousands));
	ddlAction.Items.AddObject('Convert with thousand spaces', TObject(eStringsAction_ConvertSpaces));
	ddlAction.Items.AddObject('Format float with specified significant figures',
		TObject(eStringsAction_ConvertSignificantFigures));
	ddlAction.Items.AddObject('Time string from seconds', TObject(eStringsAction_TimeStringFromSeconds));
	ddlAction.Items.AddObject('Get ISO 8601 date/time string', TObject(eStringsAction_GetIsoDateTime));
	ddlAction.Items.AddObject('Convert title case', TObject(eStringsAction_ConvertTitleCase));
	ddlAction.Items.AddObject('Insert formatting char', TObject(eStringsAction_InsertFormattingCharacter));
	ddlAction.Items.AddObject('Parse string', TObject(eStringsAction_ParseString));
	ddlAction.Items.AddObject('Generate random string', TObject(eStringsAction_GetRandomString));
end;

procedure TfrmSampleApplication.PopulateActions_Controls();
begin
	// Controls: Populate actions for this category
	ddlAction.Items.AddObject('Dump to file', TObject(eControls_DumpToFile));
	ddlAction.Items.AddObject('TWinControl size', TObject(eControls_WinControlSize));
	ddlAction.Items.AddObject('TGraphicControl size', TObject(eControls_GraphicControlSize));
	ddlAction.Items.AddObject('Multi-line label', TObject(eControls_MultiLineLabel));
	ddlAction.Items.AddObject('Save screenshot', TObject(eControls_SaveScreenshot));
	ddlAction.Items.AddObject('Select all text (TEdit)', TObject(eControls_SelectAllText));
	ddlAction.Items.AddObject('Get visible rows (TListBox)', TObject(eControls_GetListVisibleRows));
end;

procedure TfrmSampleApplication.UpdateControls(updates: CONTROL_UPDATES);
var
	nSample: Integer;
begin
	// Update controls
	for nSample:=1 to 3 do
		begin
		if (Length(updates.astrSampleTitle[nSample]) = 0) then
			begin
			// Disable this label (the action does not require, or is indepenent of, this text)
			m_cache.alblSampleTitle[nSample].Caption := m_cache.astrSampleTitleDefault[nSample];
			m_cache.aebSampleText[nSample].Text := m_cache.astrSampleTextDefault[nSample];

			m_cache.alblSampleTitle[nSample].Enabled := False;
			m_cache.aebSampleText[nSample].Enabled := False;
			m_cache.aebSampleText[nSample].Color := clSkyBlue;
			end
		else
			begin
			// Enable this label
			m_cache.alblSampleTitle[nSample].Caption := updates.astrSampleTitle[nSample];
			m_cache.aebSampleText[nSample].Text := updates.astrSampleText[nSample];

			m_cache.alblSampleTitle[nSample].Enabled := True;
			m_cache.aebSampleText[nSample].Enabled := True;
			m_cache.aebSampleText[nSample].Color := clInfoBk;
			end;
		end;

	// Explanation text?
	if (Length(updates.strExplanationText) <> 0) then
		begin
		// Show explanation text
		if (updates.strExplanationText <> m_cache.strLastExplanationText) then
			begin
			// Set the text
			m_cache.strLastExplanationText := updates.strExplanationText;
			for nSample:=1 to 3 do
				m_cache.aebSampleText[nSample].Width := m_cache.nEditBoxWidthInitial;

			lblExplanationText.Caption := updates.strExplanationText;

			// Set the font and size
			if (updates.bExplanationFixedWidth) then
				lblExplanationText.Font.Name := 'Courier New'
			else
				lblExplanationText.Font.Name := 'MS Sans Serif';

			if (updates.bExplanationSmallFont) then
				lblExplanationText.Font.Size := 8
			else
				lblExplanationText.Font.Size := 10;

			if (updates.bExplanationExtraSize) then
				begin
				lblExplanationText.Top := (btnProcess.Top + btnProcess.Height + 3);
				lblExplanationText.Height := (listOutput.Top - lblExplanationText.Top);
				end
			else
				begin
				lblExplanationText.Top := m_cache.nExplanationTop;
				lblExplanationText.Height := m_cache.nExplanationHeight;
				end;

			// Make the explanation text visible
			lblExplanationText.Visible := True;
			end;
		end
	else
		begin
		// Hide explanation text
		if (Length(m_cache.strLastExplanationText) <> 0) then
			begin
			m_cache.strLastExplanationText := '';
			lblExplanationText.Visible := False;
			for nSample:=1 to 3 do
				m_cache.aebSampleText[nSample].Width := m_cache.nEditBoxWidthMax;
			end;
		end;

	// Show a browse button?
	btnBrowse.Visible := updates.bShowBrowseButton;
end;

procedure TfrmSampleApplication.UpdateControls_Windows();
var
	updates: CONTROL_UPDATES;
begin
	// Windows: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case TCategoryWindows(ddlAction.ItemIndex) of
		eWindowsAction_Is64Bit: ;
		eWindowsAction_IsWindows10: ;
		eWindowsAction_GetWindowsLocale: ;
		eWindowsAction_ExpandEnvironment:
			begin
			updates.astrSampleTitle[1] := 'Variable';
			updates.astrSampleText[1] := 'APPDATA';
			end;

		eWindowsAction_IsProcessRunning:
			begin
			updates.astrSampleTitle[1] := 'Process name';
			updates.astrSampleText[1] := ExtractFileName(Application.ExeName);
			end;

		eWindowsAction_RunWindowsUtility:
			begin
			updates.astrSampleTitle[1] := 'Utility';
			updates.astrSampleText[1] := '1';

			updates.strExplanationText := (
				'1=Command Prompt    10=Device Manager' + #13 +
				'2=Date/Time         11=Event Viewer' + #13 +
				'3=Defrag            12=Firewall' + #13 +
				'4=Explorer          13=Language Options' + #13 +
				'5=Mouse             14=Network' + #13 +
				'6=Notepad           15=Services' + #13 +
				'7=Windows Start');
			updates.bExplanationFixedWidth := True;
			updates.bExplanationSmallFont := True;
			updates.bExplanationExtraSize := True;
			end;

		eWindowsAction_GetProcessThreads: ;
		eWindowsAction_GetSystemThreads: ;
		eWindowsAction_FindWindowByTitle:
			begin
			updates.astrSampleTitle[1] := 'Window title';
			updates.astrSampleText[1] := 'Delphi 7';
			end;

		eWindowsAction_GetDiskSpace:
			begin
			updates.astrSampleTitle[1] := 'Drive';
			updates.astrSampleText[1] := 'C:';
			end;

		eWindowsAction_GetDriveFileSystem:
			begin
			updates.astrSampleTitle[1] := 'Drive';
			updates.astrSampleText[1] := 'C:';
			end;

		eWindowsAction_GetSystemDrives: ;
		eWindowsAction_CheckDriveIsValid:
			begin
			updates.astrSampleTitle[1] := 'Drive 1';
			updates.astrSampleText[1] := 'C:';

			updates.astrSampleTitle[2] := 'Drive 2';
			updates.astrSampleText[2] := 'Q:';
			end;

		eWindowsAction_SaveToClipboard: ;
{$IFDEF DBG}
		eWindowsAction_BreakIfScrollLock:
			begin
			updates.astrSampleTitle[1] := 'Instructions';
			updates.astrSampleText[1] := 'Press Scroll Lock while debugging';
			end;
{$ENDIF}
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.UpdateControls_Files();
var
	updates: CONTROL_UPDATES;
begin
	// Files: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case TCategoryFiles(ddlAction.ItemIndex) of
		eFilesAction_GetImageSize:
			begin
			updates.astrSampleTitle[1] := 'Image file';
			updates.astrSampleText[1] := '(use browse button or paste full path here)';

			updates.bShowBrowseButton := True;
			m_cache.browse.strInitialDir :=
				(GetParentFolder(GetCurrentDir(), 3) + 'Graphics\Sample-Image-Types\');
			m_cache.browse.strFilter := (
				'All image types|*.*' +
				'|Bitmap (*.bmp)|*.bmp' +
				'|JPEG (*.jpg)|*.jpg;*.jpeg' +
				'|Portable Network Graphics (*.png)|*.png' +
				'|Graphics Interface Format (*.gif)|*.gif' +
				'|Icon (*.ico)|*.ico' +
				'|Tag Image File Format (*.tif)|*.tif;*.tiff' +
				'|MetaFile (*.emf, *.wmf)|*.emf;*.wmf');
			m_cache.browse.eFileOption := eBrowseFullPath;
			end;

		eFilesAction_FileHasData:
			begin
			updates.astrSampleTitle[1] := 'File';
			updates.astrSampleText[1] := '(use browse button or paste full path here)';

			updates.bShowBrowseButton := True;
			m_cache.browse.strInitialDir :=
				(GetParentFolder(GetCurrentDir(), 3) + 'Graphics\Sample-Image-Types\');
			m_cache.browse.strFilter := 'All file types|*.*';
			m_cache.browse.eFileOption := eBrowseFullPath;
			end;

		eFilesAction_GetFolderListing:
			begin
			updates.astrSampleTitle[1] := 'Folder';
			updates.astrSampleText[1] :=
				(GetParentFolder(GetCurrentDir(), 3) + 'Graphics\Sample-Image-Types\');

			updates.astrSampleTitle[2] := 'Filter';
			updates.astrSampleText[2] := '*.ico';
			end;

		eFilesAction_GetParentFolder:
			begin
			updates.astrSampleTitle[1] := 'Folder';
			updates.astrSampleText[1] :=
				(GetParentFolder(GetCurrentDir(), 3) + 'Graphics\Sample-Image-Types\');

			updates.astrSampleTitle[2] := 'Levels';
			updates.astrSampleText[2] := '2';
			end;

		eFilesAction_ShowFilenameParts:
			begin
			updates.astrSampleTitle[1] := 'Filename';
			updates.astrSampleText[1] := 'C:\Tmp\Subfolder\AmazingFile.abc';
			end;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.UpdateControls_Hardware();
var
	updates: CONTROL_UPDATES;
begin
	// Hardware: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case TCategoryHardware(ddlAction.ItemIndex) of
		eHardwareAction_EnumSerialPorts:
			;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.UpdateControls_Registry();
var
	updates: CONTROL_UPDATES;
begin
	// Registry: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case TCategoryRegistry(ddlAction.ItemIndex) of
		eRegistryAction_GetString:
			begin
			updates.astrSampleTitle[1] := 'Root key';
			updates.astrSampleText[1] := 'HKLM';

			updates.astrSampleTitle[2] := 'Key name';
			updates.astrSampleText[2] := 'SOFTWARE\Microsoft\Windows\CurrentVersion\ProgramFilesDir';
			end;

		eRegistryAction_GetMultiString:
			begin
			updates.astrSampleTitle[1] := 'Root key';
			updates.astrSampleText[1] := 'HKLM';

			updates.astrSampleTitle[2] := 'Key name';
			updates.astrSampleText[2] := 'SYSTEM\CurrentControlSet\Services\RpcSs\RequiredPrivileges';
			end;

		eRegistryAction_GetExpandString:
			begin
			updates.astrSampleTitle[1] := 'Root key';
			updates.astrSampleText[1] := 'HKCR';

			updates.astrSampleTitle[2] := 'Key name';
			updates.astrSampleText[2] := 'txtfile\DefaultIcon\';
			end;

		eRegistryAction_GetBinary:
			begin
			updates.astrSampleTitle[1] := 'Root key';
			updates.astrSampleText[1] := 'HKCU';

			updates.astrSampleTitle[2] := 'Key name';
			updates.astrSampleText[2] := 'Control Panel\Desktop\WindowMetrics\CaptionFont';
			end;

		eRegistryAction_GetDWORD:
			begin
			updates.astrSampleTitle[1] := 'Root key';
			updates.astrSampleText[1] := 'HKCU';

			updates.astrSampleTitle[2] := 'Key name';
			updates.astrSampleText[2] := 'Software\Microsoft\Notepad\iWindowPosX';
			end;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.UpdateControls_Strings();
var
	updates: CONTROL_UPDATES;
begin
	// Strings: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case TCategoryStrings(ddlAction.ItemIndex) of
		eStringsAction_IsNumber:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1569';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '1.569a';
			end;

		eStringsAction_TryStringToInteger:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1569';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '1.569a';
			end;

		eStringsAction_ExtractNumber:
			begin
			updates.astrSampleTitle[1] := 'Number';
			updates.astrSampleText[1] := '-23.7m/min';

			updates.astrSampleTitle[2] := 'Options';
			updates.astrSampleText[2] := '1';

			updates.strExplanationText := (
				'Options is a bitmask:' + #13 +
				'    0x01 = STR_NUMERIC ("0" to "9")' + #13 +
				'    0x02 = STR_MINUS ("-")' + #13 +
				'    0x04 = STR_PLUS ("+")' + #13 +
				'    0x08 = STR_DECIMAL (".")');
			end;

		eStringsAction_ConvertThousands:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1234567.89';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '-34567';
			end;

		eStringsAction_ConvertSpaces:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1234567';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '34567';
			end;

		eStringsAction_ConvertSignificantFigures:
			begin
			updates.astrSampleTitle[1] := 'Number';
			updates.astrSampleText[1] := '13.456839';

			updates.astrSampleTitle[2] := 'Significant figures';
			updates.astrSampleText[2] := '6';
			end;

		eStringsAction_TimeStringFromSeconds: ;
		eStringsAction_GetIsoDateTime: ;

		eStringsAction_ConvertTitleCase:
			begin
			updates.astrSampleTitle[1] := 'String 1';
			updates.astrSampleText[1] := 'the cat SAT ON THE MAT';

			updates.astrSampleTitle[2] := 'String 2';
			updates.astrSampleText[2] := 'FRED LOVES ICE CREAM';
			end;

		eStringsAction_InsertFormattingCharacter:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := '0052C2539000';

			updates.astrSampleTitle[2] := 'Formatting character';
			updates.astrSampleText[2] := '-';

			updates.astrSampleTitle[3] := 'Every N';
			updates.astrSampleText[3] := '2';
			end;

		eStringsAction_ParseString:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := '192.168.2.108';

			updates.astrSampleTitle[2] := 'Parse string';
			updates.astrSampleText[2] := '.';
			end;

		eStringsAction_GetRandomString:
			begin
			updates.astrSampleTitle[1] := 'Length';
			updates.astrSampleText[1] := '12';
			end;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.UpdateControls_Controls();
var
	updates: CONTROL_UPDATES;
begin
	// Controls: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case TCategoryControls(ddlAction.ItemIndex) of
		eControls_DumpToFile:
			begin
			updates.astrSampleTitle[1] := 'Filename';
			updates.astrSampleText[1] := 'C:\Tmp\ControlDump.txt';
			end;

		eControls_WinControlSize:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := 'Hello TWinControl';
			end;

		eControls_GraphicControlSize:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := 'Hello TGraphicControl';
			end;

		eControls_MultiLineLabel:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := 'Hello multi-line label...';

			updates.astrSampleTitle[2] := 'Iterations';
			updates.astrSampleText[2] := '5';
			end;

		eControls_SaveScreenshot:
			begin
			updates.astrSampleTitle[1] := 'Filename';
			updates.astrSampleText[1] := 'C:\Tmp\Screenshot.jpg';

			updates.astrSampleTitle[2] := 'Image type';
			updates.astrSampleText[2] := '1';

			updates.strExplanationText := (
			'The image type can be:' + #13 +
			'    1 = jpeg (.jpg)' + #13 +
			'    2 = bitmap (.bmp)' + #13);
			end;

		eControls_SelectAllText:
			begin
			updates.astrSampleTitle[1] := 'Edit';
			updates.astrSampleText[1] := 'This text will be selected';
			end;

		eControls_GetListVisibleRows: ;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.PerformAction_Windows();
var
	strTmp: String;
	cTmp: Char;
	nTmp: Integer;
	dwTmp: DWORD;
	hWndTmp: HWND;
	fTotalGB, fFreeGB: Single;
begin
	// Windows: Perform the action
	case TCategoryWindows(m_nActionCurrent) of
		eWindowsAction_Is64Bit:
			begin
			if (IsWindows64Bit()) then
				AddOutputText('Windows is 64-bit')
			else
				AddOutputText('Windows is NOT 64-bit');
			end;

		eWindowsAction_IsWindows10:
			begin
			if (IsWindows10()) then
				AddOutputText('Running Windows 10')
			else
				AddOutputText('NOT Windows 10...');
			end;

		eWindowsAction_GetWindowsLocale:
			AddOutputText(Format('Windows locale is %s', [GetWindowsLocale()]));

		eWindowsAction_ExpandEnvironment:
			begin
			// If required, add % symbols either side of the value
			if (Pos('%', ebSample1.Text) <> 0) then
				strTmp := ebSample1.Text
			else
				strTmp := ('%' + ebSample1.Text + '%');

			AddOutputText(Format('%s  =  "%s"', [strTmp, ExpandEnvironment(strTmp)]));
			end;

		eWindowsAction_IsProcessRunning:
			begin
			if (IsProcessRunning(ebSample1.Text)) then
				AddOutputText('Process is running')
			else
				AddOutputText('Process is NOT running');
			end;

		eWindowsAction_RunWindowsUtility:
			begin
			// Which utility are we running?
			if (TryStrToInt(m_cache.aebSampleText[1].Text, nTmp)) then
				begin
				dwTmp := WIN_UTIL_COMMAND_PROMPT;
				case nTmp of
					1: dwTmp := WIN_UTIL_COMMAND_PROMPT;
					2: dwTmp := WIN_UTIL_DATE_TIME;
					3: dwTmp := WIN_UTIL_DEFRAG;
					4: dwTmp := WIN_UTIL_WIN_EXPLORER;
					5: dwTmp := WIN_UTIL_MOUSE;
					6: dwTmp := WIN_UTIL_NOTEPAD;
					7: dwTmp := WIN_UTIL_START;

					10: dwTmp := WIN_UTIL_DEVICE_MANAGER;
					11: dwTmp := WIN_UTIL_EVENT_VIEWER;
					12: dwTmp := WIN_UTIL_FIREWALL;
					13: dwTmp := WIN_UTIL_LANGUAGE_OPTIONS;
					14: dwTmp := WIN_UTIL_NETWORK;
					15: dwTmp := WIN_UTIL_SERVICES;
				end;

				if (RunWindowsUtility(dwTmp)) then
					AddOutputText('Utility launched')
				else
					AddOutputText('Failed to launch utility');
				end
			else
				AddOutputText(Format('"%s" is NOT a number', [m_cache.aebSampleText[1].Text]));
			end;

		eWindowsAction_GetProcessThreads:
			AddOutputText(Format('Process threads = %d (process ID = %d)', [
				GetProcessThreadCount(GetCurrentProcessId()),
				GetCurrentProcessId()]));

		eWindowsAction_GetSystemThreads:
			AddOutputText(Format('Total system threads = %d', [GetSystemThreadCount()]));

		eWindowsAction_FindWindowByTitle:
			begin
			hWndTmp := FindWindowByTitle(Application.Handle, ebSample1.Text);
			if (hWndTmp <> 0) then
				AddOutputText(Format('Window "%s" found with handle %d', [ebSample1.Text, hWndTmp]))
			else
				AddOutputText(Format('Window "%s" NOT found', [ebSample1.Text]));
			end;

		eWindowsAction_GetDiskSpace:
			begin
			GetDiskSpaceGB(ebSample1.Text, fTotalGB, fFreeGB);
			if (fTotalGB > MIN_SINGLE) then
				AddOutputText(Format('%s is a %.1fGB disk with %.1fGB free space', [
					ebSample1.Text, fTotalGB, fFreeGB]))
			else
				AddOutputText(Format('%s is not a valid drive', [ebSample1.Text]));
			end;

		eWindowsAction_GetDriveFileSystem:
			begin
			strTmp := GetDriveFileSystem(ebSample1.Text);
			if (Length(strTmp) > 0) then
				AddOutputText(Format('The filesystem on %s is %s', [ebSample1.Text, strTmp]))
			else
				AddOutputText(Format('Unable to detector filesystem on %s', [ebSample1.Text]));
			end;

		eWindowsAction_GetSystemDrives:
			AddOutputText(Format('System drives are %s', [GetSystemDrives()]));

		eWindowsAction_CheckDriveIsValid:
			begin
			for nTmp:=1 to 2 do
				begin
				if (Length(m_cache.aebSampleText[nTmp].Text) > 0) then
					begin
					cTmp := m_cache.aebSampleText[nTmp].Text[1];
					if (CheckDriveIsValid(cTmp)) then
						AddOutputText(Format('%s:\ is a valid drive', [cTmp]))
					else
						AddOutputText(Format('%s:\ is NOT a valid drive', [cTmp]));
					end
				else
					AddOutputText(Format('"%s" should not be blank', [m_cache.aebSampleText[nTmp].Text]));
				end;
			end;

		eWindowsAction_SaveToClipboard:
			begin
			strTmp := '';
			for nTmp:=0 to (listOutput.Items.Count - 1) do
				strTmp := (strTmp + listOutput.Items[nTmp] + #13);

			SaveToClipboard(strTmp);
			Application.MessageBox(
				PAnsiChar('Output saved to the clipboard'), 'Windows', MB_ICONEXCLAMATION);
			end;

		eWindowsAction_BreakIfScrollLock:
{$IFDEF DBG}
			begin
			if (BreakIfScrollLock()) then
				AddOutputText('Scroll Lock detected while debugging - breakpoint forced!')
			else
				AddOutputText('Scroll Lock not detected or not debugging');
			end;
{$ELSE}
			AddOutputText('Scroll Lock is only tested in RELEASE builds');
{$ENDIF}
		end;
end;

procedure TfrmSampleApplication.PerformAction_Files();
var
	imgLoaded: TImage;
	astrFiles: TStringList;
	nFile, nValue: Integer;
begin
	// Windows: Perform the action
	case TCategoryFiles(m_nActionCurrent) of
		eFilesAction_GetImageSize:
			begin
			if (FileExists(ebSample1.Text)) then
				begin
				imgLoaded := TImage.Create(Self);
				AssignPictureFromFile(imgLoaded.Picture, m_cache.aebSampleText[1].Text);
				AddOutputText(Format('%s image loaded: Width=%d, Height=%d', [
					DetectImageType(m_cache.aebSampleText[1].Text),
					imgLoaded.Picture.Width,
					imgLoaded.Picture.Height]));
				imgLoaded.Picture := nil;
				imgLoaded.Free();
				end
			else
				AddOutputText('File does not exist');
			end;

		eFilesAction_FileHasData:
			begin
			if (FileExists(m_cache.aebSampleText[1].Text)) then
				begin
				if (FileHasData(m_cache.aebSampleText[1].Text)) then
					AddOutputText('File exists and has some data')
				else
					AddOutputText('File exists but has no data or is corrupted');
				end
			else
				AddOutputText('File does not exist');
			end;

		eFilesAction_GetFolderListing:
			begin
			astrFiles := TStringList.Create();
			GetFolderListing(m_cache.aebSampleText[1].Text, m_cache.aebSampleText[2].Text, astrFiles);
			if (astrFiles.Count > 0) then
				begin
				for nFile:=0 to (astrFiles.Count-1) do
					AddOutputText(astrFiles[nFile]);
				end
			else
				AddOutputText(Format('No files found in that folder type type "%s"', [
					m_cache.aebSampleText[2].Text]));

			astrFiles.Free();
			end;

		eFilesAction_GetParentFolder:
			begin
			if (TryStrToInt(m_cache.aebSampleText[2].Text, nValue)) then
				AddOutputText(GetParentFolder(m_cache.aebSampleText[1].Text, nValue))
			else
				AddOutputText(Format('"%s" not a valid level', [m_cache.aebSampleText[2].Text]));
			end;

		eFilesAction_ShowFilenameParts:
			begin
			AddOutputText(Format('%s', [m_cache.aebSampleText[1].Text]));
			AddOutputText(Format('  Drive = %s', [ExtractFileDrive(m_cache.aebSampleText[1].Text)]));
			AddOutputText(Format('  Directory = %s', [ExtractFileDir(m_cache.aebSampleText[1].Text)]));
			AddOutputText(Format('  Path = %s', [ExtractFilePath(m_cache.aebSampleText[1].Text)]));
			AddOutputText(Format('  Extension = %s', [ExtractFileExt(m_cache.aebSampleText[1].Text)]));
			AddOutputText(Format('  Filename = %s', [ExtractFileName(m_cache.aebSampleText[1].Text)]));

			nFile := LastDelimiter(PathDelim + DriveDelim, m_cache.aebSampleText[1].Text);
			AddOutputText(Format('  File (raw) = %s', [Copy(
				m_cache.aebSampleText[1].Text,
				nFile + 1,
				LastDelimiter('.' + PathDelim + DriveDelim, m_cache.aebSampleText[1].Text) - nFile - 1)]));
			end;
		end;
end;

procedure TfrmSampleApplication.PerformAction_Hardware();
var
	strTmp: String;
	nTmp: Integer;
	aszPorts: TStringList;
begin
	// Hardware: Perform the action
	case TCategoryRegistry(m_nActionCurrent) of
		eRegistryAction_GetString:
			begin
			aszPorts := TStringList.Create();
			EnumComPorts(aszPorts);
			if (aszPorts.Count > 0) then
				begin
				strTmp := 'Serial ports: ';
				for nTmp:= 0 to (aszPorts.Count - 1) do
					begin
					if (nTmp <> (aszPorts.Count - 1)) then
						strTmp := (strTmp + aszPorts[nTmp] + ', ')
					else
						strTmp := (strTmp + aszPorts[nTmp]);

					end;
				end
			else
				strTmp := 'Serial ports: (none)';

			AddOutputText(strTmp);
			aszPorts.Free();
			end;
		end;
end;

procedure TfrmSampleApplication.PerformAction_Registry();
var
	hRootKey: HKEY;
	strRegValue: String;
	dwRegValue, dwBufferSize: DWORD;
	pRegBuffer: Pointer;
	listTmp: TStringList;
	nTmp: Integer;
begin
	// Registry: Perform the action

	// Get the root key (all registry actions require this)
	hRootKey := GetRootKey(m_cache.aebSampleText[1].Text);
	if (hRootKey = 0) then
		begin
		AddOutputText(Format('"%s" is not a valid registry key', [m_cache.aebSampleText[1].Text]));
		Exit;
		end;

	case TCategoryRegistry(m_nActionCurrent) of
		eRegistryAction_GetString:
			begin
			if (RegGetString(hRootKey, m_cache.aebSampleText[2].Text, strRegValue)) then
				AddOutputText(Format('REG_SZ value = %s', [strRegValue]))
			else
				AddOutputText('Failed to read registry value');
			end;

		eRegistryAction_GetMultiString:
			begin
			listTmp := TStringList.Create();
			if (RegGetMultiString(hRootKey, m_cache.aebSampleText[2].Text, listTmp)) then
				begin
				if (listTmp.Count > 0) then
					begin
					AddOutputText('REG_MULTI_SZ value has sub-strings:');
					for nTmp:=0 to (listTmp.Count - 1) do
						AddOutputText(Format('    %s', [listTmp[nTmp]]));
					end
				else
					AddOutputText('REG_MULTI_SZ value is empty');
				end
			else
				AddOutputText('Failed to read registry value');

			listTmp.Free();
			end;

		eRegistryAction_GetExpandString:
			begin
			if (RegGetExpandString(hRootKey, m_cache.aebSampleText[2].Text, strRegValue)) then
				begin
				// REG_EXPAND_SZ values have unexpended environment variables (eg. "%PATH%)
				AddOutputText(Format('REG_EXPAND_SZ value = %s', [strRegValue]));
				AddOutputText(Format('    [ This expands to %s ]', [
					ReplaceEnvironmentVariables(strRegValue)]));
				end
			else
				AddOutputText('Failed to read registry value');
			end;

		eRegistryAction_GetBinary:
			begin
			if (RegGetBinaryAsPointer(hRootKey, m_cache.aebSampleText[2].Text, pRegBuffer, dwBufferSize)) then
				begin
				if (dwBufferSize > 0) then
					begin
					AddOutputText('REG_BINARY value has the value:');
					AddOutputText(Format('    %s', [ConvertRawBuffer(pRegBuffer, dwBufferSize, ' ')]))
					end
				else
					AddOutputText('REG_BINARY value is empty');

				// Remember to free memory allocated in "RegGetBinaryAsPointer"
				FreeMem(pRegBuffer);
				end
			else
				AddOutputText('Failed to read registry value');
			end;

		eRegistryAction_GetDWORD:
			begin
			if (RegGetDWORD(hRootKey, m_cache.aebSampleText[2].Text, dwRegValue)) then
				AddOutputText(Format('REG_DWORD value = %d (0x%8.8x)', [dwRegValue, dwRegValue]))
			else
				AddOutputText('Failed to read registry value');
			end;
		end;
end;

procedure TfrmSampleApplication.PerformAction_Strings();
var
	nTmp, nValue: Integer;
	bTmp: Boolean;
	listTmp: TStringList;
begin
	// Strings: Perform the action
	case TCategoryStrings(m_nActionCurrent) of
		eStringsAction_IsNumber:
			begin
			for nTmp:=1 to 2 do
				begin
				if (IsNumber(m_cache.aebSampleText[nTmp].Text)) then
					AddOutputText(Format('"%s" is a number', [m_cache.aebSampleText[nTmp].Text]))
				else
					AddOutputText(Format('"%s" is NOT a number', [m_cache.aebSampleText[nTmp].Text]));
				end;
			end;

		eStringsAction_TryStringToInteger:
			begin
			for nTmp:=1 to 2 do
				begin
				if (TryStrToInt(m_cache.aebSampleText[nTmp].Text, nValue)) then
					AddOutputText(Format('"%s" has the value %d', [
						m_cache.aebSampleText[nTmp].Text, nValue]))
				else
					AddOutputText(Format('"%s" not converted', [m_cache.aebSampleText[nTmp].Text]));
				end;
			end;

		eStringsAction_ExtractNumber:
			begin
			bTmp := False;
			if (TryStrToInt(m_cache.aebSampleText[2].Text, nValue)) then
				begin
				if (nValue > Low(BYTE)) and (nValue < High(BYTE)) then
					bTmp := True;
				end;

			if (bTmp) then
				AddOutputText(Format('"%s" extracts to %s', [
					m_cache.aebSampleText[1].Text,
					ExtractNumber(m_cache.aebSampleText[1].Text, nValue)]))
			else
				AddOutputText(Format('Option "%s" is invalid or out-of-range', [
					m_cache.aebSampleText[2].Text]));
			end;

		eStringsAction_ConvertThousands:
			begin
			for nTmp:=1 to 2 do
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[nTmp].Text,
					ConvertNumberWithThousands(m_cache.aebSampleText[nTmp].Text)]));
			end;

		eStringsAction_ConvertSpaces:
			begin
			for nTmp:=1 to 2 do
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[nTmp].Text,
					ConvertNumberWithSpaces(StrToInt(m_cache.aebSampleText[nTmp].Text))]));
			end;

		eStringsAction_ConvertSignificantFigures:
			AddOutputText(Format('"%s" to %s significant figure is %s', [
				m_cache.aebSampleText[1].Text,
				m_cache.aebSampleText[2].Text,
				ConvertNumberWithSigFigures(
					StrToFloat(m_cache.aebSampleText[1].Text),
					StrToInt(m_cache.aebSampleText[2].Text))]));

		eStringsAction_TimeStringFromSeconds:
			AddOutputText(Format('Computer has been powered-up for %s', [
				GetTimeStringFromSeconds(GetTickCount() div 1000)]));

		eStringsAction_GetIsoDateTime:
			AddOutputText(Format('The ISO-8601 format for now is %s', [GetIsoDateTimeString(Now())]));

		eStringsAction_ConvertTitleCase:
			begin
			for nTmp:=1 to 2 do
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[nTmp].Text,
					ConvertTitleCase(m_cache.aebSampleText[nTmp].Text)]));
			end;

		eStringsAction_InsertFormattingCharacter:
			begin
			if (TryStrToInt(m_cache.aebSampleText[3].Text, nValue)) then
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[1].Text,
					InsertFormattingChar(
						m_cache.aebSampleText[1].Text,
						m_cache.aebSampleText[2].Text[1],
						nValue)]))
			else
				AddOutputText(Format('"%s" is not a valid number', [m_cache.aebSampleText[3].Text]));
			end;

		eStringsAction_ParseString:
			begin
			listTmp := TStringList.Create();
			ParseString(m_cache.aebSampleText[1].Text, m_cache.aebSampleText[2].Text, listTmp);
			if (listTmp.Count > 1) then
				begin
				AddOutputText(Format('"%s" has sub-strings:', [m_cache.aebSampleText[1].Text]));
				for nTmp:=0 to (listTmp.Count - 1) do
					AddOutputText(Format('    %s', [listTmp[nTmp]]));
				end
			else
				AddOutputText(Format('"%s" has NO sub-strings using delimiter "%s"', [
					m_cache.aebSampleText[1].Text,
					m_cache.aebSampleText[2].Text]));

			listTmp.Free();
			end;

		eStringsAction_GetRandomString:
			begin
			if (TryStrToInt(m_cache.aebSampleText[1].Text, nValue)) then
				begin
				AddOutputText(Format('%s   (lowercase letters)', [GetRandomString(nValue, True)]));
				AddOutputText(Format('%s   (any character)', [GetRandomString(nValue, False)]));
				end
			else
				AddOutputText(Format('"%s" is not a valid number', [m_cache.aebSampleText[3].Text]));
			end;
		end;
end;

procedure TfrmSampleApplication.PerformAction_Controls();
var
	nTmp, nValue, nLines, nVerticalSize: Integer;
	txtSize: TSize;
	strMsg: String;
	fImage: File of BYTE;
begin
	// Controls: Perform the action
	case TCategoryControls(m_nActionCurrent) of
		eControls_DumpToFile:
			begin
			// Dump one of the sample controls (in "gbSampleControls") to disk
			nValue := Random(gbSampleControls.ControlCount);
			DumpToFile(gbSampleControls.Controls[nValue], m_cache.aebSampleText[1].Text);
			AddOutputText(Format('"%s" (child %d) was dumped to disk', [
				gbSampleControls.Controls[nValue].Name, nValue]));
			end;

		eControls_WinControlSize:
			begin
			// TEdit padding
			nValue := (ebEditBox.Width - ebEditBox.ClientWidth);
			txtSize := GetWinControlPixelSize(ebEditBox, m_cache.aebSampleText[1].Text);
			ebEditBox.Text := m_cache.aebSampleText[1].Text;
			ebEditBox.Width := Min(txtSize.cx + (2 * nValue), m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" pixel size is (x:%d, y:%d) in %s (%s)', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				ebEditBox.ClassName, ebEditBox.Name]));

			// TComboBox padding depends on various registry settings such as:
			//		HKCU\Control Panel\Desktop\WindowMetricsScroll\Width
			// We simplify the issue here by using the fixed constant "24"
			cbComboBox.Clear();
			cbComboBox.Text := m_cache.aebSampleText[1].Text;
			cbComboBox.Width := Min(txtSize.cx + 24, m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" pixel size is (x:%d, y:%d) in %s (%s)', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				cbComboBox.ClassName, cbComboBox.Name]));

			// TStaticText padding
			nValue := (lblStatic.Width - lblStatic.ClientWidth);
			txtSize := GetWinControlPixelSize(lblStatic, m_cache.aebSampleText[1].Text);
			lblStatic.Caption := m_cache.aebSampleText[1].Text;
			lblStatic.Width := Min(txtSize.cx + (2 * nValue), m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" pixel size is (x:%d, y:%d) in %s (%s)', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				lblStatic.ClassName, lblStatic.Name]));
			end;

		eControls_GraphicControlSize:
			begin
			// No padding is required for TLabel controls
			txtSize := GetGraphicControlPixelSize(lblSingleLine, m_cache.aebSampleText[1].Text);
			lblSingleLine.Caption := m_cache.aebSampleText[1].Text;
			lblSingleLine.Width := Min(txtSize.cx, m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" pixel size id (x:%d, y:%d) in %s (%s)', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				lblSingleLine.ClassName, lblSingleLine.Name]));
			end;

		eControls_MultiLineLabel:
			begin
			if (TryStrToInt(m_cache.aebSampleText[2].Text, nValue)) then
				begin
				strMsg := '';
				for nTmp:=1 to nValue do
					begin
					if (nTmp < nValue) then
						strMsg := Format('%s%.2d %s ', [strMsg, nTmp, m_cache.aebSampleText[1].Text])
					else
						strMsg := Format('%s%.2d %s', [strMsg, nTmp, m_cache.aebSampleText[1].Text]);
					end;

				ConfigureMultiLineLabel(lblMultiLine, strMsg, 15, nLines, nVerticalSize);
				lblMultiLine.Height := (nLines * nVerticalSize);
				lblMultiLine.Caption := strMsg;
				AddOutputText(Format('Message used %d lines. Each line has a height of %d pixels.', [
					nLines, nVerticalSize]));
				end
			else
				AddOutputText(Format('"%s" is not a valid number', [m_cache.aebSampleText[2].Text]));
			end;

		eControls_SaveScreenshot:
			begin
			if (TryStrToInt(m_cache.aebSampleText[2].Text, nValue)) then
				begin
				SaveScreenshot(@Self, m_cache.aebSampleText[1].Text, nValue);
				AssignFile(fImage, m_cache.aebSampleText[1].Text);
				Reset(fImage);
				try
					AddOutputText(Format('Screen saved to file (size is %s bytes)', [
						ConvertNumberWithThousands(FileSize(fImage))]));
				finally
					CloseFile(fImage);
				end;
				end
			else
				AddOutputText(Format('"%s" is not a valid number', [m_cache.aebSampleText[2].Text]));
			end;

		eControls_SelectAllText:
			SelectAllText(m_cache.aebSampleText[1]);

		eControls_GetListVisibleRows:
			AddOutputText(Format('There are %d rows visible in this TListBox', [
				GetListVisibleRows(listOutput)]));
		end;
end;

procedure TfrmSampleApplication.AddOutputText(const cstrTxt: String);
begin
	// Add text to list
	listOutput.Items.Add(cstrTxt);

	// Select last item (this will automatically scroll down, if required)
	listOutput.Selected[listOutput.Count - 1] := True;
end;
// Private functions: Start

end.
