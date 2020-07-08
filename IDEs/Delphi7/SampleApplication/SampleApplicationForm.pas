unit SampleApplicationForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Controls, ExtCtrls, Forms, SysUtils, StdCtrls,
  CoreFormClasses, CoreTypes, FormUtils, SystemUtils;

type
  // Enumerations
  TCategory = (
	eCategoryNone,
	eCategoryWindows,
	eCategoryRegistry,
	eCategoryStrings,
	eCategoryControls
  );

  CACHE_CONTROL_UPDATES = record
	// Cache to assist control updates
	alblSampleTitle: array[1..3] of TLabel;
	aebSampleText: array[1..3] of TEdit;
	astrSampleTitleDefault, astrSampleTextDefault: array[1..3] of String;

	// Edit box and output list size (which can change depending on the selections made)
	nEditBoxWidthInitial, nEditBoxWidthMax: Integer;
	nOutputWidthInitial, nOutputWidthMax: Integer;

	// Maximum size to set sample control
	nMaxControlWidth: Integer;

	// Whether to display some advanced explanation text
	strLastExplanationText: String;
  end;

  CONTROL_UPDATES = record
	// Updates to the user interface
	astrSampleTitle, astrSampleText: array [1..3] of String;
	strExplanationText: String;
  end;

  TfrmSampleApplication = class(TGeneralBaseForm)
	gbSettings: TGroupBox;
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
	btnProcess: TButton;
	lblExplanationText: TStaticText;
	listOutput: TListBox;

	gbSampleControls: TGroupBox;
	lblSampleControlsA: TLabel;
	lblSampleControlsB: TLabel;
	ebSampleControlC: TEdit;
	cbSampleControlD: TComboBox;
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
	procedure PopulateActions_Registry();
	procedure PopulateActions_Strings();
	procedure PopulateActions_Controls();

	procedure UpdateControls(updates: CONTROL_UPDATES);
	procedure UpdateControls_Windows();
	procedure UpdateControls_Registry();
	procedure UpdateControls_Strings();
	procedure UpdateControls_Controls();

	procedure PerformAction_Windows();
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
  Graphics, Math;

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

  TCategoryRegistry = (
	eRegistryAction_GetString
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
	InitialiseSystemUtils();
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

		eCategoryRegistry:
			PerformAction_Registry();

		eCategoryStrings:
			PerformAction_Strings();

		eCategoryControls:
			PerformAction_Controls();
		end;
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

	// Maximum size to set sample controls
	m_cache.nMaxControlWidth := lblSampleControlsA.Width;

	// Last explanation text
	m_cache.strLastExplanationText := 'z';
end;

procedure TfrmSampleApplication.PopulateCategories();
begin
	// Add categories
	ddlCategory.Items.Clear();
	ddlCategory.Items.AddObject('Windows', TObject(eCategoryWindows));
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

		eCategoryRegistry:
			PopulateActions_Registry();

		eCategoryStrings:
			PopulateActions_Strings();

		eCategoryControls:
			PopulateActions_Controls();
		end;

	// Set the width of the output window and controls
	if (	(m_eCategoryCurrent = eCategoryWindows) or
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
		lblSampleControlsA.Caption := Format('There are %d child controls in this group', [
			gbSampleControls.ControlCount]);
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
	ddlAction.Items.AddObject('Get process threads', TObject(eWindowsAction_GetProcessThreads));
	ddlAction.Items.AddObject('Get system threads', TObject(eWindowsAction_GetSystemThreads));
	ddlAction.Items.AddObject('Find windows by title', TObject(eWindowsAction_FindWindowByTitle));
	ddlAction.Items.AddObject('Get disk free space (in GB)', TObject(eWindowsAction_GetDiskSpace));
	ddlAction.Items.AddObject('Get drive filesystem', TObject(eWindowsAction_GetDriveFileSystem));
	ddlAction.Items.AddObject('Get system drives', TObject(eWindowsAction_GetSystemDrives));
	ddlAction.Items.AddObject('Check drive is valud', TObject(eWindowsAction_CheckDriveIsValid));
	ddlAction.Items.AddObject('Save output to clipboard', TObject(eWindowsAction_SaveToClipboard));
{$IFDEF DBG}
	ddlAction.Items.AddObject('Break if Scroll Lock is pressed', TObject(eWindowsAction_BreakIfScrollLock));
{$ENDIF}
end;

procedure TfrmSampleApplication.PopulateActions_Registry();
begin
	// Registry: Populate actions for this category
	ddlAction.Items.AddObject('Read string value (REG_SZ)', TObject(eRegistryAction_GetString));
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
			m_cache.strLastExplanationText := updates.strExplanationText;
			for nSample:=1 to 3 do
				m_cache.aebSampleText[nSample].Width := m_cache.nEditBoxWidthInitial;

			lblExplanationText.Caption := updates.strExplanationText;
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
			updates.astrSampleText[1] := 'C:\';
			end;

		eWindowsAction_GetSystemDrives: ;
		eWindowsAction_CheckDriveIsValid:
			begin
			updates.astrSampleTitle[1] := 'Drive 1';
			updates.astrSampleText[1] := 'C';

			updates.astrSampleTitle[2] := 'Drive 2';
			updates.astrSampleText[2] := 'Q';
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
			updates.astrSampleText[1] := 'TWinControl message';
			end;

		eControls_GraphicControlSize:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := 'TGraphicControl message';
			end;

		eControls_MultiLineLabel:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := 'A message for you...';

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
			AddOutputText(Format('The filesystem on %s is %s', [
				ebSample1.Text, GetDriveFileSystem(ebSample1.Text)]));

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
				PAnsiChar('Output saved to the clipboard)'), 'Windows', MB_ICONEXCLAMATION);
			end;

{$IFDEF DBG}
		eWindowsAction_BreakIfScrollLock:
			begin
			if (BreakIfScrollLock()) then
				AddOutputText('Scroll Lock detected while debugging - breakpoint forced!')
			else
				AddOutputText('Scroll Lock not detected or not debugging');
			end;
{$ENDIF}
		end;
end;

procedure TfrmSampleApplication.PerformAction_Registry();
var
	hRootKey: HKEY;
	strRegValue: String;
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
				AddOutputText(Format('Registry value = %s', [strRegValue]))
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
			// Get the padding required to display text in a TEdit
			nValue := (ebSampleControlC.Width - ebSampleControlC.ClientWidth);
			txtSize := GetWinControlPixelSize(ebSampleControlC, m_cache.aebSampleText[1].Text);
			ebSampleControlC.Text := m_cache.aebSampleText[1].Text;
			ebSampleControlC.Width := Min(txtSize.cx + (2 * nValue), m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" has a pixel size of (x:%d, y:%d) in %s', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				ebSampleControlC.Name]));

			// TComboBox padding depends on various registry settings such as:
			//		HKCU\Control Panel\Desktop\WindowMetricsScrollWidth
			// We simplify the issue here by using the fixed constant "24"
			cbSampleControlD.Clear();
			cbSampleControlD.Text := m_cache.aebSampleText[1].Text;
			cbSampleControlD.Width := Min(txtSize.cx + 24, m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" has a pixel size of (x:%d, y:%d) in %s', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				cbSampleControlD.Name]));
			end;

		eControls_GraphicControlSize:
			begin
			// No padding is required for TLabel controls
			txtSize := GetGraphicControlPixelSize(lblSampleControlsB, m_cache.aebSampleText[1].Text);
			lblSampleControlsB.Caption := m_cache.aebSampleText[1].Text;
			lblSampleControlsB.Width := Min(txtSize.cx, m_cache.nMaxControlWidth);
			AddOutputText(Format('"%s" has a pixel size of (x:%d, y:%d) in %s', [
				m_cache.aebSampleText[1].Text,
				txtSize.cx, txtSize.cy,
				lblSampleControlsB.Name]));
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
				AddOutputText(Format('Message required %d lines. Each line has a height of %d pixels.', [
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
