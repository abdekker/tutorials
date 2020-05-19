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
	eCategoryStrings);

  CACHE_CONTROL_UPDATES = record
	// Cache to assist control updates
	alblSampleTitle: array[1..3] of TLabel;
	aebSampleText: array[1..3] of TEdit;
	astrSampleTitleDefault, astrSampleTextDefault: array[1..3] of String;
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

	UpdateTimer: TTimer;

	procedure FormCreate(Sender: TObject);
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
	procedure PopulateActions_Strings();

	procedure UpdateControls(updates: CONTROL_UPDATES);
	procedure UpdateControls_Windows();
	procedure UpdateControls_Strings();

	procedure PerformAction_Windows();
	procedure PerformAction_Strings();

	procedure AddOutputText(const cstrTxt: String);

  public
	{ Public declarations }
  end;

var
  frmSampleApplication: TfrmSampleApplication;

implementation

uses
  Graphics;

const
  // Actions for each category
  ACTION_WINDOWS_IS_64BIT				= 0;	// eCategoryWindows
  ACTION_WINDOWS_IS_WIN10				= 1;
  ACTION_WINDOWS_EXPAND_ENVIRONMENT		= 2;
  ACTION_WINDOWS_IS_PROCESS_RUNNING		= 3;
  ACTION_WINDOWS_GET_PROCESS_THREADS	= 4;
  ACTION_WINDOWS_GET_SYSTEM_THREADS		= 5;
  ACTION_WINDOWS_FIND_WINDOW_BY_TITLE	= 6;
  ACTION_WINDOWS_GET_DISK_SPACE			= 7;
  ACTION_WINDOWS_GET_DRIVE_FILE_SYSTEM	= 8;
  ACTION_WINDOWS_GET_SYSTEM_DRIVES		= 9;
  ACTION_WINDOWS_SAVE_TO_CLIPBOARD		= 10;

  ACTION_STRINGS_IS_NUMBER				= 0;	// eCategoryStrings
  ACTION_STRINGS_TRY_STRING_TO_INT		= 1;
  ACTION_STRINGS_EXTRACT_NUMBER			= 2;
  ACTION_STRINGS_CONVERT_THOUSANDS		= 3;
  ACTION_STRINGS_CONVERT_SPACES			= 4;
  ACTION_STRINGS_CONVERT_SIG_FIGURES	= 5;
  ACTION_STRINGS_TIME_STRING_FROM_SECS	= 6;
  ACTION_STRINGS_GET_ISO_DATETIME		= 7;
  ACTION_STRINGS_CONVERT_TITLE_CASE		= 8;
  ACTION_STRINGS_INSERT_FORMATTING_CHAR	= 9;
  ACTION_STRINGS_PARSE_STRING			= 10;
  ACTION_STRINGS_RANDOM_STRING			= 11;

{$R *.dfm}

procedure TfrmSampleApplication.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_bExiting := False;

	m_eCategoryCurrent := eCategoryNone;
	m_eCategoryLastUpdate := eCategoryNone;

	m_nActionCurrent := -1;
	m_nActionLastUpdate := -1;

	RandSeed := Integer(GetTickCount() mod 223);
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

		eCategoryStrings:
			PerformAction_Strings();
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

			eCategoryStrings:
				UpdateControls_Strings();
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
end;

procedure TfrmSampleApplication.PopulateCategories();
begin
	// Add categories
	ddlCategory.Items.Clear();
	ddlCategory.Items.AddObject('Windows', TObject(eCategoryWindows));
	ddlCategory.Items.AddObject('Strings', TObject(eCategoryStrings));
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

		eCategoryStrings:
			PopulateActions_Strings();
		end;

	ddlAction.DropDownCount := ddlAction.Items.Count;
	ddlAction.ItemIndex := 0;
	m_nActionLastUpdate := -1;
end;

procedure TfrmSampleApplication.PopulateActions_Windows();
begin
	// Windows: Populate actions for this category
	ddlAction.Items.AddObject('Is Windows 64-bit ?', TObject(ACTION_WINDOWS_IS_64BIT));
	ddlAction.Items.AddObject('Running Windows 10 ?', TObject(ACTION_WINDOWS_IS_WIN10));
	ddlAction.Items.AddObject('Expand enviroment variable', TObject(ACTION_WINDOWS_EXPAND_ENVIRONMENT));
	ddlAction.Items.AddObject('Is process running ?', TObject(ACTION_WINDOWS_IS_PROCESS_RUNNING));
	ddlAction.Items.AddObject('Get process threads', TObject(ACTION_WINDOWS_GET_PROCESS_THREADS));
	ddlAction.Items.AddObject('Get system threads', TObject(ACTION_WINDOWS_GET_SYSTEM_THREADS));
	ddlAction.Items.AddObject('Find windows by title', TObject(ACTION_WINDOWS_FIND_WINDOW_BY_TITLE));
	ddlAction.Items.AddObject('Get disk free space (in GB)', TObject(ACTION_WINDOWS_GET_DISK_SPACE));
	ddlAction.Items.AddObject('Get drive filesystem', TObject(ACTION_WINDOWS_GET_DRIVE_FILE_SYSTEM));
	ddlAction.Items.AddObject('Get system drives', TObject(ACTION_WINDOWS_GET_SYSTEM_DRIVES));
	ddlAction.Items.AddObject('Save output to clipboard', TObject(ACTION_WINDOWS_SAVE_TO_CLIPBOARD));
end;

procedure TfrmSampleApplication.PopulateActions_Strings();
begin
	// Strings: Populate actions for this category
	ddlAction.Items.AddObject('Is this a number ?', TObject(ACTION_STRINGS_IS_NUMBER));
	ddlAction.Items.AddObject('Try convert to integer', TObject(ACTION_STRINGS_TRY_STRING_TO_INT));
	ddlAction.Items.AddObject('Extract number', TObject(ACTION_STRINGS_EXTRACT_NUMBER));
	ddlAction.Items.AddObject('Convert with thousand commas', TObject(ACTION_STRINGS_CONVERT_THOUSANDS));
	ddlAction.Items.AddObject('Convert with thousand spaces', TObject(ACTION_STRINGS_CONVERT_SPACES));
	ddlAction.Items.AddObject('Format float with specified significant figures',
		TObject(ACTION_STRINGS_CONVERT_SIG_FIGURES));
	ddlAction.Items.AddObject('Time string from seconds', TObject(ACTION_STRINGS_TIME_STRING_FROM_SECS));
	ddlAction.Items.AddObject('Get ISO 8601 date/time string', TObject(ACTION_STRINGS_GET_ISO_DATETIME));
	ddlAction.Items.AddObject('Convert title case', TObject(ACTION_STRINGS_CONVERT_TITLE_CASE));
	ddlAction.Items.AddObject('Insert formatting char', TObject(ACTION_STRINGS_INSERT_FORMATTING_CHAR));
	ddlAction.Items.AddObject('Parse string', TObject(ACTION_STRINGS_PARSE_STRING));
	ddlAction.Items.AddObject('Generate random string', TObject(ACTION_STRINGS_RANDOM_STRING));
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
		lblExplanationText.Caption := updates.strExplanationText;
		lblExplanationText.Visible := True;
		end
	else
		lblExplanationText.Visible := False;
end;

procedure TfrmSampleApplication.UpdateControls_Windows();
var
	updates: CONTROL_UPDATES;
begin
	// Windows: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case ddlAction.ItemIndex of
		ACTION_WINDOWS_IS_64BIT: ;
		ACTION_WINDOWS_IS_WIN10: ;
		ACTION_WINDOWS_EXPAND_ENVIRONMENT:
			begin
			updates.astrSampleTitle[1] := 'Variable';
			updates.astrSampleText[1] := 'APPDATA';
			end;

		ACTION_WINDOWS_IS_PROCESS_RUNNING:
			begin
			updates.astrSampleTitle[1] := 'Process name';
			updates.astrSampleText[1] := ExtractFileName(Application.ExeName);
			end;

		ACTION_WINDOWS_GET_PROCESS_THREADS: ;
		ACTION_WINDOWS_GET_SYSTEM_THREADS: ;
		ACTION_WINDOWS_FIND_WINDOW_BY_TITLE:
			begin
			updates.astrSampleTitle[1] := 'Window title';
			updates.astrSampleText[1] := 'Delphi 7';
			end;

		ACTION_WINDOWS_GET_DISK_SPACE:
			begin
			updates.astrSampleTitle[1] := 'Drive';
			updates.astrSampleText[1] := 'C:';
			end;

		ACTION_WINDOWS_GET_DRIVE_FILE_SYSTEM:
			begin
			updates.astrSampleTitle[1] := 'Drive';
			updates.astrSampleText[1] := 'C:\';
			end;

		ACTION_WINDOWS_GET_SYSTEM_DRIVES: ;
		ACTION_WINDOWS_SAVE_TO_CLIPBOARD: ;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.UpdateControls_Strings();
var
	updates: CONTROL_UPDATES;
begin
	// Strings: Update controls based on the selected action
	ZeroMemory(@updates, SizeOf(CONTROL_UPDATES));
	case ddlAction.ItemIndex of
		ACTION_STRINGS_IS_NUMBER:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1569';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '1.569a';
			end;

		ACTION_STRINGS_TRY_STRING_TO_INT:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1569';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '1.569a';
			end;

		ACTION_STRINGS_EXTRACT_NUMBER:
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

		ACTION_STRINGS_CONVERT_THOUSANDS:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1234567.89';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '-34567';
			end;

		ACTION_STRINGS_CONVERT_SPACES:
			begin
			updates.astrSampleTitle[1] := 'Number 1';
			updates.astrSampleText[1] := '1234567';

			updates.astrSampleTitle[2] := 'Number 2';
			updates.astrSampleText[2] := '34567';
			end;

		ACTION_STRINGS_CONVERT_SIG_FIGURES:
			begin
			updates.astrSampleTitle[1] := 'Number';
			updates.astrSampleText[1] := '13.456839';

			updates.astrSampleTitle[2] := 'Significant figures';
			updates.astrSampleText[2] := '6';
			end;

		ACTION_STRINGS_TIME_STRING_FROM_SECS: ;
		ACTION_STRINGS_GET_ISO_DATETIME: ;

		ACTION_STRINGS_CONVERT_TITLE_CASE:
			begin
			updates.astrSampleTitle[1] := 'String 1';
			updates.astrSampleText[1] := 'the cat SAT ON THE MAT';

			updates.astrSampleTitle[2] := 'String 2';
			updates.astrSampleText[2] := 'FRED LOVES ICE CREAM';
			end;

		ACTION_STRINGS_INSERT_FORMATTING_CHAR:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := '0052C2539000';

			updates.astrSampleTitle[2] := 'Formatting character';
			updates.astrSampleText[2] := '-';

			updates.astrSampleTitle[3] := 'Every N';
			updates.astrSampleText[3] := '2';
			end;

		ACTION_STRINGS_PARSE_STRING:
			begin
			updates.astrSampleTitle[1] := 'String';
			updates.astrSampleText[1] := '192.168.2.108';

			updates.astrSampleTitle[2] := 'Parse string';
			updates.astrSampleText[2] := '.';
			end;

		ACTION_STRINGS_RANDOM_STRING:
			begin
			updates.astrSampleTitle[1] := 'Length';
			updates.astrSampleText[1] := '12';
			end;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.PerformAction_Windows();
var
	strTmp: String;
	nTmp: Integer;
	hWndTmp: HWND;
	fTotalGB, fFreeGB: Single;
begin
	// Windows: Perform the action
	case m_nActionCurrent of
		ACTION_WINDOWS_IS_64BIT:
			begin
			if (IsWindows64Bit()) then
				AddOutputText('Windows is 64-bit')
			else
				AddOutputText('Windows is NOT 64-bit');
			end;

		ACTION_WINDOWS_IS_WIN10:
			begin
			if (IsWindows10()) then
				AddOutputText('Running Windows 10')
			else
				AddOutputText('NOT Windows 10...');
			end;

		ACTION_WINDOWS_EXPAND_ENVIRONMENT:
			begin
			strTmp := ('%' + ebSample1.Text + '%');
			AddOutputText(Format('%s  =  "%s"', [strTmp, ExpandEnvironment(strTmp)]));
			end;

		ACTION_WINDOWS_IS_PROCESS_RUNNING:
			begin
			if (IsProcessRunning(ebSample1.Text)) then
				AddOutputText('Process is running')
			else
				AddOutputText('Process is NOT running');
			end;

		ACTION_WINDOWS_GET_PROCESS_THREADS:
			AddOutputText(Format('Process threads = %d (process ID = %d)', [
				GetProcessThreadCount(GetCurrentProcessId()),
				GetCurrentProcessId()]));

		ACTION_WINDOWS_GET_SYSTEM_THREADS:
			AddOutputText(Format('Total system threads = %d', [GetSystemThreadCount()]));

		ACTION_WINDOWS_FIND_WINDOW_BY_TITLE:
			begin
			hWndTmp := FindWindowByTitle(Application.Handle, ebSample1.Text);
			if (hWndTmp <> 0) then
				AddOutputText(Format('Window "%s" found with handle %d', [ebSample1.Text, hWndTmp]))
			else
				AddOutputText(Format('Window "%s" NOT found', [ebSample1.Text]));
			end;

		ACTION_WINDOWS_GET_DISK_SPACE:
			begin
			GetDiskSpaceGB(ebSample1.Text, fTotalGB, fFreeGB);
			if (fTotalGB > MIN_SINGLE) then
				AddOutputText(Format('%s is a %.1fGB disk with %.1fGB free space', [
					ebSample1.Text, fTotalGB, fFreeGB]))
			else
				AddOutputText(Format('%s is not a valid drive', [ebSample1.Text]));
			end;

		ACTION_WINDOWS_GET_DRIVE_FILE_SYSTEM:
			AddOutputText(Format('The filesystem on %s is %s', [
				ebSample1.Text, GetDriveFileSystem(ebSample1.Text)]));

		ACTION_WINDOWS_GET_SYSTEM_DRIVES:
			AddOutputText(Format('System drives are %s', [GetSystemDrives()]));

		ACTION_WINDOWS_SAVE_TO_CLIPBOARD:
			begin
			strTmp := '';
			for nTmp:=0 to (listOutput.Items.Count - 1) do
				strTmp := (strTmp + listOutput.Items[nTmp] + #13);

			SaveToClipboard(strTmp);
			Application.MessageBox(
				PAnsiChar('Output saved to the clipboard)'), 'Windows', MB_ICONEXCLAMATION);
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
	case m_nActionCurrent of
		ACTION_STRINGS_IS_NUMBER:
			begin
			for nTmp:=1 to 2 do
				begin
				if (IsNumber(m_cache.aebSampleText[nTmp].Text)) then
					AddOutputText(Format('"%s" is a number', [m_cache.aebSampleText[nTmp].Text]))
				else
					AddOutputText(Format('"%s" is NOT a number', [m_cache.aebSampleText[nTmp].Text]));
				end;
			end;

		ACTION_STRINGS_TRY_STRING_TO_INT:
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

		ACTION_STRINGS_EXTRACT_NUMBER:
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

		ACTION_STRINGS_CONVERT_THOUSANDS:
			begin
			for nTmp:=1 to 2 do
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[nTmp].Text,
					ConvertNumberWithThousands(m_cache.aebSampleText[nTmp].Text)]));
			end;

		ACTION_STRINGS_CONVERT_SPACES:
			begin
			for nTmp:=1 to 2 do
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[nTmp].Text,
					ConvertNumberWithSpaces(StrToInt(m_cache.aebSampleText[nTmp].Text))]));
			end;

		ACTION_STRINGS_CONVERT_SIG_FIGURES:
			AddOutputText(Format('"%s" to %s significant figure is %s', [
				m_cache.aebSampleText[1].Text,
				m_cache.aebSampleText[2].Text,
				ConvertNumberWithSigFigures(
					StrToFloat(m_cache.aebSampleText[1].Text),
					StrToInt(m_cache.aebSampleText[2].Text))]));

		ACTION_STRINGS_TIME_STRING_FROM_SECS:
			AddOutputText(Format('Computer has been powered-up for %s', [
				GetTimeStringFromSeconds(GetTickCount() div 1000)]));

		ACTION_STRINGS_GET_ISO_DATETIME:
			AddOutputText(Format('The ISO-8601 format for now is %s', [GetIsoDateTimeString(Now())]));

		ACTION_STRINGS_CONVERT_TITLE_CASE:
			begin
			for nTmp:=1 to 2 do
				AddOutputText(Format('"%s" converts to %s', [
					m_cache.aebSampleText[nTmp].Text,
					ConvertTitleCase(m_cache.aebSampleText[nTmp].Text)]));
			end;

		ACTION_STRINGS_INSERT_FORMATTING_CHAR:
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

		ACTION_STRINGS_PARSE_STRING:
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

		ACTION_STRINGS_RANDOM_STRING:
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

procedure TfrmSampleApplication.AddOutputText(const cstrTxt: String);
begin
	// Add text to list
	listOutput.Items.Add(cstrTxt);

	// Select last item (this will automatically scroll down, if required)
	listOutput.Selected[listOutput.Count - 1] := True;
end;
// Private functions: Start

end.
