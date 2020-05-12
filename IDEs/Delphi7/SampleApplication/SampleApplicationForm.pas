unit SampleApplicationForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Controls, Forms, SysUtils, StdCtrls,
  CoreFormClasses, FormUtils, SystemUtils, ExtCtrls;

type
  // Enumerations
  TCategory = (
	eCategoryNone,
	eCategoryWindows);

  CONTROL_UPDATES = record
	// Updates to the user interface
	bEnableSampleText: Boolean;
  end;

  TfrmSampleApplication = class(TGeneralBaseForm)
	gbSettings: TGroupBox;
	btnExit: TButton;
	btnClearOutput: TButton;

	lblCategory: TLabel;
	ddlCategory: TComboBox;
	lblAction: TLabel;
	ddlAction: TComboBox;
	lblSample: TLabel;
	ebSample: TEdit;
	btnProcess: TButton;
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

	procedure PopulateCategories();

	procedure PopulateActions();
	procedure PopulateActions_Windows();

	procedure SetStartingText_Windows();

	procedure UpdateControls(updates: CONTROL_UPDATES);
	procedure UpdateControls_Windows();

	procedure PerformAction_Windows();

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
  ACTIONS_WINDOWS_IS_64BIT				= 0;	// eCategoryWindows
  ACTIONS_WINDOWS_IS_WIN10				= 1;
  ACTIONS_WINDOWS_EXPAND_ENVIRONMENT	= 2;
  ACTIONS_WINDOWS_IS_PROCESS_RUNNING	= 3;
  ACTIONS_WINDOWS_GET_PROCESS_THREADS	= 4;

{$R *.dfm}

procedure TfrmSampleApplication.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_bExiting := False;

	m_eCategoryCurrent := eCategoryNone;
	m_eCategoryLastUpdate := eCategoryNone;

	m_nActionCurrent := -1;
	m_nActionLastUpdate := -1;
end;

procedure TfrmSampleApplication.FormShow(Sender: TObject);
begin
	// Show form
	SetChildComboHandlers(gbSettings);
	PopulateCategories();

	UpdateTimer.Enabled := True;

	//GetListVisibleRows(listOutput);
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
	eCategory := TCategory(ddlCategory.ItemIndex);
	if (eCategory <> m_eCategoryCurrent) then
		begin
		m_eCategoryCurrent := eCategory;
		PopulateActions();
		end;
end;

procedure TfrmSampleApplication.ddlActionClick(Sender: TObject);
begin
	// If appropriate, put some starting text into the sample edit box
	case m_eCategoryCurrent of
		eCategoryWindows:
			SetStartingText_Windows();
		end;
end;

procedure TfrmSampleApplication.btnProcessClick(Sender: TObject);
begin
	// Perform some action...
	m_nActionCurrent := ddlAction.ItemIndex;
	case m_eCategoryCurrent of
		eCategoryWindows:
			PerformAction_Windows();
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
	eCategory := TCategory(ddlCategory.ItemIndex);
	nAction := ddlAction.ItemIndex;
	if (	(eCategory <> m_eCategoryLastUpdate) or
			(nAction <> m_nActionLastUpdate)) then
		begin
		m_eCategoryLastUpdate := eCategory;
		m_nActionLastUpdate := nAction;
		case m_eCategoryCurrent of
			eCategoryWindows:
				UpdateControls_Windows();
			end;
		end;

	// Restart the timer
	UpdateTimer.Enabled := True;
end;

// Private functions: Start
procedure TfrmSampleApplication.PopulateCategories();
begin
	// Add categories
	ddlCategory.Items.Clear();
	ddlCategory.Items.AddObject('Windows', TObject(eCategoryWindows));
	ddlCategory.ItemIndex := 0;
	m_eCategoryCurrent := eCategoryWindows;
	PopulateActions();
end;

procedure TfrmSampleApplication.PopulateActions();
begin
	// Population the Actions based on the category
	ddlAction.Items.Clear();
	case m_eCategoryCurrent of
		eCategoryWindows:
			PopulateActions_Windows();
		end;

	ddlAction.ItemIndex := 0;
end;

procedure TfrmSampleApplication.PopulateActions_Windows();
begin
	// Windows: Populate actions for the Windows category
	ddlAction.Items.AddObject('Is Windows 64-bit ?', TObject(ACTIONS_WINDOWS_IS_64BIT));
	ddlAction.Items.AddObject('Running Windows 10 ?', TObject(ACTIONS_WINDOWS_IS_WIN10));
	ddlAction.Items.AddObject('Expand enviroment variable', TObject(ACTIONS_WINDOWS_EXPAND_ENVIRONMENT));
	ddlAction.Items.AddObject('Is process running ?', TObject(ACTIONS_WINDOWS_IS_PROCESS_RUNNING));
	ddlAction.Items.AddObject('Get process threads', TObject(ACTIONS_WINDOWS_GET_PROCESS_THREADS));
end;

procedure TfrmSampleApplication.SetStartingText_Windows();
begin
	// Windows: Set some starting text
	case ddlAction.ItemIndex of
		ACTIONS_WINDOWS_IS_64BIT: ;
		ACTIONS_WINDOWS_IS_WIN10: ;
		ACTIONS_WINDOWS_EXPAND_ENVIRONMENT:
			ebSample.Text := 'APPDATA';
		ACTIONS_WINDOWS_IS_PROCESS_RUNNING:
			ebSample.Text := ExtractFileName(Application.ExeName);
		ACTIONS_WINDOWS_GET_PROCESS_THREADS: ;
		end;
end;

procedure TfrmSampleApplication.UpdateControls(updates: CONTROL_UPDATES);
begin
	// Update controls
	lblSample.Enabled := updates.bEnableSampleText;
	ebSample.Enabled := updates.bEnableSampleText;
	if (ebSample.Enabled) then
		ebSample.Color := clInfoBk
	else
		ebSample.Color := clSkyBlue;
end;

procedure TfrmSampleApplication.UpdateControls_Windows();
var
	updates: CONTROL_UPDATES;
begin
	// Windows: Update controls based on the selected action
	updates.bEnableSampleText := False;
	case ddlAction.ItemIndex of
		ACTIONS_WINDOWS_IS_64BIT: ;
		ACTIONS_WINDOWS_IS_WIN10: ;
		ACTIONS_WINDOWS_EXPAND_ENVIRONMENT:
			updates.bEnableSampleText := True;
		ACTIONS_WINDOWS_IS_PROCESS_RUNNING:
			updates.bEnableSampleText := True;
		ACTIONS_WINDOWS_GET_PROCESS_THREADS: ;
		end;

	UpdateControls(updates);
end;

procedure TfrmSampleApplication.PerformAction_Windows();
var
	strTmp: String;
begin
	// Windows: Perform the action
	case m_nActionCurrent of
		ACTIONS_WINDOWS_IS_64BIT:
			begin
			if (IsWindows64Bit()) then
				AddOutputText('Windows is 64-bit')
			else
				AddOutputText('Windows is NOT 64-bit');
			end;

		ACTIONS_WINDOWS_IS_WIN10:
			begin
			if (IsWindows10()) then
				AddOutputText('Running Windows 10')
			else
				AddOutputText('NOT Windows 10...');
			end;

		ACTIONS_WINDOWS_EXPAND_ENVIRONMENT:
			begin
			strTmp := ('%' + ebSample.Text + '%');
			AddOutputText(Format('%s  =  "%s"', [strTmp, ExpandEnvironment(strTmp)]));
			end;

		ACTIONS_WINDOWS_IS_PROCESS_RUNNING:
			begin
			if (IsProcessRunning(ebSample.Text)) then
				AddOutputText('Process is running')
			else
				AddOutputText('Process is NOT running');
			end;

		ACTIONS_WINDOWS_GET_PROCESS_THREADS:
			AddOutputText(Format('Process threads = %d (process ID = %d)', [
				GetProcessThreadCount(GetCurrentProcessId()),
				GetCurrentProcessId()]));
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
