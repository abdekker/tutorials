unit puzzle0008;
{$I ..\..\Languages\Delphi\Utils\CoreOptions.inc}

{ In his book, "Godel, Escher, Bach: An eternal golden braid", Douglas R. Hofstadter presented the
"MIU" formal system. This has one axiom and four rules of production.

Axiom: Strings are formed with the letters "M", "I" and "U". Only these letters are allowed.

Rules of production:
1) If you have MxI, you may make a new string MxIU
2) If you have Mx, you may make a new string Mxx
3) If you have a string with "III" anywhere in it, you may make a new string by replacing "III" with "U"
4) If you have a string with "UU" anywhere in it, you may make a new string by dropping "UU"

Note: Hofstadter calls these "theorems", but we use the generic term "strings".

Examples:
1) Given MUI, you may add MUIU to your collection
2) Given MIIU, you may add MIIUIIU to your collection
3) Given MIIII, you may add MUI or MIU to your collection
4) Given MIUUIU, you may add MIIU to your collection

You are given only the starting word "MI". Can the word "MU" be formed?

################
The MU puzzle cannot be solved (ie. the program should return "Not found"). The reason is that the
starting axiom (MI) and rules of production never remove all Is. Only rules 2 & 3 change the number
of Is. By rule 2, the number of Is is doubled and by rule 3, it is reduced by 3. The invariant is
that the number of Is is never divisible by 3:

* In the beginning, the number of Is is 1 which is not divisible by 3.
* Doubling a number that is not divisible by 3 does not make it divisible by 3.
* Subtracting 3 from a number that is not divisible by 3 does not make it divisible by 3 either.

Since the goal of MU requires there be no Is, and 0 is divisible by 3, the goal cannot be achieved.
See https://en.wikipedia.org/wiki/MU_puzzle for more details.

To be practical, this program restricts the size of generated strings. }

interface

uses
  Windows, Classes, Contnrs, Controls, ExtCtrls, Messages, StdCtrls, SyncObjs, jpeg,
  CoreFormClasses;

const
  // Thread message (used for processing strings)
  TH_STRING_PROCESSED = (WM_USER + 1);

type
  // Statistics about the puzzle
  PUZZLE_0008_STATS = record
	nThreadLoops: Integer;
	nRejectedDuplicates, nRejectedTooLong: Integer;
  end;

  // Structure to communicate with a separate thread to process a string
  STRING_TO_PROCESS = record
	strValue: String;
  end;

  // ################## Start: TMiuString / TMiuStringList ##################
  // To use a list of structures, the following classes could be used. This would enable more
  // complex information, such as the sequence of transformations used to reach string X, to be saved.

  {TMiuString = class(TObject)
  public
	strValue: String;
	bProcessed: Boolean;
  end;

  TMiuStringList = class(TObjectList)
  private
	function GetString(nIndex: Integer) : TMiuString;
	procedure SetString(nIndex: Integer; const ctMsg: TMiuString);

  public
	property Items[nIndex: Integer]: TMiuString read GetString write SetString; default;

	function Contains(const cstrString: String) : Boolean;
	function FirstUnprocessed(const cnStartIndex: Integer = 0) : Integer;
  end;}
  // ################## End: TMiuString / TMiuStringList ##################

  TfrmPuzzle0008 = class(TGeneralBaseForm)
	gbSettings: TGroupBox;
	lblStartString: TLabel;
	ebStartString: TEdit;
	lblTargetString: TLabel;
	ebTargetString: TEdit;
	lblMaxStringLength: TLabel;
	ebMaxStringLength: TEdit;
	btnStart: TButton;
	imgGodelEscherBach: TImage;

	gbResults: TGroupBox;
	lblPuzzleStatusTitle: TLabel;
	lblPuzzleStatus: TLabel;
	lblTotalProcessedTitle: TLabel;
	lblTotalProcessed: TLabel;
	lblToBeProcessedTitle: TLabel;
	lblToBeProcessed: TLabel;
	lblRejectedStringsTitle: TLabel;
	lblRejectedStrings: TLabel;
	lblDuplicateStringsTitle: TLabel;
	lblDuplicateStrings: TLabel;
	lblStringsTooLongTitle: TLabel;
	lblStringsTooLong: TLabel;
	lblProgressTitle: TLabel;
	lblProgress: TLabel;
	lblThreadLoopsTitle: TLabel;
	lblThreadLoops: TLabel;
	lblTimeTitle: TLabel;
	lblTime: TLabel;

	btnExit: TButton;
	UpdateTimer: TTimer;

	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnStartClick(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure OnUpdateTimer(Sender: TObject);

  private
	{ Private declarations }
	m_bExiting: Boolean;

	// Main string collections
	m_dictionary: TStringList;
	m_aszToProcess: TStringList;

	// Main puzzle flags
	m_dwStartTime, m_dwEndTime: DWORD;
	m_bRunningPuzzle: Boolean;
	m_stats: PUZZLE_0008_STATS;
	m_fProgress: Single;

	// Thread to process strings
	m_critPuzzle: TCriticalSection;
	m_stringToProcess: STRING_TO_PROCESS;
	m_bStopThread, m_bThreadIsProcessing: Boolean;
	m_dwThreadID: DWORD;

	procedure ThreadReply(var Msg: TMessage); message TH_STRING_PROCESSED;
	procedure UpdateControls();
	procedure UpdateStatistics();

  public
	{ Public declarations }
	constructor Create(AOwner: TComponent); override;

  end;

var
  frmPuzzle0008: TfrmPuzzle0008;
  g_nMaxLength: Integer;

implementation

uses
  StrUtils, SysUtils, SystemUtils;

const
  // Start and target string in the MIU system
  START_STRING_GUI = 'MI';
  TARGET_STRING_GUI = 'MU';
  START_STRING = 'I';		// Removing the initial "M" for efficiency
  TARGET_STRING = 'U';

  // Maximum allowed length (strings can grow by rules 1 & 2)
  MAX_LENGTH = 20;	// Should this be editable?

  // Impulse filter (used for generating a smoothed averag)
  IMPULSE_NEW_VALUE_FAST: Single	= 0.05;
  IMPULSE_OLD_AVG_FAST: Single		= 0.95;

{$R *.dfm}

// ####################### TMiuStringList Start #######################
{function TMiuStringList.GetString(nIndex: Integer): TMiuString;
begin
	Result := Get(nIndex);
end;

procedure TMiuStringList.SetString(nIndex: Integer; const ctMsg: TMiuString);
begin
	Put(nIndex, ctMsg);
end;

function TMiuStringList.Contains(const cstrString: String) : Boolean;
var
	pos: Integer;
begin
	// Determine whether the given string exists in the list...if it does, it won't be added again
	// Note: The list is unsorted, so this can take some time for a large list!
	Result := False;
	for pos:=0 to (Self.Count - 1) do
		begin
		if (AnsiCompareText(Items[pos].strValue, cstrString) = 0) then
			begin
			Result := True;
			break;
			end;
		end;
end;

function TMiuStringList.FirstUnprocessed(const cnStartIndex: Integer = 0) : Integer;
var
	pos: Integer;
begin
	// Find the first unprocessed string in our list
	Result := -1;
	for pos:=cnStartIndex to (Self.Count - 1) do
		begin
		if (not Items[pos].bProcessed) then
			begin
			Result := pos;
			break;
			end;
		end;
end;}
// ####################### TMiuStringList End #######################

// Thread functions
procedure ApplyRules(const cstrInput: String; astrOutput: TStringList);
var
	len, pos: Integer;
	strItoU, strMinusUs: String;
begin
	// Apply the four rules of production to the given string

	// Our start string is "MI" and the rules of production always retain the initial letter. For
	// efficiency (in the context of this puzzle), all strings have no initial "M". We also never
	// allow zero-length strings (ie. "UU" reduces to "" by rule 4, but we won't allow this).
	astrOutput.Clear();
	len := Length(cstrInput);

	// Rule 1: If the string has the form "MxI", add "MxIU"
	// Note: Since all strings have no initial "M", we need only check for a trailing "I". Formally,
	// we should check "if (AnsiStartsStr('M', cstrInput))".
	if (AnsiEndsStr('I', cstrInput)) and (len < MAX_LENGTH) then
		begin
		if (len < MAX_LENGTH) then
			astrOutput.Add(cstrInput + 'U')
		else
			Inc(frmPuzzle0008.m_stats.nRejectedTooLong);
		end;

	// Rule 2: If the string has the form "Mx", add "Mxx"
	// Note: Since all strings have no initial "M", and we only allow non zero-length strings,
	// every string can simply be doubled by this rule.
	if ((len * 2) <= g_nMaxLength) then
		astrOutput.Add(cstrInput + cstrInput)
	else
		Inc(frmPuzzle0008.m_stats.nRejectedTooLong);

	// Rule 3: If the string contains "III", replace "III" with "U"
	if (len >= 3) then
		begin
		// NB: Strings in Delphi are 1-based!
		for pos:=1 to (len - 2) do
			begin
			if (	(cstrInput[pos] = 'I') and
					(cstrInput[pos + 1] = 'I') and
					(cstrInput[pos + 2] = 'I')) then
				begin
				strItoU := (
					AnsiLeftStr(cstrInput, pos - 1) +
					'U' +
					AnsiRightStr(cstrInput, len - pos - 2));
				astrOutput.Add(strItoU);
				end;
			end;
		end;

	// Rule 4: If the string contains "UU", remove "UU"
	// Note: We don't allow zero-length strings, so "UU" is left unprocessed by rule 4
	if (len > 2) then
		begin
		// NB: Strings in Delphi are 1-based!
		pos := 1;
		while (pos < (len - 1)) do
			begin
			if (	(cstrInput[pos] = 'U') and
					(cstrInput[pos + 1] = 'U')) then
				begin
				strMinusUs :=
					(AnsiLeftStr(cstrInput, pos - 1) + AnsiRightStr(cstrInput, len - pos - 1));
				astrOutput.Add(strMinusUs);
				end;

			Inc(pos);
			end;
		end;
end;

function ThreadProcessString(pData: Pointer) : Integer;
var
	pos, index: Integer;
	strNew: String;
	astrNew: TStringList;
begin
	// Process a new string...
	frmPuzzle0008.m_critPuzzle.Acquire();
	frmPuzzle0008.m_bThreadIsProcessing := True;
	astrNew := TStringList.Create();
	Result := 0;
	try
		if (not frmPuzzle0008.m_bStopThread) then
			begin
			// Apply rules...
			ApplyRules(STRING_TO_PROCESS(pData^).strValue, astrNew);

			// Any new strings to add?
			if (astrNew.Count > 0) then
				begin
				for pos:=0 to (astrNew.Count - 1) do
					begin
					strNew := astrNew[pos];
					if (frmPuzzle0008.m_dictionary.Find(strNew, index)) then
						Inc(frmPuzzle0008.m_stats.nRejectedDuplicates)
					else
						frmPuzzle0008.m_aszToProcess.Add(strNew);
					end;
				end;
			end;
	finally
		// Increment statistics
		Inc(frmPuzzle0008.m_stats.nThreadLoops);

		// Clean up
		astrNew.Free();

		// Inform the main form that another string has been processed
		// Note: "PostMessage" is better than "SendMessage" because the latter blocks and does not
		// return until the window procedure for specified window has processed the message.
		frmPuzzle0008.m_bThreadIsProcessing := False;
		frmPuzzle0008.m_critPuzzle.Release();
		PostMessage(frmPuzzle0008.Handle, TH_STRING_PROCESSED, 0, 0);
	end;
end;

// Constructor
constructor TfrmPuzzle0008.Create(AOwner: TComponent);
begin
	// Local data
	frmPuzzle0008 := Self;
	m_bExiting := False;

	m_dictionary := TStringList.Create();
	m_dictionary.Sorted := True;
	m_aszToProcess := TStringList.Create();
	m_aszToProcess.Sorted := False;

	m_dwStartTime := 0;
	m_dwEndTime := 0;
	m_bRunningPuzzle := False;
	ZeroMemory(@m_stats, SizeOf(PUZZLE_0008_STATS));

	m_critPuzzle := TCriticalSection.Create();
	m_bStopThread := True;
	m_bThreadIsProcessing := False;

	// Call base constructor
	inherited;
end;

procedure TfrmPuzzle0008.FormDestroy(Sender: TObject);
begin
	// Clean up memory
	m_dictionary.Free();
	m_aszToProcess.Free();
	m_critPuzzle.Free();
end;

procedure TfrmPuzzle0008.FormShow(Sender: TObject);
var
	strTitle: String;
begin
	// Form show event
	strTitle := (
		'Puzzle 0008 - MIU '
		{$IFDEF DBG} + '[DBG]' {$ENDIF}
		{$IFDEF NDBG} + '[NDBG]' {$ENDIF});
	Caption := strTitle;

	ebStartString.Text := START_STRING_GUI;
	ebTargetString.Text := TARGET_STRING_GUI;
	ebMaxStringLength.Text := IntToStr(MAX_LENGTH);

	UpdateTimer.Enabled := True;
end;

procedure TfrmPuzzle0008.btnStartClick(Sender: TObject);
begin
	// Start or stop the puzzle
	m_bRunningPuzzle := (not m_bRunningPuzzle);
	UpdateControls();
	if (m_bRunningPuzzle) then
		begin
		// About to run the puzzle...
		m_critPuzzle.Acquire();
		if (not TryStrToInt(ebMaxStringLength.Text, g_nMaxLength)) then
			g_nMaxLength := MAX_LENGTH;

		// Run the puzzle!
		m_dwStartTime := GetTickCount();
		m_bStopThread := False;
		m_fProgress := 0.0;

		// Clean up data from a previous run
		m_dictionary.Clear();
		m_aszToProcess.Clear();
		ZeroMemory(@m_stats, SizeOf(PUZZLE_0008_STATS));

		// Add the starting string and process!
		m_dictionary.Add(START_STRING);
		m_stringToProcess.strValue := START_STRING;
		m_critPuzzle.Release();
		BeginThread(nil, 0, @ThreadProcessString, Addr(m_stringToProcess), 0, m_dwThreadID);
		end
	else
		begin
		// Stop the puzzle...
		m_bStopThread := True;
		while (m_bThreadIsProcessing) do
			Sleep(1);
		end;
end;

procedure TfrmPuzzle0008.btnExitClick(Sender: TObject);
begin
	m_bExiting := True;
	m_bStopThread := True;
	while (m_bThreadIsProcessing) do
		Sleep(1);

	Close();
end;

procedure TfrmPuzzle0008.OnUpdateTimer(Sender: TObject);
begin
	// Update the user interface
	UpdateTimer.Enabled := False;

	// Statistics
	if (m_bRunningPuzzle) then
		begin
		m_critPuzzle.Acquire();
		UpdateStatistics();
		m_critPuzzle.Release();
		end;

	UpdateTimer.Enabled := (not m_bExiting);
end;

// Private functions: Start
procedure TfrmPuzzle0008.ThreadReply(var Msg: TMessage);
var
	index: Integer;
	fTime: Single;
begin
	// This method is called when our worker thread processes a string...

	// Are there any more strings to process?
	if (m_bRunningPuzzle) then
		begin
		// Any more strings to process?
		// Note,1: This program would probably be more efficient if we started several threads at
		// once...but that would be over-engineering for this simple puzzle.
		// Note,2: It is much more efficient to remove TStringList items from the end (as opposed
		// to the beginning)...so use "list.Delete(Count - 1)" and not "list.Delete(0)"!
		m_bRunningPuzzle := (m_aszToProcess.Count > 0);
		if (m_bRunningPuzzle) then
			begin
			// Puzzle still running...
			m_critPuzzle.Acquire();
			m_stringToProcess.strValue := m_aszToProcess[m_aszToProcess.Count - 1];
			m_dictionary.Add(m_stringToProcess.strValue);
			m_aszToProcess.Delete(m_aszToProcess.Count - 1);
			m_critPuzzle.Release();
			BeginThread(nil, 0, @ThreadProcessString, Addr(m_stringToProcess), 0, m_dwThreadID);
			end
		else
			begin
			// Puzzle complete!
			UpdateControls();
			UpdateStatistics();

			// Write a more accurate time (down to ms)
			m_dwEndTime := GetTickCount();
			fTime := ((m_dwEndTime - m_dwStartTime) / 1000.0);
			lblTime.Caption := Format('%.3f s', [fTime]);

			// Was the target string generated?
			if (m_dictionary.Find(TARGET_STRING, index)) then
				MessageBox(0,
					PAnsiChar(Format('"%s" found with length %d', [TARGET_STRING_GUI, g_nMaxLength])),
					'MIU System', MB_ICONEXCLAMATION)
			else
				MessageBox(0,
					PAnsiChar(Format('"%s" NOT found with length %d', [TARGET_STRING_GUI, g_nMaxLength])),
					'MIU System', MB_ICONEXCLAMATION)
			end;
		end;
end;

procedure TfrmPuzzle0008.UpdateControls();
begin
	// Update the user interface
	if (m_bRunningPuzzle) then
		begin
		// Puzzle is now running...
		btnStart.Caption := 'Stop';
		lblPuzzleStatus.Caption := 'RUNNING';
		UpdateStatistics();
		end
	else
		begin
		// Puzzle is no longer running
		btnStart.Caption := 'Start!';
		lblPuzzleStatus.Caption := 'WAITING...';
		// Don't update the stats...
		end;
end;

procedure TfrmPuzzle0008.UpdateStatistics();
var
	nRejected, nProcessed, nTotal: Integer;
	fProgress: Single;
	dwElapsed: DWORD;
begin
	// Update statistics while running the puzzle
	lblTotalProcessed.Caption := IntToStr(m_dictionary.Count);
	lblToBeProcessed.Caption := IntToStr(m_aszToProcess.Count);

	nRejected := (m_stats.nRejectedDuplicates + m_stats.nRejectedTooLong);
	nProcessed := (nRejected + m_dictionary.Count);
	nTotal := (nProcessed + m_aszToProcess.Count);

	lblRejectedStrings.Caption := FloatToStrF(nRejected, ffNumber, 10, 0);
	lblDuplicateStrings.Caption := FloatToStrF(m_stats.nRejectedDuplicates, ffNumber, 10, 0);
	lblStringsTooLong.Caption := FloatToStrF(m_stats.nRejectedTooLong, ffNumber, 10, 0);

	if (nTotal > 0) then
		begin
		if (m_aszToProcess.Count = 0) then
			m_fProgress := 1.0
		else
			begin
			fProgress := (nProcessed / nTotal);
			m_fProgress := (
				(IMPULSE_NEW_VALUE_FAST * fProgress) +
				(IMPULSE_OLD_AVG_FAST * m_fProgress));
			end;

		lblProgress.Caption := Format('%.1f %%', [m_fProgress * 100.0]);
		end
	else
		lblProgress.Caption := '0.0 %';

	lblThreadLoops.Caption := IntToStr(m_stats.nThreadLoops);
	dwElapsed := (GetTickCount() - m_dwStartTime);
	lblTime.Caption := GetTimeStringFromSeconds(dwElapsed div 1000);
end;
// Private functions: End

end.
