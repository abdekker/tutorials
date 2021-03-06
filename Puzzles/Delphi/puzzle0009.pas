unit puzzle0009;
{$I ..\..\Languages\Delphi\Utils\CoreOptions.inc}

{ There are 18 people (or items, etc) and a group size of 3. Each round, partition the people into
groups. For example: Round 1 = (1,2,3) (4,5,6) (7,8,9) (10,11,12) (13,14,15) (16,17,18)

On each subsequent round, generate a new partition (ie. mix the people into different groups). The
aim is to generate the minimum number of rounds such that everyone is with everyone else at least
once. Duplicates (two people being in the same group) will be inevitable, but should be kept to a
minimum. The order in each group is not relevant.

Ideally, the total number of people and the group size would be flexible.

An example application:
* 18 young violin players are meeting at a summer camp
* The organiser has acquired a number of violin trios
* Each round, the organiser wishes to partition the players together into six groups (18/3=6)
* During a round, all groups play through the same trio
* After, say, 30 minutes and a short break, a new round starts with a fresh grouping and new trio
* Everyone should play with everyone else at least once during the day (order does not matter)
* What is the minimum number of rounds required to product this result? }

interface

uses
  Windows, Classes, ComCtrls, Contnrs, Controls, ExtCtrls, Messages, StdCtrls,
  CoreFormClasses;

type
  // ################## Start: TConnectionPair / TConnectionPairList ##################
  TConnectionPair = class(TObject)
  public
	a, b: Integer;
  end;

  TConnectionPairList = class(TObjectList)
  private
	function GetPair(nIndex: Integer) : TConnectionPair;
	procedure SetPair(nIndex: Integer; const cPair: TConnectionPair);
	function Equals(const cPair1: TConnectionPair; const cPair2: TConnectionPair) : Boolean;

  public
	// Properties
	property Items[nIndex: Integer]: TConnectionPair read GetPair write SetPair; default;

	// Methods
	function GetPos(const cPair: TConnectionPair) : Integer;
	function CountOccurrences(const cnItem: Integer) : Integer;
  end;
  // ################## End: TConnectionPair / TConnectionPairList ##################

  // ################## Start: TItemsGroup / TItemsGroupList ##################
  TItemsGroup = class(TObject)
  public
	items: TList;

	constructor Create();
	destructor Destroy(); override;
	function Contains(const cValue: Integer) : Boolean;
  end;

  TItemsGroupList = class(TObjectList)
  private
	function GetGroup(nIndex: Integer) : TItemsGroup;
	procedure SetGroup(nIndex: Integer; const cGroup: TItemsGroup);
	function Equals(const cGroup1: TItemsGroup; const cGroup2: TItemsGroup) : Boolean;

  public
	// Properties
	property Items[nIndex: Integer]: TItemsGroup read GetGroup write SetGroup; default;

	// Methods
	function TotalItemsCount() : Integer;
	function GetPos(const cGroup: TItemsGroup) : Integer;
	function Contains(const cValue: Integer) : Boolean; overload;
	function Contains(const cValue: Integer;
		const cnStartGroup, cnEndGroup: Integer) : Boolean; overload;
  end;
  // ################## End: TItemsGroup / TItemsGroupList ##################

  TfrmPuzzle0009 = class(TGeneralBaseForm)
	gbSettings: TGroupBox;
	lblTotalItems: TLabel;
	ebTotalItems: TEdit;
	trackerTotalItems: TTrackBar;
	lblGroupSize: TLabel;
	ebGroupSize: TEdit;
	trackerGroupSize: TTrackBar;
	tbSortGroupsInternal: TCheckBox;
	tbSortGroupsExternal: TCheckBox;
	btnStart: TButton;

	gbResults: TGroupBox;
	lblTimeTitle: TLabel;
	lblTime: TLabel;
	pbProgress: TProgressBar;
	memoResults: TMemo;

	btnExit: TButton;
	UpdateTimer: TTimer;

	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnStartClick(Sender: TObject);
	procedure btnExitClick(Sender: TObject);
	procedure OnUpdateTimer(Sender: TObject);

	procedure OnTotalItemsChange(Sender: TObject);
	procedure OnGroupSizeChange(Sender: TObject);

  private
	{ Private declarations }
	m_bExiting: Boolean;

	// Puzzle flags
	m_dwStartTime, m_dwEndTime, m_dwStarts: DWORD;
	m_bRunningPuzzle: Boolean;

	// Puzzle settings
	m_nTotalItems, m_nGroupSize, m_nGroupsPerRound: Integer;

	// Rounds
	m_nNumRounds, m_nNumRoundsBest: Integer;
	m_roundsAll, m_roundsAllBest: TItemsGroupList;
	m_pairsToDo: TConnectionPairList;
	m_nInitialPairsToDo: Integer;

	// Current round
	m_roundCurrent: TItemsGroupList;
	m_pairsCurrent: TConnectionPairList;
	m_connectionsAllGroups, m_connectionsWithinGroup: TList;

	procedure UpdateControls();
	procedure UpdateStatistics();

	procedure RunPuzzle();
	procedure GenerateGroup(group: TItemsGroup);
	procedure AnalyseConnections(connections: TList; group: TItemsGroup; var nMaxConnections: Integer);
	procedure SortRound(round: TItemsGroupList);

	procedure DbgCheckRoundForAllItems(round: TItemsGroupList);
	procedure DbgCheckRoundsForAllConnections(rounds: TItemsGroupList);

	function DbgGetConnectionPairs(pairs: TConnectionPairList) : String;
	function DbgGetGroupItems(group: TItemsGroup) : String;
	function DbgGetRoundGroups(round: TItemsGroupList) : String;
	function DbgGetRoundGroupsAll(rounds: TItemsGroupList; const cnRound: Integer) : String;

  public
	{ Public declarations }
	constructor Create(AOwner: TComponent); override;

  end;

var
  frmPuzzle0009: TfrmPuzzle0009;

implementation

uses
  Forms, StrUtils, SysUtils,
  SystemUtils;

{$R *.dfm}

// ####################### TConnectionPairList Start #######################
function TConnectionPairList.GetPair(nIndex: Integer) : TConnectionPair;
begin
	Result := Get(nIndex);
end;

procedure TConnectionPairList.SetPair(nIndex: Integer; const cPair: TConnectionPair);
begin
	Put(nIndex, cPair);
end;

function TConnectionPairList.Equals(
	const cPair1: TConnectionPair; const cPair2: TConnectionPair) : Boolean;
begin
	// Pairs are equal if they contain the same members (order does not matter)
	Result := False;
	if (	((cPair1.a = cPair2.a) and (cPair1.b = cPair2.b)) or
			((cPair1.a = cPair2.b) and (cPair1.b = cPair2.a))) then
		Result := True;
end;

function TConnectionPairList.GetPos(const cPair: TConnectionPair) : Integer;
var
	pos: Integer;
begin
	// Return the index of the item (-1 if the item is not in the list)
	Result := -1;
	for pos:=0 to (Self.Count - 1) do
		begin
		if (Equals(Items[pos], cPair)) then
			begin
			Result := pos;
			break;
			end;
		end;
end;

function TConnectionPairList.CountOccurrences(const cnItem: Integer) : Integer;
var
	nCount, pos: Integer;
begin
	// Return how many times the requested item appears in any pair (in any order)
	nCount := 0;
	for pos:=0 to (Self.Count - 1) do
		begin
		if (	(cnItem = TConnectionPair(Items[pos]).a) or
				(cnItem = TConnectionPair(Items[pos]).b)) then
			Inc(nCount);
		end;

	Result := nCount;
end;
// ####################### TConnectionPairList End #######################

// ####################### TItemsGroup Start #######################
constructor TItemsGroup.Create();
begin
	// Create a new user account list
	inherited;
	items := TList.Create();
end;

destructor TItemsGroup.Destroy();
begin
	// Call the parent destructor
	items.Free();
	inherited;
end;

function TItemsGroup.Contains(const cValue: Integer) : Boolean;
var
	pos, groupValue: Integer;
begin
	// Does this group contain the specified value?
	Result := False;
	for pos:=0 to (items.Count - 1) do
		begin
		groupValue := Integer(items[pos]);
		if (groupValue = cValue) then
			begin
			Result := True;
			break;
			end;
		end;
end;
// ####################### TItemsGroup End #######################

// ####################### TItemsGroupList Start #######################
function TItemsGroupList.GetGroup(nIndex: Integer) : TItemsGroup;
begin
	Result := Get(nIndex);
end;

procedure TItemsGroupList.SetGroup(nIndex: Integer; const cGroup: TItemsGroup);
begin
	Put(nIndex, cGroup);
end;

function TItemsGroupList.Equals(const cGroup1: TItemsGroup; const cGroup2: TItemsGroup) : Boolean;
var
	nItem: Integer;
begin
	// Groups are equal if they contain the same members (order does not matter)
	Result := False;
	if (cGroup1.items.Count = cGroup2.items.Count) then
		begin
		// Groups are the same size...
		for nItem:=0 to (cGroup1.items.Count - 1) do
			begin
			if (not cGroup2.Contains(Integer(cGroup1.items[nItem]))) then
				Exit;
			end;

		// If we get here, every item in the first group is contained the second. Since the groups
		// are also the same size, they must be equal!
		Result := True;
		end;
end;

function TItemsGroupList.TotalItemsCount() : Integer;
var
	pos, countTotal: Integer;
begin
	// Count of all items in all sub-groups. For example, the list (1,2,3)(4,5,6)(7,8,9) has three
	// groups with a total items count of 9.
	countTotal := 0;
	for pos:=0 to (Self.Count - 1) do
		Inc(countTotal, Items[pos].items.Count);

	Result := countTotal;
end;

function TItemsGroupList.GetPos(const cGroup: TItemsGroup) : Integer;
var
	pos: Integer;
begin
	// Return the index of the item (-1 if the item is not in the list)
	Result := -1;
	for pos:=0 to (Self.Count - 1) do
		begin
		if (Equals(Items[pos], cGroup)) then
			begin
			Result := pos;
			break;
			end;
		end;
end;

function TItemsGroupList.Contains(const cValue: Integer) : Boolean;
var
	pos: Integer;
begin
	// Do any of the groups in this list contain the specified value?
	Result := False;
	for pos:=0 to (Self.Count - 1) do
		begin
		if (TItemsGroup(Items[pos]).Contains(cValue)) then
			begin
			Result := True;
			break;
			end;
		end;
end;

function TItemsGroupList.Contains(const cValue: Integer; const cnStartGroup, cnEndGroup: Integer) : Boolean;
var
	pos: Integer;
begin
	// Do any of the groups, within the given 0-based range, contain the specified value?
	Result := False;
	if (	(cnStartGroup < 0) or
			(cnStartGroup > cnEndGroup) or
			(cnEndGroup > (Self.Count - 1))) then
		Exit;

	for pos:=cnStartGroup to cnEndGroup do
		begin
		if (TItemsGroup(Items[pos]).Contains(cValue)) then
			begin
			Result := True;
			break;
			end;
		end;

end;
// ####################### TConnectionPairList End #######################

// Constructor
constructor TfrmPuzzle0009.Create(AOwner: TComponent);
begin
	// Local data
	Randomize();
	frmPuzzle0009 := Self;
	m_bExiting := False;

	m_dwStartTime := 0;
	m_dwEndTime := 0;
	m_dwStarts := 0;
	m_bRunningPuzzle := False;

	m_nNumRoundsBest := High(Integer);
	m_roundsAll := TItemsGroupList.Create();
	m_roundsAllBest := TItemsGroupList.Create();
	m_pairsToDo := TConnectionPairList.Create();

	m_roundCurrent := TItemsGroupList.Create();
	m_pairsCurrent := TConnectionPairList.Create();
	m_connectionsAllGroups := TList.Create();
	m_connectionsWithinGroup := TList.Create();

	// Call base constructor
	inherited;
end;

procedure TfrmPuzzle0009.FormDestroy(Sender: TObject);
begin
	// Clean up memory
	m_roundsAll.Free();
	m_roundsAllBest.Free();
	m_pairsToDo.Free();

	m_roundCurrent.Free();
	m_pairsCurrent.Free();
	m_connectionsAllGroups.Free();
	m_connectionsWithinGroup.Free();
end;

procedure TfrmPuzzle0009.FormShow(Sender: TObject);
var
	strTitle: String;
begin
	// Form show event
	strTitle := (
		'Puzzle 0009 - Group partitioning '
		{$IFDEF DBG} + '[DBG]' {$ENDIF}
		{$IFDEF NDBG} + '[NDBG]' {$ENDIF});
	Caption := strTitle;
	btnStart.SetFocus();

	// Start an update timer
	UpdateTimer.Enabled := True;
end;

procedure TfrmPuzzle0009.btnStartClick(Sender: TObject);
begin
	// Start or stop the puzzle
	Inc(m_dwStarts);
	m_bRunningPuzzle := (not m_bRunningPuzzle);
	UpdateControls();
	if (m_bRunningPuzzle) then
		begin
		// About to run the puzzle...
		m_dwStartTime := GetTickCount();
		lblTime.Caption := '0.000s';

		// Run the puzzle!
		RunPuzzle();
		end
	else
		begin
		// Stop the puzzle...
		end;
end;

procedure TfrmPuzzle0009.btnExitClick(Sender: TObject);
begin
	m_bExiting := True;
	m_bRunningPuzzle := False;
	Close();
end;

procedure TfrmPuzzle0009.OnUpdateTimer(Sender: TObject);
begin
	// Update the user interface
	UpdateTimer.Enabled := False;

	// Statistics
	if (m_bRunningPuzzle) then
		UpdateStatistics();

	UpdateTimer.Enabled := (not m_bExiting);
end;

procedure TfrmPuzzle0009.OnTotalItemsChange(Sender: TObject);
begin
	ebTotalItems.Text := IntToStr(trackerTotalItems.Position);
	m_nNumRoundsBest := High(Integer);
end;

procedure TfrmPuzzle0009.OnGroupSizeChange(Sender: TObject);
begin
	ebGroupSize.Text := IntToStr(trackerGroupSize.Position);
	m_nNumRoundsBest := High(Integer);
end;

// Private functions: Start
procedure TfrmPuzzle0009.UpdateControls();
begin
	// Update the user interface
	trackerTotalItems.Enabled := (not m_bRunningPuzzle);
	trackerGroupSize.Enabled := (not m_bRunningPuzzle);
	if (m_bRunningPuzzle) then
		begin
		// Puzzle is now running...
		btnStart.Caption := 'Stop';
		UpdateStatistics();
		end
	else
		begin
		// Puzzle is no longer running
		btnStart.Caption := 'Start!';
		// Don't update the stats...
		end;
end;

procedure TfrmPuzzle0009.UpdateStatistics();
var
	dwElapsed: DWORD;
begin
	// Update statistics while running the puzzle
	dwElapsed := (GetTickCount() - m_dwStartTime);
	lblTime.Caption := Format('%.3fs', [(dwElapsed / 1000.0)]);
end;

procedure TfrmPuzzle0009.RunPuzzle();
var
	nFirst, nSecond, nRound, nGroup: Integer;
	pair: TConnectionPair;
	group: TItemsGroup;
	strRound: String;
	{$IFDEF DBG} strDbg: String; {$ENDIF}
begin
	// Main method to run the puzzle (could use a thread, but this is a relatively simple puzzle)
	m_roundsAll.Clear();

	// Settings. The group size should divide exactly into the total number of items. If not, add
	// additional items to the total number (these would be wildcard items, people, byes, etc).
	m_nTotalItems := trackerTotalItems.Position;
	m_nGroupSize := trackerGroupSize.Position;
	if (m_nTotalItems < m_nGroupSize) then
		m_nTotalItems := m_nGroupSize;

	if ((m_nTotalItems mod m_nGroupSize) <> 0) then
		begin
		Dec(m_nTotalItems, (m_nTotalItems mod m_nGroupSize));
		Inc(m_nTotalItems, m_nGroupSize);
		end;

	m_nGroupsPerRound := (m_nTotalItems div m_nGroupSize);
	if (trackerTotalItems.Position <> m_nTotalItems) then
		trackerTotalItems.Position := m_nTotalItems;

	if (trackerGroupSize.Position <> m_nGroupSize) then
		trackerGroupSize.Position := m_nGroupSize;

	// Add all possible connections into a large "to do" set. After all rounds are complete, this
	// set should be empty.
	for nFirst:=1 to m_nTotalItems do
		begin
		for nSecond:=(nFirst + 1) to m_nTotalItems do
			begin
			pair := TConnectionPair.Create();
			pair.a := nFirst;
			pair.b := nSecond;
			m_pairsToDo.Add(pair);
			end;
		end;

	{$IFDEF DBG} strDbg := DbgGetConnectionPairs(m_pairsToDo); {$ENDIF}
	// Pairs can be extracted from the list using any of:
	//		pair := m_pairsToDo.GetPair(x);
	//		pair := m_pairsToDo[x];
	//		pair := m_pairsToDo.Items[x];
	//		pair := TConnectionPair(m_pairsToDo.Items[x]);

	// If there are thousands of pairs to do, show a progress bar
	m_nInitialPairsToDo := m_pairsToDo.Count;
	if (m_nInitialPairsToDo >= 1000) then
		begin
		pbProgress.Max := m_nInitialPairsToDo;
		pbProgress.Position := 0;
		pbProgress.Visible := True;
		end;

	// Run the puzzle!
	m_nNumRounds := 0;
	while (m_pairsToDo.Count > 0) do
		begin
		// Check we are still running the puzzle...
		if (not m_bRunningPuzzle) then
			break;

		// Start a new round!
		Inc(m_nNumRounds);
		m_roundCurrent.Clear();
		while (m_roundCurrent.TotalItemsCount <> m_nTotalItems) do
			begin
			// Create a new grouping to be added to the current round
			group := TItemsGroup.Create();
			GenerateGroup(group);
			{$IFDEF DBG} strDbg := DbgGetConnectionPairs(m_pairsToDo); {$ENDIF}
			{$IFDEF DBG} strDbg := DbgGetGroupItems(group); {$ENDIF}

			m_roundCurrent.Add(group);
			end;

		// Round is complete! Perform a quick check to ensure that every item is in this round.
		DbgCheckRoundForAllItems(m_roundCurrent);

		// Sort the group?
		if (tbSortGroupsInternal.Checked) or (tbSortGroupsExternal.Checked) then
			SortRound(m_roundCurrent);

		// Copy all groups from this round to the list of all rounds
		{$IFDEF DBG} strDbg := DbgGetRoundGroups(m_roundCurrent); {$ENDIF}
		for nGroup:=0 to (m_roundCurrent.Count - 1) do
			begin
			group := TItemsGroup.Create();
			group.items.Assign(TItemsGroup(m_roundCurrent[nGroup]).items);
			m_roundsAll.Add(group);
			end;

		{$IFDEF DBG} strDbg := DbgGetRoundGroups(m_roundsAll); {$ENDIF}
		if (pbProgress.Visible) then
			pbProgress.Position := (m_nInitialPairsToDo - m_pairsToDo.Count);

		// Check whether the user has stopped or exited the puzzle
		Application.ProcessMessages();
		end;

	// Puzzle is complete! Perform a quick check to ensure that every connection is satisfied.
	DbgCheckRoundsForAllConnections(m_roundsAll);

	// Is this the best round so far?
	if (	(m_pairsToDo.Count = 0) and
			(m_nNumRounds < m_nNumRoundsBest)) then
		begin
		// Yes! Record this...
		m_roundsAllBest.Clear();
		m_nNumRoundsBest := m_nNumRounds;
		for nGroup:=0 to (m_roundsAll.Count - 1) do
			begin
			group := TItemsGroup.Create();
			group.items.Assign(TItemsGroup(m_roundsAll[nGroup]).items);
			m_roundsAllBest.Add(group);
			end;
		end;

	memoResults.Lines.Clear();
	if (m_bRunningPuzzle) then
		begin
		// Number of rounds
		memoResults.Lines.Add(Format('Total rounds: %d (Items: %d, Group size: %d)', [
			m_nNumRounds, m_nTotalItems, m_nGroupSize]));
		for nRound:=1 to m_nNumRounds do
			begin
			strRound := Format('Rd %.3d: %s', [
				nRound,
				DbgGetRoundGroupsAll(m_roundsAll, nRound)]);
			memoResults.Lines.Add(strRound);
			end;
		end
	else
		memoResults.Lines.Add('Calculation interrupted...');

	// Show the best solution so far...
	if (m_nNumRoundsBest < High(Integer)) then
		begin
		memoResults.Lines.Add('');
		memoResults.Lines.Add(Format('Best so far! Total rounds: %d', [m_nNumRoundsBest]));
		for nRound:=1 to m_nNumRoundsBest do
			begin
			strRound := Format('Rd %.3d: %s', [
				nRound,
				DbgGetRoundGroupsAll(m_roundsAllBest, nRound)]);
			memoResults.Lines.Add(strRound);
			end;
		end;

	// Stop the puzzle
	m_bRunningPuzzle := False;
	UpdateControls();
	UpdateStatistics();
	pbProgress.Visible := False;
end;

procedure TfrmPuzzle0009.GenerateGroup(group: TItemsGroup);
var
	maxConnectionsToDoAll, maxConnectionsToDoGroup, pairItem, pos: Integer;
	listMaxConnectionsToDoAll, listMaxConnectionsToDoGroup: TList;
	pair: TConnectionPair;
begin
	// Analyse all connections still to do. Select the item which has the highest number of
	// unsatisified connections as the first member of the new group.
	AnalyseConnections(m_connectionsAllGroups, group, maxConnectionsToDoAll);

	// Transfer all items which have the maximum connections still to do into a new list
	listMaxConnectionsToDoAll := TList.Create();
	for pos:=0 to (m_connectionsAllGroups.Count - 1) do
		begin
		if (Integer(m_connectionsAllGroups[pos]) = maxConnectionsToDoAll) then
			listMaxConnectionsToDoAll.Add(Pointer(pos + 1));
		end;

	// Select one of these items at random as the first member of the new group
	pos := Integer(listMaxConnectionsToDoAll[Random(listMaxConnectionsToDoAll.Count)]);
	group.items.Add(Pointer(pos));

	// Add new items to the group until the group has the required size. Each new item added:
	// * must not have been selected in another group this round
	// * should have the maximum number of unsatisfied connections to other members of the group
	listMaxConnectionsToDoGroup := TList.Create();
	pair := TConnectionPair.Create();
	while (group.items.Count < m_nGroupSize) do
		begin
		// Count connections still to do, but only inside this group
		AnalyseConnections(m_connectionsWithinGroup, group, maxConnectionsToDoGroup);

		// Transfer all items which have the maximum connections still to do into a new list
		// Note: One idea here would be to use a secondary heuristic in the case of a tie, such as
		// the number of unsatisfied connections outside of the group. This was attempted but did
		// not give any improvement in the number required rounds.
		listMaxConnectionsToDoGroup.Clear();
		for pos:=0 to (m_connectionsWithinGroup.Count - 1) do
			begin
			if (Integer(m_connectionsWithinGroup[pos]) = maxConnectionsToDoGroup) then
				listMaxConnectionsToDoGroup.Add(Pointer(pos + 1));
			end;

		// Select one of these items at random as the next member of the new group
		pos := Integer(listMaxConnectionsToDoGroup[Random(listMaxConnectionsToDoGroup.Count)]);
		group.items.Add(Pointer(pos));

		// Remove connections between the new item and other items in the group from the overall
		// "to do" list of unsatisfied connections
		pair.a := pos;
		for pairItem:=0 to (group.items.Count - 2) do
			begin
			pair.b := Integer(group.items[pairItem]);
			pos := m_pairsToDo.GetPos(pair);
			if (pos >= 0) then
				m_pairsToDo.Delete(pos);
			end;
		end;

	// Group is complete!

	// Clean up
	listMaxConnectionsToDoAll.Free();
	listMaxConnectionsToDoGroup.Free();
	pair.Free();
end;

procedure TfrmPuzzle0009.AnalyseConnections(connections: TList; group: TItemsGroup;
	var nMaxConnections: Integer);
var
	groupItem, pairItem, nOccurrences, nNonNegativeOccurrences: Integer;
	pair: TConnectionPair;
begin
	// Analyse the connections still to do
	nMaxConnections := 0;
	nNonNegativeOccurrences := 0;
	connections.Clear();
	for groupItem:=1 to m_nTotalItems do
		begin
		nOccurrences := 0;
		if (m_roundCurrent.Contains(groupItem)) then
			begin
			// Item already selected in another group in this round...
			nOccurrences := -1;
			end
		else if (group.items.Count > 0) then
			begin
			// New group: Count connections only to other items already in this (incomplete) group
			if (group.Contains(groupItem)) then
				begin
				// Item has already been selected for this new group
				nOccurrences := -1;
				end
			else
				begin
				// Possible new selection for the group...count unsatisfied connections between
				// this item and all other items (in this group only)
				pair := TConnectionPair.Create();
				pair.a := groupItem;
				for pairItem:=0 to (group.items.Count - 1) do
					begin
					pair.b := Integer(group.items[pairItem]);
					if (m_pairsToDo.GetPos(pair) >= 0) then
						Inc(nOccurrences);
					end;

				pair.Free();
				end;
			end
		else
			begin
			// There are no items selected in this group. Count how many times this item appears in
			// the overall "to do" list of connections.
			nOccurrences := m_pairsToDo.CountOccurrences(groupItem);
			end;

		if (nOccurrences >= 0) then
			Inc(nNonNegativeOccurrences);

		connections.Add(Pointer(nOccurrences));
		if (nOccurrences > nMaxConnections) then
			nMaxConnections := nOccurrences;
		end;

	// If we exit the loop with all items already having been selected either:
	// * in another group earlier in this round or
	// * as a member of the current group
	// then the value of "nNonNegativeOccurences" will be zero. The algorithm ensures this should
	// never happen, so this note is purely a reminder of this (theoretic) possibility.
	if (nNonNegativeOccurrences = 0) then
		;	// Do something?
end;

procedure TfrmPuzzle0009.SortRound(round: TItemsGroupList);
var
	nGroup, nGroupAfter, nItem: Integer;
	group: TItemsGroup;

	// Nested function to sort items in a group
	function SortItems(pItem1, pItem2: Pointer): Integer;
	begin
		// This function is used to help sort the list of user accounts (by user ID)
		Result := 0;
		if (Integer(pItem1) > Integer(pItem2)) then
			Result := 1
		else if (Integer(pItem1) < Integer(pItem2)) then
			Result := -1;
	end;
begin
	// Sort the groups within a single round. Example:
	//		(13,4,17) (3,15,1) (5,7,18) (11,6,16) (14,10,9) (2,12,8)
	// Sorted internally within each group to:
	//		(4,13,17) (1,3,15) (5,7,18) (6,11,16) (9,10,14) (2,8,12)
	// Then sorted between groups to:
	//		(1,3,15) (2,8,12) (4,13,17) (5,7,18) (6,11,16) (9,10,14)

	// Note that the final sorted list always has the first item on the far left.

	// Sort the items internally within each group
	if (tbSortGroupsInternal.Checked) then
		begin
		for nGroup:=0 to (round.Count - 1) do
			TItemsGroup(round[nGroup]).items.Sort(@SortItems);
		end;

	// Sort externally between the groups
	if (tbSortGroupsExternal.Checked) then
		begin
		nItem := 1;
		nGroup := 0;
		while (nItem <= m_nTotalItems) do
			begin
			if (round.Contains(nItem, 0, (nGroup - 1))) then
				begin
				// One of the preceding groups contains the item!
				end
			else if (TItemsGroup(round[nGroup]).Contains(nItem)) then
				begin
				// The current group contains the items!
				Inc(nGroup);
				end
			else
				begin
				// Neither the current group, nor any earlier group, contain the item. Find the group
				// later in the round which contains the item and swap these groups.
				for nGroupAfter:=(nGroup + 1) to (round.Count - 1) do
					begin
					if (TItemsGroup(round[nGroupAfter]).Contains(nItem)) then
						break;
					end;

				round.Exchange(nGroup, nGroupAfter);
				Inc(nGroup);
				end;

			// Onto the next item!
			Inc(nItem);
			end;
		end;
end;

procedure TfrmPuzzle0009.DbgCheckRoundForAllItems(round: TItemsGroupList);
var
	item, itemMissing: Integer;
begin
	// Helper function to check that the current round contains all the items (1,2,3,...,N)
	itemMissing := 0;
	for item:=1 to m_nTotalItems do
		begin
		if (not round.Contains(item)) then
			begin
			itemMissing := item;
			break;
			end;
		end;

	if (itemMissing > 0) then
		Application.MessageBox(
			PAnsiChar(Format('Round %d does not contain the value %d', [m_nNumRounds, itemMissing])),
		'Puzzle 0009', MB_ICONEXCLAMATION);
end;

procedure TfrmPuzzle0009.DbgCheckRoundsForAllConnections(rounds: TItemsGroupList);
var
	nFirst, nSecond, nGroup, nPair: Integer;
	bFoundConnection: Boolean;
	group: TItemsGroup;
	pairMissing: TConnectionPair;
	pairsMissing: TConnectionPairList;
	strMissingPairsMsg: String;
begin
	// Helper function to check that the solved puzzle correcting connects all items
	pairsMissing := TConnectionPairList.Create();
	for nFirst:=1 to m_nTotalItems do
		begin
		for nSecond:=(nFirst + 1) to m_nTotalItems do
			begin
			bFoundConnection := False;
			for nGroup:=0 to (rounds.Count - 1) do
				begin
				group := TItemsGroup(rounds[nGroup]);
				if (	(group.Contains(nFirst)) and
						(group.Contains(nSecond))) then
					begin
					bFoundConnection := True;
					break;
					end;
				end;

			if (not bFoundConnection) then
				begin
				pairMissing := TConnectionPair.Create();
				pairMissing.a := nFirst;
				pairMissing.b := nSecond;
				pairsMissing.Add(pairMissing);
				end;
			end;
		end;

	// Any missing pairs?
	if (pairsMissing.Count > 0) then
		begin
		strMissingPairsMsg := (
			Format('Puzzle solution has %d missing connection(s):', [pairsMissing.Count]) + #13#10);
		for nPair:=0 to (pairsMissing.Count - 1) do
			begin
			pairMissing := pairsMissing[nPair];
			strMissingPairsMsg := (strMissingPairsMsg + Format('  [%d,%d]', [
				pairMissing.a,
				pairMissing.b]));
			if (nPair <> (pairsMissing.Count - 1)) then
				strMissingPairsMsg := (strMissingPairsMsg + #13#10);
			end;

		Application.MessageBox(PAnsiChar(strMissingPairsMsg), 'Puzzle 0009', MB_ICONEXCLAMATION);
		end;

	pairsMissing.Free();
end;

function TfrmPuzzle0009.DbgGetConnectionPairs(pairs: TConnectionPairList) : String;
var
	strPairs: String;
	nPair: Integer;
	pair: TConnectionPair;
begin
	// Helper function to get the list of connection pairs as a string
	strPairs := '';
	for nPair:=0 to (pairs.Count - 1) do
		begin
		pair := TConnectionPair(pairs[nPair]);
		strPairs := (strPairs + Format('(%d,%d)', [pair.a, pair.b]));
		if (nPair <> (pairs.Count - 1)) then
			strPairs := (strPairs + ' ');
		end;

	Result := strPairs;
end;

function TfrmPuzzle0009.DbgGetGroupItems(group: TItemsGroup) : String;
var
	strItems: String;
	nItem: Integer;
begin
	// Helper function to get the items in a group as a string
	strItems := '(';
	for nItem:=0 to (group.items.Count - 1) do
		begin
		strItems := (strItems + IntToStr(Integer(group.items[nItem])));
		if (nItem <> (group.items.Count - 1)) then
			strItems := (strItems + ',');
		end;

	strItems := (strItems + ')');
	Result := strItems;
end;

function TfrmPuzzle0009.DbgGetRoundGroups(round: TItemsGroupList) : String;
var
	strGroups: String;
	nGroup: Integer;
	group: TItemsGroup;
begin
	// Helper function to get the items in a group as a string
	strGroups := '';
	for nGroup:=0 to (round.Count - 1) do
		begin
		group := TItemsGroup(round[nGroup]);
		strGroups := (strGroups + DbgGetGroupItems(group));
		end;

	Result := strGroups;
end;

function TfrmPuzzle0009.DbgGetRoundGroupsAll(rounds: TItemsGroupList; const cnRound: Integer) : String;
var
	strGroups: String;
	nGroup, nGroupsStart, nGroupsStop: Integer;
	group: TItemsGroup;
begin
	// Helper function to get the items in a group (from the total list of groups) as a string
	strGroups := '';
	if (	(cnRound >= 0) and
			(rounds.Count >= (cnRound * m_nGroupsPerRound))) then
		begin
		nGroupsStart := ((cnRound - 1) * m_nGroupsPerRound);
		nGroupsStop := ((cnRound * m_nGroupsPerRound) - 1);

		for nGroup:=nGroupsStart to nGroupsStop do
			begin
			group := TItemsGroup(rounds[nGroup]);
			strGroups := (strGroups + DbgGetGroupItems(group));
			if (nGroup <> nGroupsStop) then
				strGroups := (strGroups + ' ');
			end;
		end;

	Result := strGroups;
end;
// Private functions: End

end.

