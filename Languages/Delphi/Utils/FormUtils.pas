{ Utilities designed for Windows TForm applications }
unit FormUtils;

interface

uses
  Windows, Classes, Controls, Forms, ExtCtrls, StdCtrls;

const
  // Control types
  CONTROL_TLABEL			= $0001;
  CONTROL_TEDIT				= $0002;
  CONTROL_TLABELEDEDIT		= $0004;
  CONTROL_TCHECKBOX			= $0008;	// Includes TCheckBox and TTntCheckBox
  CONTROL_TBUTTON			= $0010;
  CONTROL_TBITBTN			= $0020;
  CONTROL_TCOMBOBOX			= $0040;
  CONTROL_TRADIO			= $0080;
  CONTROL_TDATETIMEPICKER	= $0100;
  CONTROL_TIMAGE			= $0200;
  CONTROL_TPANEL			= $0400;
  CONTROL_ALL				= (
	CONTROL_TLABEL +
	CONTROL_TEDIT +
	CONTROL_TLABELEDEDIT +
	CONTROL_TCHECKBOX +
	CONTROL_TBUTTON +
	CONTROL_TBITBTN +
	CONTROL_TCOMBOBOX +
	CONTROL_TRADIO +
	CONTROL_TDATETIMEPICKER +
	CONTROL_TIMAGE +
	CONTROL_TPANEL);

  // Image types
  IMAGE_JPG		= 1;
  IMAGE_BMP		= 2;

type
  // Type pointers
  PTForm = ^TForm;

// Public methods

// Initialisation and cache
procedure InitialiseFormUtils();
procedure CloseFormUtils();

// General
procedure LockControl(control: TWinControl; bLock: Boolean);
procedure DumpToFile(comp: TComponent; const cstrFile: String);
function IsControlType(control: TControl; wControls: WORD) : Boolean;
procedure SetSubControlsEnabled(parent: TWinControl; wControls: WORD; bEnabled: Boolean;
	nSubLevel: Integer = 0);
procedure SetSubBackColour(parent: TWinControl; nSubLevel: Integer = 0);
function GetChildIndex(parent, child: TWinControl) : Integer;
function GetCloseControlClick(parent: TWinControl; wControls: WORD; nIgnoreControl: Integer = -1) : Integer;
function GetWinControlPixelSize(wc: TWinControl; const strCaption: String) : TSize;
function GetGraphicControlPixelSize(gc: TGraphicControl; const strCaption: String) : TSize;
procedure ConfigureMultiLineLabel(lblLabel: TLabel; const cstrLabel: String;
	const cnMaxLines: Integer; var nLines: Integer; var nSizeY: Integer);
procedure SaveScreenshot(pSubScreen: PTForm; const cstrFile: String; const cnImgType: Integer);

// TEdit
procedure SelectAllText(edit: TCustomEdit);

// TComboBox
procedure SetComboHandlers(control: TControl);
procedure SetChildComboHandlers(parent: TWinControl; nSubLevel: Integer = 0);

// TListBox and TListView
function GetListVisibleRows(list: TCustomListControl) : Integer;

// TMemo
procedure ScrollMemoLastLine(handle: HWND; nLines: Integer);

implementation

uses
  Buttons, ComCtrls, Graphics, jpeg, Messages, StrUtils, SysUtils;

const
  // Maximum level of nesting for iterating child controls (eg. setting character set)
  MAX_CHILD_CONTROL_ITERATIONS = 8;

type
  // Helper class which allows an (invisible) reference to Self to be pushed on the stack
  TEventHandlers = class
	procedure ddlComboDropDown(Sender: TObject);
	procedure ddlComboCloseUp(Sender: TObject);
  end;

  // Cache
  CACHE_FORM_UTILS = record
	// Generic device context
	handleDC: HDC;
  end;

// Private variables (only modifiable from inside this unit)
var
  eventHandlers: TEventHandlers;
  m_cache: CACHE_FORM_UTILS;

// Start: Public methods

// Initialisation and cache
procedure InitialiseFormUtils();
begin
	// Initialise the cache (and other private variables used in this unit)
	ZeroMemory(@m_cache, SizeOf(CACHE_FORM_UTILS));

	// Generic device context. Used to calculate the size of on-screen text. To use call:
	//		SelectObject(m_cache.handleDC, HANDLE);
	m_cache.handleDC := GetDC(0);
end;

procedure CloseFormUtils();
begin
	// Generic device context
	ReleaseDC(0, m_cache.handleDC);
end;

// General / System
procedure LockControl(control: TWinControl; bLock: Boolean);
begin
	// Prevents the control from redrawing (for example, while it is being resized)

	// Note to developer: Avoid using this method in the form's OnShow event as in:
	//		LockControl(frmBlah, True);
	//		SomeCode();
	//		LockControl(frmBlah, False);
	// This is because the Position property (usually "poScreenCenter") only gets applied late in
	// the form's construction. Forcing a redraw results in flickering as the form gets drawn at
	// the design-time position (wherever the developer last left it when the IDE was open) and
	// then re-drawn at the runtime position of the screen centre.

	// If used before the end of OnShow, use "SetFormRuntimePos(Self)" in the form's OnCreate event
	// to ensure the correct runtime position is set early.

	// Alternatively, use flag to decide whether to use LockControl. The flag is set when the form
	// (or the section being locked) has been fully initialised.
	if (control = nil) or (control.Handle = 0) then
		Exit;

	if (bLock) then
		SendMessage(control.Handle, WM_SETREDRAW, 0, 0)
	else
		begin
		SendMessage(control.Handle, WM_SETREDRAW, 1, 0);
		RedrawWindow(control.Handle, nil, 0,
			(RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN));
		end;
end;

procedure DumpToFile(comp: TComponent; const cstrFile: String);
var
	strmObject: TFileStream;
begin
	// Debugging utility to save an object (derived from TComponent) to file. Example usage:
	//		DumpToFile(Self, Format('C:\Tmp\Dbg%.3d.txt', [SOME_NUMBER]));
	strmObject := TFileStream.Create(cstrFile, (fmCreate or fmShareDenyNone));
	try
		strmObject.WriteComponent(comp);
	finally
		strmObject.Free();
	end;
end;

function IsControlType(control: TControl; wControls: WORD) : Boolean;
var
	bControlIsType: Boolean;
begin
	// Check whether the control type is one we are interested in
	bControlIsType := False;
	if ((wControls and CONTROL_TLABEL) <> 0) and (control is TLabel) then
		bControlIsType := True
	else if ((wControls and CONTROL_TEDIT) <> 0) and (control is TEdit) then
		bControlIsType := True
	else if ((wControls and CONTROL_TLABELEDEDIT) <> 0) and (control is TLabeledEdit) then
		bControlIsType := True
	else if ((wControls and CONTROL_TCHECKBOX) <> 0) and (control is TCheckBox) then
		bControlIsType := True
	else if ((wControls and CONTROL_TBUTTON) <> 0) and (control is TButton) then
		bControlIsType := True
	else if ((wControls and CONTROL_TBITBTN) <> 0) and (control is TBitBtn) then
		bControlIsType := True
	else if ((wControls and CONTROL_TCOMBOBOX) <> 0) and (control is TComboBox) then
		bControlIsType := True
	else if ((wControls and CONTROL_TRADIO) <> 0) and (control is TRadioButton) then
		bControlIsType := True
	else if ((wControls and CONTROL_TDATETIMEPICKER) <> 0) and (control is TDateTimePicker) then
		bControlIsType := True
	else if ((wControls and CONTROL_TIMAGE) <> 0) and (control is TImage) then
		bControlIsType := True
	else if ((wControls and CONTROL_TPANEL) <> 0) and (control is TPanel) then
		bControlIsType := True;

	Result := bControlIsType;
end;

procedure SetSubControlsEnabled(parent: TWinControl; wControls: WORD; bEnabled: Boolean;
	nSubLevel: Integer = 0);
var
	nControl: Integer;
begin
	// See "EnumSubControlsCharset" for further comments
	if (parent <> nil) and (nSubLevel < MAX_CHILD_CONTROL_ITERATIONS) then
		begin
		for nControl:=0 to (parent.ControlCount - 1) do
			begin
			if (IsControlType(parent.Controls[nControl], wControls)) then
				parent.Controls[nControl].Enabled := bEnabled
			else
				begin
				// None of the above...if it's a group box, recursively check its' child controls
				if (parent.Controls[nControl] is TWinControl) then
					SetSubControlsEnabled(TWinControl(parent.Controls[nControl]),
						wControls, bEnabled, (nSubLevel + 1));
				end;
			end;
		end;
end;

procedure SetSubBackColour(parent: TWinControl; nSubLevel: Integer = 0);
var
	nControl: Integer;
begin
	// Enumerate the TWinControl-derived control (typically a group box). Set the back colour for
	// Edit and Combo controls to better show they are disabled:
	// * Enabled = clInfoBk
	// * Disabled = clSkyBlue
	if (parent <> nil) and (nSubLevel < MAX_CHILD_CONTROL_ITERATIONS) then
		begin
		for nControl:=0 to (parent.ControlCount - 1) do
			begin
			if (parent.Controls[nControl] is TEdit) then
				begin
				// Edit
				if (TEdit(parent.Controls[nControl]).Enabled) then
					TEdit(parent.Controls[nControl]).Color := clInfoBk
				else
					TEdit(parent.Controls[nControl]).Color := clSkyBlue;
				end
			else if (parent.Controls[nControl] is TComboBox) then
				begin
				// Combo box
				if (TComboBox(parent.Controls[nControl]).Enabled) then
					begin
					if (TComboBox(parent.Controls[nControl]).DroppedDown) then
						TComboBox(parent.Controls[nControl]).Color := clSkyBlue
					else
						TComboBox(parent.Controls[nControl]).Color := clInfoBk;
					end
				else
					TComboBox(parent.Controls[nControl]).Color := clSkyBlue;
				end
			else if (parent.Controls[nControl] is TDateTimePicker) then
				begin
				// Date/Time picker
				if (TDateTimePicker(parent.Controls[nControl]).Enabled) then
					begin
					if (TDateTimePicker(parent.Controls[nControl]).DroppedDown) then
						TDateTimePicker(parent.Controls[nControl]).Color := clSkyBlue
					else
						TDateTimePicker(parent.Controls[nControl]).Color := clInfoBk;
					end
				else
					TDateTimePicker(parent.Controls[nControl]).Color := clSkyBlue;
				end
			else
				begin
				// None of the above...if it's a group box, recursively check its' child controls
				if (parent.Controls[nControl] is TWinControl) then
					SetSubBackColour(TWinControl(parent.Controls[nControl]), (nSubLevel + 1));
				end;
			end;
		end;
end;

function GetChildIndex(parent, child: TWinControl) : Integer;
var
	bFoundChild: Boolean;
	nControl: Integer;
begin
	// Find the index of the given child in the list of controls owned by a parent (eg. a TPanel or
	// TGroupBox). Return "-1" if the child is not found.
	nControl := -1;
	bFoundChild := False;
	if ((parent <> nil) and (child <> nil)) then
		begin
		for nControl:=0 to (parent.ControlCount - 1) do
			begin
			if (	(parent.Controls[nControl] is TCustomControl) and
					(parent.Controls[nControl].Name = child.Name)) then
				begin
				// Found it!
				bFoundChild := True;
				break;
				end;
			end;
		end;

	if (bFoundChild) then
		Result := nControl
	else
		Result := -1;
end;

function GetCloseControlClick(parent: TWinControl; wControls: WORD; nIgnoreControl: Integer = -1) : Integer;
var
	nControl, nLeft, nRight, nTop, nBottom: Integer;
	nControlIndex: Integer;
	bPotentialControl, bCloseX, bCloseY: Boolean;
	pt: TPoint;
begin
	// Find any control (within a TGroupBox or TPanel) and determine whether it is close to the
	// user's click point. Especially useful for TCheckBox and TRadioButton since operating these
	// controls can be awkward on a touchscreen.
	nControlIndex := -1;
	if (parent <> nil) then
		begin
		// Where did the user click?
		pt := TWinControl(parent).ScreenToClient(Mouse.CursorPos);
		for nControl:=0 to (parent.ControlCount - 1) do
			begin
			// Ignore this control?
			if (nIgnoreControl > -1) and (nControl = nIgnoreControl) then
				continue;

			// Control must be both visible and enabled!
			bPotentialControl :=
				(parent.Controls[nControl].Visible) and (parent.Controls[nControl].Enabled);

			// Is this control's type one that the caller is interested in?
			if (bPotentialControl) then
				bPotentialControl := IsControlType(parent.Controls[nControl], wControls);

			// Control must be close to the users' click point
			if (bPotentialControl) then
				begin
				// Get the controls' bounding rectangle
				nLeft := parent.Controls[nControl].Left;
				nRight := (nLeft + parent.Controls[nControl].Width);

				nTop := parent.Controls[nControl].Top;
				nBottom := (nTop + parent.Controls[nControl].Height);

				// Set the X direction to be slightly more sensitive than the Y direction
				bCloseX := (pt.X >= (nLeft - 6)) and (pt.X < (nRight + 6));
				bCloseY := (pt.Y >= (nTop - 4)) and (pt.Y < (nBottom + 4));
				if (bCloseX) and (bCloseY) then
					begin
					// Got it!
					nControlIndex := nControl;
					break;
					end;
				end;
			end;
		end;

	Result := nControlIndex;
end;

function GetWinControlPixelSize(wc: TWinControl; const strCaption: String) : TSize;
var
	txtSize: TSize;
begin
	// Return the size of text displayed on a TWinControl control (eg. TStaticText)
	txtSize.cx := 0;
	txtSize.cy := 0;

	// Assign a device context (DC) to the control depending on its type. Add control types
	// (derived from TWinControl) as required.
	if (wc is TEdit) then
		SelectObject(m_cache.handleDC, TEdit(wc).Font.Handle)
	else if (wc is TStaticText) then
		SelectObject(m_cache.handleDC, TStaticText(wc).Font.Handle)
	else if (wc is TMemo) then
		SelectObject(m_cache.handleDC, TMemo(wc).Font.Handle)
	else if (wc is TListView) then
		SelectObject(m_cache.handleDC, TListView(wc).Font.Handle);

	// Calculate the size this text will occupy on-screen (using the current font)
	GetTextExtentPoint32(m_cache.handleDC, PChar(strCaption), Length(strCaption), txtSize);
	Result := txtSize;
end;

function GetGraphicControlPixelSize(gc: TGraphicControl; const strCaption: String) : TSize;
var
	txtSize: TSize;
begin
	// Return the size of text displayed on a TGraphicControl control (eg. TSpeedButton)
	txtSize.cx := 0;
	txtSize.cy := 0;

	// Assign a device context (DC) to the control depending on its type. Add control types
	// (derived from TWinControl) as required.
	if (gc is TLabel) then
		SelectObject(m_cache.handleDC, TLabel(gc).Font.Handle)
	else if (gc is TSpeedButton) then
		SelectObject(m_cache.handleDC, TSpeedButton(gc).Font.Handle);

	// Calculate the size this text will occupy on-screen (using the current font)
	GetTextExtentPoint32(m_cache.handleDC, PChar(strCaption), Length(strCaption), txtSize);
	Result := txtSize;
end;

procedure ConfigureMultiLineLabel(lblLabel: TLabel; const cstrLabel: String;
	const cnMaxLines: Integer; var nLines: Integer; var nSizeY: Integer);
var
	txtSizeLabel, txtSizeChar, txtSizeLastSpace: TSize;
	strChar: String;
	nChar: Integer;
begin
	// Used for TLabel controls that will display a string of variable length. The TLabel control
	// needs to have these properties:
	// * AutoSize set to False (ie. fixed width)
	// * WordWrap set to True

	// Assign the font label to our cached DC
	SelectObject(m_cache.handleDC, lblLabel.Font.Handle);

	// Now calculate how many lines the label will occupy
	nLines := 1;
	nSizeY := 20;	// This will be updated in the loop below
	txtSizeLabel.cx := 0;
	txtSizeChar.cy := 0;
	txtSizeLastSpace.cx := 0;
	for nChar:=1 to Length(cstrLabel) do
		begin
		strChar := AnsiMidStr(cstrLabel, nChar, 1);
		GetTextExtentPoint32(m_cache.handleDC, PChar(strChar), 1, txtSizeChar);
		Inc(txtSizeLabel.cx, txtSizeChar.cx);
		Inc(txtSizeLastSpace.cx, txtSizeChar.cx);
		if (strChar = ' ') or (strChar = #13) then
			txtSizeLastSpace.cx := 0;

		if (txtSizeLabel.cx > lblLabel.ClientWidth) or (strChar = #13) then
			begin
			// Text will exceed the current line length (or a newline character)
			nLines := (nLines + 1);
			txtSizeLabel.cx := txtSizeLastSpace.cx;
			end;

		// Don't allow the number of lines to exceed the maximum allowed
		if (nLines >= cnMaxLines) then
			break;
		end;

	// Set the vertical size required for each character on this label (and font)
	nSizeY := txtSizeChar.cy;
end;

procedure SaveScreenshot(pSubScreen: PTForm; const cstrFile: String; const cnImgType: Integer);
var
	rectRegion: TRect;
	bmpScreenShot: TBitmap;
	jpgScreenShot: TJPEGImage;
	nRasterCaps: Integer;
	lpPalette: PLOGPALETTE;
begin
	// Example usage:
	//		SaveScreenshot(@Self, 'C:\Tmp\Screenshot.bmp');

	// Set up a rectangle using the form's on-screen location and size
	// Note: This code came from About.com: Delphi Programming by Zarko Gajic
	rectRegion.Left := pSubScreen.Left;
	rectRegion.Top := pSubScreen.Top;
	rectRegion.Right := (pSubScreen.Left + pSubScreen.Width);
	rectRegion.Bottom := (pSubScreen.Top + pSubScreen.Height);

	// Set up bitmap for screenshot
	bmpScreenShot := TBitmap.Create();
	bmpScreenShot.Width := (rectRegion.Right - rectRegion.Left);
	bmpScreenShot.Height := (rectRegion.Bottom - rectRegion.Top);

	// Do we have a palette device?
	// Note to developer: On the developer desktop, the capability of the graphics comes back as
	// $7E99 ie. the RC_PALETTE bit is not set.
	nRasterCaps := GetDeviceCaps(m_cache.handleDC, RASTERCAPS);
	if ((nRasterCaps and RC_PALETTE) = RC_PALETTE) then
		begin
		// Allocate memory for a logical palette
		GetMem(lpPalette, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));

		// Zero it out to be neat
		FillChar(lpPalette^, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)), #0);

		// Fill in the palette version
		lpPalette^.palVersion := $300;

		// Grab the system palette entries
		lpPalette^.palNumEntries := GetSystemPaletteEntries(
			m_cache.handleDC, 0, 256, lpPalette^.palPalEntry);

		// Create the palette?
		if (lpPalette^.palNumEntries <> 0) then
			bmpScreenShot.Palette := CreatePalette(lpPalette^);

		// Clean up memory
		FreeMem(lpPalette, SizeOf(TLOGPALETTE) + (255 * SizeOf(TPALETTEENTRY)));
	end;

	// Copy the screen to the bitmap
	BitBlt(bmpScreenShot.Canvas.Handle,
		0, 0, bmpScreenShot.Width, bmpScreenShot.Height, m_cache.handleDC,
		rectRegion.Left, rectRegion.Top, SRCCOPY);

	// Save the image to disk
	// Note: JPG format is much smaller and faster
	if (cnImgType = IMAGE_JPG) then
		begin
		// Create a jpeg image, assign to it and save
		jpgScreenShot := TJPEGImage.Create();
			try
				jpgScreenShot.Assign(bmpScreenShot);
				jpgScreenShot.SaveToFile(cstrFile);
			finally
				jpgScreenShot.Free();
			end;
		end
	else if (cnImgType = IMAGE_BMP) then
		bmpScreenShot.SaveToFile(cstrFile);

	// Clean up memory
	bmpScreenShot.Free();
end;

// TEdit
procedure SelectAllText(edit: TCustomEdit);
begin
	// Select all the text in a TCustomEdit control (usually a TEdit). Other controls which derive
	// from TCustomEdit include TCustomMemo.
	LockControl(edit, True);
	SendMessage(edit.Handle, WM_LBUTTONDOWN, MK_LBUTTON, MAKELPARAM(edit.Width, 0));
	SendMessage(edit.Handle, WM_LBUTTONUP, 0, MAKELPARAM(edit.Width, 0));

	edit.SetFocus();
	edit.SelectAll();
	LockControl(edit, False);
end;

// TComboBox
procedure SetComboHandlers(control: TControl);
begin
	// Set up the OnDropDown and OnCloseUp handlers for a combobox
	// Note: See "SetChildComboHandlers" to iteratively set handlers for container controls (such
	// as a groupbox) which have many child combox or group controls
	TComboBox(control).OnDropDown := eventHandlers.ddlComboDropDown;
	TComboBox(control).OnCloseUp := eventHandlers.ddlComboCloseUp;
end;

procedure SetChildComboHandlers(parent: TWinControl; nSubLevel: Integer = 0);
var
	nControl: Integer;
begin
	// Variant of "SetComboHandlers" which iterates all child controls of some parent container
	// (eg. TGroupBox or TPanel). If the child is a TComboBox, set handlers. Useful when the parent
	// has many child combox controls (if not, just call "SetComboHandlers" directly).
	if (parent <> nil) and (nSubLevel < MAX_CHILD_CONTROL_ITERATIONS) then
		begin
		for nControl:=0 to (parent.ControlCount - 1) do
			begin
			if (parent.Controls[nControl] is TComboBox) then
				begin
				// Combobox, so set the handlers
				SetComboHandlers(parent.Controls[nControl]);
				end
			else
				begin
				// Not a combobox, so recursively check its' child controls
				if (parent.Controls[nControl] is TWinControl) then
					SetChildComboHandlers(TWinControl(parent.Controls[nControl]), (nSubLevel + 1));
				end;
			end;
		end;
end;

// TListBox and TListView
function GetListVisibleRows(list: TCustomListControl) : Integer;
var
	nVisibleRows: Integer;
	txtSize: TSize;
begin
	// Calculate the number of visible rows in TListBox and TListView controls
	nVisibleRows := 0;
	if (list is TListBox) then
		begin
		// TListBox
		// Note to developer: If you use "Round", this will return the number of rows that are
		// fully AND partially visible. "Trunc" returns the number of visible rows before a
		// vertical scrollbar is required.
		nVisibleRows := Trunc(TListBox(list).ClientHeight / TListBox(list).ItemHeight);
		end
	else if (list is TListView) then
		begin
		// TListView
		txtSize := GetWinControlPixelSize(list, 'Xy');
		nVisibleRows := Trunc((TListView(list).ClientHeight - 6) / (txtSize.cy + 1));

		// Note to developer: An alternative method is to create a temporary TListView, and work
		// out the space required to display a single item in the list. This requires the caller
		// to add a parameter for the parent form ie. "form: TForm". While this works, it is ~200
		// times slower.
		end;

	Result := nVisibleRows;
end;

// TMemo
procedure ScrollMemoLastLine(handle: HWND; nLines: Integer);
begin
	SendMessage(handle, EM_LINESCROLL, 0, nLines);
end;
// End: Public methods

// Start: Private methods
procedure TEventHandlers.ddlComboDropDown(Sender: TObject);
begin
	// Modify the combo background colour (when extended)
	// Note: Assumes the colour combination clInfoBk / clSkyBlue
	TComboBox(Sender).Color := clSkyBlue;
end;

procedure TEventHandlers.ddlComboCloseUp(Sender: TObject);
begin
	// Reset the background colour
	TComboBox(Sender).Color := clInfoBk;
end;
// End: Private methods

end.
