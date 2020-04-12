{ Utilities designed for Windows TForm applications }
unit FormUtils;

interface

uses
  Windows, Classes, Controls, StdCtrls;

const
  DUMMY_INTERFACE_CONSTANT = 0;

type
  PDummyInterfacePointer = ^Integer;

// Public methods

// General controls
procedure DumpToFile(comp: TComponent; const cstrFile: String);
function GetWinControlPixelSize(wc: TWinControl; const strCaption: String) : TSize;
function GetGraphicControlPixelSize(gc: TGraphicControl; const strCaption: String) : TSize;

// TComboBox
procedure SetComboHandlers(control: TControl);
procedure SetChildComboHandlers(parent: TWinControl; nSubLevel: Integer = 0);

// TMemo
procedure ScrollMemoLastLine(handle: HWND; nLines: Integer);

implementation

uses
  Buttons, ComCtrls, Graphics, Messages, SysUtils;

const
  // Maximum level of nesting for iterating child controls (eg. setting character set)
  MAX_CHILD_CONTROL_ITERATIONS = 8;

type
  // Helper class which allows an (invisible) reference to Self to be pushed on the stack
  TEventHandlers = class
	procedure ddlComboDropDown(Sender: TObject);
	procedure ddlComboCloseUp(Sender: TObject);
  end;

var
  // Private variables are only modifiable from inside this unit
  eventHandlers: TEventHandlers;

// Start: Public methods
// General controls
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

function GetWinControlPixelSize(wc: TWinControl; const strCaption: String) : TSize;
var
	txtSize: TSize;
	handleDC: HDC;
begin
	// Return the size of text displayed on a TWinControl control (eg. TStaticText)
	txtSize.cx := 0;
	txtSize.cy := 0;

	// Get a device context (this can be cached to improve performance)
	handleDC := GetDC(0);

	// Assign a device context (DC) to the control depending on its type. Add control types
	// (derived from TWinControl) as required.
	if (wc is TStaticText) then
		SelectObject(handleDC, TStaticText(wc).Font.Handle)
	else if (wc is TMemo) then
		SelectObject(handleDC, TMemo(wc).Font.Handle)
	else if (wc is TListView) then
		SelectObject(handleDC, TListView(wc).Font.Handle);

	// Calculate the size this text will occupy on-screen (using the current font)
	GetTextExtentPoint32(handleDC, PChar(strCaption), Length(strCaption), txtSize);
	Result := txtSize;

	// Release the device context
	ReleaseDC(0, handleDC);
end;

function GetGraphicControlPixelSize(gc: TGraphicControl; const strCaption: String) : TSize;
var
	txtSize: TSize;
	handleDC: HDC;
begin
	// Return the size of text displayed on a TGraphicControl control (eg. TSpeedButton)
	txtSize.cx := 0;
	txtSize.cy := 0;
	if (gc is TSpeedButton) then
		begin
		// Get a device context (this can be cached to improve performance)
		handleDC := GetDC(0);

		// Assign the DC to the control depending on its type
		// Note: Currently only TSpeedButton is supported
		SelectObject(handleDC, TSpeedButton(gc).Font.Handle);

		// Calculate the size this text will occupy on-screen (using the current font)
		GetTextExtentPoint32(handleDC, PChar(strCaption), Length(strCaption), txtSize);

		// Release the device context
		ReleaseDC(0, handleDC);
		end;

	Result := txtSize;
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
