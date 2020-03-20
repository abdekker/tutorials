{ Utilities designed for Windows TForm applications }
unit FormUtils;

interface

uses
  Windows, Controls, StdCtrls;

const
  DUMMY_INTERFACE_CONSTANT = 0;

type
  PDummyInterfacePointer = ^Integer;

// Public methods

// TComboBox
procedure SetComboHandlers(control: TControl);
procedure SetChildComboHandlers(parent: TWinControl; nSubLevel: Integer = 0);

// TMemo
procedure ScrollMemoLastLine(handle: HWND; nLines: Integer);

implementation

uses
  Graphics, Messages;

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
