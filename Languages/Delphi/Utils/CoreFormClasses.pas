{ Base class(es) for TForm classes }
unit CoreFormClasses;
{$I CoreOptions.inc}

interface

uses
  Windows, Classes, Controls, Forms;

type
  TGeneralBaseForm = class(TForm)
  private
	{ Private declarations }

	// ComboBox event handlers
	procedure ddlComboDropDown(Sender: TObject);
	procedure ddlComboCloseUp(Sender: TObject);

  protected
	{ Protected declarations }

  public
	{ Public declarations }
	constructor Create(AOwner: TComponent); override;
	destructor Destroy(); override;

	// Control event handlers
	// Note: Only if the form is derived from TLomaBaseForm. For forms derived from TForm,
	// implement these event handlers manually.
	procedure SetComboHandlers(control: TControl);
	procedure SetChildComboHandlers(parent: TWinControl; nSubLevel: Integer = 0);
  end;
  TGeneralBase = class of TGeneralBaseForm;

implementation

uses
  Graphics, StdCtrls;

const
  // Maximum level of nesting for iterating child controls (eg. setting character set)
  MAX_CHILD_CONTROL_ITERATIONS = 8;

// Public functions: Start
constructor TGeneralBaseForm.Create(AOwner: TComponent);
begin
	// Call base constructor
	inherited;

	// Add additional constructor code here...
end;

destructor TGeneralBaseForm.Destroy();
begin
	// Additional destructor code here...

	// Call base destructor
	inherited;
end;

procedure TGeneralBaseForm.SetComboHandlers(control: TControl);
begin
	// Set up the OnDropDown and OnCloseUp handlers for a combobox
	// Note: See "SetChildComboHandlers" to iteratively set handlers for container controls (such
	// as a groupbox) which have many child combo or group controls
	TComboBox(control).OnDropDown := ddlComboDropDown;
	TComboBox(control).OnCloseUp := ddlComboCloseUp;
end;

procedure TGeneralBaseForm.SetChildComboHandlers(parent: TWinControl; nSubLevel: Integer = 0);
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
// Public functions: End

// Private functions: Start
procedure TGeneralBaseForm.ddlComboDropDown(Sender: TObject);
begin
	// Modify the combo background colour (when extended)
	TComboBox(Sender).Color := clSkyBlue;
end;

procedure TGeneralBaseForm.ddlComboCloseUp(Sender: TObject);
begin
	// Reset the background colour
	TComboBox(Sender).Color := clInfoBk;
end;
// Private functions: End

end.

