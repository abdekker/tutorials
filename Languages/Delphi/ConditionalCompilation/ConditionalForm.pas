unit ConditionalForm;
{ Symbols are defined using $DEFINE and cleared with $UNDEF. This operates strictly top-to-bottom.
The pre-processing occurs once only at compile-time (ie. a symbol cannot be defined at runtime). }

// To use a symbol for drawing text in the grid, toggle this comment and re-compile
{$DEFINE ALL_TEXT_CENTRE_ALIGNED}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids;

type
  TfrmConditional = class(TForm)
	gbOutput: TGroupBox;
	btnExit: TButton;
    gridDefined: TStringGrid;
    lblOutput1: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;

	procedure FormShow(Sender: TObject);
	procedure gridDefinedDrawCell(Sender: TObject; col, row: Integer; Rect: TRect; State: TGridDrawState);
	procedure btnExitClick(Sender: TObject);

  private
	{ Private declarations }
	procedure UpdateGrid();
	procedure UpdateRow(row: Integer; abDefined: array of Boolean);

  public
	{ Public declarations }
  end;

var
  frmConditional: TfrmConditional;

implementation

{$R *.dfm}

procedure TfrmConditional.FormShow(Sender: TObject);
begin
	gridDefined.Cols[0].Text := '1';
	gridDefined.Cols[1].Text := '2';
	gridDefined.Cols[2].Text := '1 or 2';
	UpdateGrid();
end;

procedure TfrmConditional.gridDefinedDrawCell(Sender: TObject; col, row: Integer; Rect: TRect;
	State: TGridDrawState);
{$IFNDEF ALL_TEXT_CENTRE_ALIGNED}
var
	szLocal: string;
	wSavedAlign: WORD;
{$ENDIF}
begin
{$IFDEF ALL_TEXT_CENTRE_ALIGNED}
	// Set all text centre-aligned
	SetTextAlign(gridDefined.Canvas.Handle, TA_CENTER);
	gridDefined.Canvas.TextRect(Rect,
		Rect.Left + (Rect.Right - Rect.Left) div 2, Rect.Top + 2, gridDefined.Cells[col, row]);
{$ELSE}
	// Set just the header row text centre-aligned
	if (row = 0) then
		begin
		szLocal := gridDefined.Cells[col, row];
		wSavedAlign := SetTextAlign(gridDefined.Canvas.Handle, TA_CENTER);
		SetTextAlign(gridDefined.Canvas.Handle, TA_CENTER);
		gridDefined.Canvas.TextRect(Rect,
			Rect.Left + (Rect.Right - Rect.Left) div 2, Rect.Top + 2, szLocal);
		SetTextAlign(gridDefined.Canvas.Handle, wSavedAlign);
		end;
{$ENDIF}
end;

procedure TfrmConditional.btnExitClick(Sender: TObject);
begin
	Close();
end;

{ Private functions: Start }
procedure TfrmConditional.UpdateGrid();
var
	abDefined: array[0..2] of Boolean;
begin
	// Define both symbols
{$DEFINE A1}
{$DEFINE A2}
	// Use $IF with $IFEND
	abDefined[0] := {$IF Defined(A1)} True {$ELSE} False {$IFEND};
	abDefined[1] := {$IF Defined(A2)} True {$ELSE} False {$IFEND};
	abDefined[2] := {$IF Defined(A1) or Defined(A2)} True {$ELSE} False {$IFEND};
	UpdateRow(1, abDefined);

	// Use $IFDEF with $ENDIF
	abDefined[0] := {$IFDEF A1} True {$ELSE} False {$ENDIF};
	abDefined[1] := {$IFDEF A2} True {$ELSE} False {$ENDIF};
	abDefined[2] :=
		{$IFDEF A1}
			True
		{$ELSE}
			{$IFDEF A2} True {$ELSE} False {$ENDIF}
		{$ENDIF};
	UpdateRow(2, abDefined);

	// Define 1st symbol only
{$DEFINE B1}
{$IFDEF B2}
	{$UNDEF B2}
{$ENDIF}
	abDefined[0] := {$IF Defined(B1)} True {$ELSE} False {$IFEND};
	abDefined[1] := {$IF Defined(B2)} True {$ELSE} False {$IFEND};
	abDefined[2] := {$IF Defined(B1) or Defined(B2)} True {$ELSE} False {$IFEND};
	UpdateRow(3, abDefined);

	// Define 2nd symbol only
{$IFDEF C1}
	{$UNDEF C1}
{$ENDIF}
{$DEFINE C2}
	abDefined[0] := {$IF Defined(C1)} True {$ELSE} False {$IFEND};
	abDefined[1] := {$IF Defined(C2)} True {$ELSE} False {$IFEND};
	abDefined[2] := {$IF Defined(C1) or Defined(C2)} True {$ELSE} False {$IFEND};
	UpdateRow(4, abDefined);

	// Define neither symbols
{$IFDEF D1} {$UNDEF D1} {$ENDIF}
{$IFDEF D2} {$UNDEF D2} {$ENDIF}
	abDefined[0] := {$IF Defined(D1)} True {$ELSE} False {$IFEND};
	abDefined[1] := {$IF Defined(D2)} True {$ELSE} False {$IFEND};
	abDefined[2] := {$IF Defined(D1) or Defined(D2)} True {$ELSE} False {$IFEND};
	UpdateRow(5, abDefined);
end;

procedure TfrmConditional.UpdateRow(row: Integer; abDefined: array of Boolean);
var
	nColumn: Integer;
begin
	// Column 0		1st symbol
	// Column 1		2nd symbol
	// Column 2		1st or 2nd symbol
	for nColumn:=0 to 2 do
		begin
		if (abDefined[nColumn]) then
			gridDefined.Cells[nColumn, row] := 'Y'
		else
			gridDefined.Cells[nColumn, row] := 'N';
		end;
end;
{ Private functions: End }

end.
