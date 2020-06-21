unit GameAbout;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Buttons, Controls, Classes, Forms, Messages, StdCtrls;

type
  TfrmGameAbout = class(TForm)
	btnOk: TBitBtn;
	gbNotes: TGroupBox;
	lblNotes: TLabel;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure btnOkClick(Sender: TObject);

  private
	{ Private declarations }
	procedure WMNCHitTest(var Msg: TMessage); message WM_NCHITTEST;

  public
	{ Public declarations }
  end;

var
  frmGameAbout: TfrmGameAbout;

implementation

uses
  Dialogs;

{$R *.dfm}

// Private functions: Start
procedure TfrmGameAbout.WMNCHitTest(var Msg: TMessage);
begin
	// Prevent Disclaimer from being moved around which causes refresh problems
	DefaultHandler(Msg);
	if (Msg.Result = Windows.HTCAPTION) or (Msg.Result = Windows.HTNOWHERE) then
		Msg.Result := Windows.HTCLIENT;
end;
// Private functions: End

// Public functions: Start
// Public functions: End

procedure TfrmGameAbout.FormCreate(Sender: TObject);
begin
	// Initialise form
	// Nothing to do...
end;

procedure TfrmGameAbout.FormDestroy(Sender: TObject);
begin
	// Clean up...
end;

procedure TfrmGameAbout.FormShow(Sender: TObject);
var
	strDisclaimer: String;
begin
	// Set the disclaimer
	strDisclaimer := ('This program was written for Bronwen Dekker on her birthday in June 2020!' + #13#10);
	strDisclaimer := (strDisclaimer + 'Enjoy!' + #13#10);
	lblNotes.Caption := strDisclaimer;

	// Set focus to the OK button
	btnOk.SetFocus();
end;

procedure TfrmGameAbout.btnOkClick(Sender: TObject);
begin
	ModalResult := mrOk;
end;

end.
