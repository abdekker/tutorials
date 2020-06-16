unit Disclaimer;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Buttons, Classes, ComCtrls, Controls, ExtCtrls, Forms, Graphics, StdCtrls,
  SysUtils,
  SystemUtils;

type
  TfrmDisclaimer = class(TForm)
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
  frmDisclaimer: TfrmDisclaimer;

implementation

uses
  Dialogs;

{$R *.dfm}

// Private functions: Start
procedure TfrmDisclaimer.WMNCHitTest(var Msg: TMessage);
begin
	// Prevent Disclaimer from being moved around which causes refresh problems
	DefaultHandler(Msg);
	if (Msg.Result = Windows.HTCAPTION) or (Msg.Result = Windows.HTNOWHERE) then
		Msg.Result := Windows.HTCLIENT;
end;
// Private functions: End

// Public functions: Start
// Public functions: End

procedure TfrmDisclaimer.FormCreate(Sender: TObject);
begin
	// Initialise form
	// Nothing to do...
end;

procedure TfrmDisclaimer.FormDestroy(Sender: TObject);
begin
	// Clean up...
end;

procedure TfrmDisclaimer.FormShow(Sender: TObject);
var
	strDisclaimer: String;
begin
	// Set the disclaimer
	strDisclaimer := ('This program was written for Sarah Dekker on her 2nd birthday in September 2010!' + #13#10);
	strDisclaimer := (strDisclaimer + 'The programmer was her father, Alain Dekker.' + #13#10);
	strDisclaimer := (strDisclaimer + 'You may use this program freely but must keep this disclaimer.' + #13#10 + #13#10);
	strDisclaimer := (strDisclaimer + 'Contact: (source code / donations) email: abdekker.online@gmail.com.' + #13#10 + #13#10);

	strDisclaimer := (strDisclaimer + 'You may add your own images. Only JPG and BMP formats are supported, they can be any size.' + #13#10);
	strDisclaimer := (strDisclaimer + 'To use your own images, delete the contents of the folders below:' + #13#10);

	strDisclaimer := (strDisclaimer + '   Internal (Default) - "General Images"' + #13#10);
	strDisclaimer := (strDisclaimer + '   Numbers - "Numbers"' + #13#10);
	strDisclaimer := (strDisclaimer + '   Letters - "Letters"' + #13#10);
	strDisclaimer := (strDisclaimer + 'Or just simply point to any folder you like.' + #13#10);
	strDisclaimer := (strDisclaimer + 'For the best results, use images where the Width is about the same as the Height.' + #13#10);
	strDisclaimer := (strDisclaimer + 'The maximum grid size is 8x8 requires 32 images, but you can use any number of images.' + #13#10);
	lblNotes.Caption := strDisclaimer;

	// Set focus to the OK button
	btnOk.SetFocus();
end;

procedure TfrmDisclaimer.btnOkClick(Sender: TObject);
begin
	ModalResult := mrOk;
end;

end.
