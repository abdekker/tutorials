{ Example Delphi application to create a GUID }
unit CreateGuidForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Classes, ComObj, Controls, ExtCtrls, Forms, StdCtrls, SysUtils,
  SystemUtils;

type
  TfrmCreateGUID = class(TForm)
	gbGUID: TGroupBox;
	btnCreateGUID: TButton;
	lblCopiedToClipboard: TLabel;
	pnlGUID: TPanel;
	btnExit: TButton;

	procedure btnCreateGUIDClick(Sender: TObject);
	procedure btnExitClick(Sender: TObject);

  private
	{ Private declarations }

  public
	{ Public declarations }
  end;

var
  frmCreateGUID: TfrmCreateGUID;

implementation

{$R *.dfm}

procedure TfrmCreateGUID.btnCreateGUIDClick(Sender: TObject);
var
	guidNew: TGUID;
	strGUID: String;
begin
	// Create new GUID, format it, print to the screen and copy to the Windows clipboard
	OleCheck(CreateGUID(guidNew));
	strGUID := GUIDToString(guidNew);
	pnlGUID.Caption := strGUID;
	SaveToClipboard(strGUID);
end;

procedure TfrmCreateGUID.btnExitClick(Sender: TObject);
begin
	ModalResult := mrOk;
	Close();
end;

end.
