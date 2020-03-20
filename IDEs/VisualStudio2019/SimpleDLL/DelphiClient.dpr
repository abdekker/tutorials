program DelphiClient;

uses
  Forms,
  FormUtils in '..\..\..\Languages\Delphi\Utils\FormUtils.pas',
  DelphiClientForm in 'SimpleDllClientDelphi\DelphiClientForm.pas' {frmDelphiClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDelphiClient, frmDelphiClient);
  Application.Run;
end.
