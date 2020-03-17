program DelphiClient;

uses
  Forms,
  DelphiClientForm in 'SimpleDllClientDelphi\DelphiClientForm.pas' {frmDelphiClient};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDelphiClient, frmDelphiClient);
  Application.Run;
end.
