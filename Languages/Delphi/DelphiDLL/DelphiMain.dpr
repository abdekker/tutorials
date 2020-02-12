program DelphiMain;

uses
  Forms,
  DelphiMainForm in 'DelphiMainForm.pas' {frmDelphiMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDelphiMain, frmDelphiMain);
  Application.Run;
end.
