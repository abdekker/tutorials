program CloudNine;

uses
  Forms,
  CloudNineMain in 'CloudNineMain.pas' {frmCloudNineMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCloudNineMain, frmCloudNineMain);
  Application.Run;
end.
