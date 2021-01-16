program AlfredsWykeSimulator;

uses
  Forms,
  AWSimulatorForm in 'AWSimulatorForm.pas' {AWSimulatorForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmAWSimulator, frmAWSimulator);
  Application.Run;
end.
