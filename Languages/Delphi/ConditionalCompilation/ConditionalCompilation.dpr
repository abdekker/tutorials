program ConditionalCompilation;

uses
  Forms,
  ConditionalForm in 'ConditionalForm.pas' {frmConditional};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmConditional, frmConditional);
  Application.Run;
end.
