program CreateGUID;

uses
  Forms,
  SystemUtils in '..\..\..\Languages\Delphi\Utils\SystemUtils.pas',
  CreateGuidForm in 'CreateGuidForm.pas' {frmCreateGUID};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmCreateGUID, frmCreateGUID);
  Application.Run;
end.
