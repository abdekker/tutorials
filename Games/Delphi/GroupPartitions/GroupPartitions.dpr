program GroupPartitions;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

uses
  Forms,
  CoreFormClasses in '..\..\..\Languages\Delphi\Utils\CoreFormClasses.pas',
  CoreTypes in '..\..\..\Languages\Delphi\Utils\CoreTypes.pas',
  FormUtils in '..\..\..\Languages\Delphi\Utils\FormUtils.pas',
  SystemUtils in '..\..\..\Languages\Delphi\Utils\SystemUtils.pas',

  GroupPartitionsForm in 'GroupPartitionsForm.pas' {frmGroupPartitions};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGroupPartitions, frmGroupPartitions);
  Application.Run;
end.
