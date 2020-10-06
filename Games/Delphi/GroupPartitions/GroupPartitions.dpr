program GroupPartitions;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

uses
{$IFDEF DBG}
  // Use "FastMM4" for debugging and memory-leak testing. Except for testing, leave this commented
  // out and only use for DEBUG builds.
  FastMM4,
{$ENDIF}
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
