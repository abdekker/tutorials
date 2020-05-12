program SampleApplication;

uses
  Forms,
  SampleApplicationForm in 'SampleApplicationForm.pas' {frmSampleApplication},

  CoreFormClasses in '..\..\..\Languages\Delphi\Utils\CoreFormClasses.pas',
  FormUtils in '..\..\..\Languages\Delphi\Utils\FormUtils.pas',
  SystemUtils in '..\..\..\Languages\Delphi\Utils\SystemUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmSampleApplication, frmSampleApplication);
  Application.Run;
end.
