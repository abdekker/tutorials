program WindowsMetaFile;

uses
  Forms,
  CoreFormClasses in '..\Utils\CoreFormClasses.pas',
  CoreTypes in '..\Utils\CoreTypes.pas',
  FormUtils in '..\Utils\FormUtils.pas',
  SystemUtils in '..\Utils\SystemUtils.pas',

  WindowsMetaFileForm in 'WindowsMetaFileForm.pas' {frmWindowsMetaFile};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmWindowsMetaFile, frmWindowsMetaFile);
  Application.Run;
end.
