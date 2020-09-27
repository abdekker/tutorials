program DelphiPuzzles;
{$I ..\..\Languages\Delphi\Utils\CoreOptions.inc}

uses
  Forms,
  CoreFormClasses in '..\..\Languages\Delphi\Utils\CoreFormClasses.pas',
  CoreTypes in '..\..\Languages\Delphi\Utils\CoreTypes.pas',
  FormUtils in '..\..\Languages\Delphi\Utils\FormUtils.pas',
  SystemUtils in '..\..\Languages\Delphi\Utils\SystemUtils.pas',

  PuzzlesMain in 'PuzzlesMain.pas' {frmPuzzlesMain},
  puzzle0001 in 'puzzle0001.pas' {frmPuzzle0001},
  puzzle0008 in 'puzzle0008.pas' {frmPuzzle0008};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPuzzlesMain, frmPuzzlesMain);
  Application.Run;
end.
