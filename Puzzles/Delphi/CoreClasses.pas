{ Base class for all puzzle forms }
unit CoreClasses;
{$I ..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Classes, Forms;

type
  TPuzzleBaseForm = class(TForm)
  private
	{ Private declarations }

  protected
	{ Protected declarations }

  public
  	{ Public declarations }
	constructor Create(AOwner: TComponent); virtual;
  end;
  TPuzzleBase = class of TPuzzleBaseForm;

implementation

constructor TPuzzleBaseForm.Create(AOwner: TComponent);
begin
	// Call base constructor
	inherited;
end;

end.