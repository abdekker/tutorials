{ Base class(es) for TForm classes }
unit CoreFormClasses;
{$I CoreOptions.inc}

interface

uses
  Windows, Classes, Forms;

type
  TGeneralBaseForm = class(TForm)
  private
	{ Private declarations }

  protected
	{ Protected declarations }

  public
	{ Public declarations }
	//constructor Create(AOwner: TComponent); override; virtual;
	constructor Create(AOwner: TComponent); override;
  end;
  TGeneralBase = class of TGeneralBaseForm;

implementation

constructor TGeneralBaseForm.Create(AOwner: TComponent);
begin
	// Call base constructor
	inherited;
end;

end.