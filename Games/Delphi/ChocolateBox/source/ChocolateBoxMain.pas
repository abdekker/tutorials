unit ChocolateBoxMain;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Controls, ExtCtrls, Forms, Graphics, StdCtrls, SysUtils,
  CoreFormClasses, CoreTypes, FormUtils, GameMachine, SystemUtils;

type
  // Main form
  TfrmChocolateBox = class(TGeneralBaseForm)

	UpdateTimer: TTimer;
	imgBackground: TImage;

	imgSettings: TImage;
	lblSettings: TStaticText;
	imgExit: TImage;
	lblExit: TStaticText;

	procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure imgSettingsClick(Sender: TObject);
	procedure imgExitClick(Sender: TObject);

	procedure OnUpdateTimer(Sender: TObject);

  private
	{ Private declarations }
	// Initialisation and system-state flags
	m_bFirstTimeShow, m_bExiting, m_bAllowDraw: Boolean;

	// Initialisation
	procedure FirstTimeInit();

  public
	{ Public declarations }
  end;

var
  frmChocolateBox: TfrmChocolateBox;

implementation

uses
  Math, GameSettings;

const
  DUMMY_CONST = 0;

type
  // Available actions for each category
  TDummy = (
	eDummy1
  );

{$R *.dfm}

procedure TfrmChocolateBox.FormCreate(Sender: TObject);
begin
	// Initialise main form
	m_bFirstTimeShow := True;
	m_bExiting := False;
	m_bAllowDraw := True;

	// Seed the random number generator (7919 is the 1000th prime number)
	RandSeed := Integer(GetTickCount() mod 7919);

	// Initialise system and form utilities
	InitialiseSystemUtils();
	InitialiseFormUtils();
end;

procedure TfrmChocolateBox.FormDestroy(Sender: TObject);
begin
	// Clean up

	// Close system and form utilities
	CloseSystemUtils();
	CloseFormUtils();

	// Close machine object (which will save settings)
	ChocolateBox.PrepareToExit();
	ChocolateBox.Free();
end;

procedure TfrmChocolateBox.FormShow(Sender: TObject);
begin
	// Show form
	if (m_bFirstTimeShow) then
		begin
		// Initialise the system
		FirstTimeInit();
		m_bFirstTimeShow := False;

		// Set the form size
		Self.Width := ChocolateBox.GameCache.nScreenWidth;
		Self.Height := ChocolateBox.GameCache.nScreenHeight;

		// Set the top position to be close to the top of the screen
		Self.Top := 5;
		Self.Left := ((Screen.Width - Self.ClientWidth) div 2);

		// Load the starting background image
		imgBackground.Width := Self.Width;
		imgBackground.Height := Self.Height;
		imgBackground.Picture.LoadFromFile(ChocolateBox.GameCache.szAppPath + 'ChocolateBox-2.jpg');

		// Start the update timer
		UpdateTimer.Enabled := True;
	end;
end;

procedure TfrmChocolateBox.imgExitClick(Sender: TObject);
begin
	// Close form
	m_bExiting := True;
	ChocolateBox.PrepareToExit();
	Close();
end;

procedure TfrmChocolateBox.imgSettingsClick(Sender: TObject);
begin
	// Display game settings
	m_bAllowDraw := False;
	frmGameSettings := TfrmGameSettings.Create(Self);
	frmGameSettings.settings := ChocolateBox.GameSettings;
	frmGameSettings.SetGameRunning(False);
	if (frmGameSettings.ShowModal() = mrOk) then
		begin
		// Settings have been changed, copy them back
		ChocolateBox.GameSettings := frmGameSettings.settings;
		end;

	// Clean up
	frmGameSettings.Free();
	frmGameSettings := nil;
	m_bAllowDraw := False;
end;

procedure TfrmChocolateBox.OnUpdateTimer(Sender: TObject);
begin
	// An update timer event...
	if (m_bExiting) then
		Exit;

	// Disable timer while we perform the task for that timer
	UpdateTimer.Enabled := False;

	// Restart the timer
	UpdateTimer.Enabled := True;
end;

// Private functions: Start
procedure TfrmChocolateBox.FirstTimeInit();
begin
	// Load main game settings
	ChocolateBox := TGameMachine.Create();
	ChocolateBox.InitSystem();
end;
// Private functions: Start

end.
