unit WindowsMetaFileForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Dialogs, ExtCtrls, Forms, Graphics, Controls, StdCtrls, SysUtils,
  CoreFormClasses, ComCtrls;

type
  // Settings
  SETTINGS_OFFSETS = record
	// Size offsets (to assist with form resize events)
	nGroupWidth, nGroupHeight: Integer;
	nFilePathWidth, nBrowseLeft, nDiagnosticsLeft, nBevelWidth: Integer;
	nOutputMsgWidth, nOutputMsgTop, nExitButtonLeft, nExitButtonTop: Integer;
  end;

  SETTINGS_IMAGE_READER = record
	// Application name and path
	szAppName, szAppFolder: String;

	// Size offsets (to assist with form resize events)
	offsets: SETTINGS_OFFSETS;

	// Current image and image type
	szImage: String;

	// True size of the underlying picture (required because of a D7 bug where .ico always report
	// width / height as 32, regardless of the actual size of the icon)
	nTruePictureWidth, nTruePictureHeight: Integer;
  end;

  // Main form
  TfrmWindowsMetaFile = class(TForm)
	gbImageFile: TGroupBox;
	lblImageFile: TLabel;
	ebImageFile: TEdit;
	btnBrowse: TButton;
	tbShowBorder: TCheckBox;
	tbFullScreen: TCheckBox;
	btnSaveImage: TButton;
	lblGifFrame: TLabel;
	imgGifPause: TImage;
	imgGifPlay: TImage;
	lblDiagnostics: TLabel;
	bevelSeparator: TBevel;

	lblOutputMessage: TStaticText;
	btnExit: TButton;

	UpdateTimer: TTimer;

	procedure FormCreate(Sender: TObject);
	procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
	procedure FormResize(Sender: TObject);

	procedure btnExitClick(Sender: TObject);
	procedure gbImageFileClick(Sender: TObject);
	procedure btnBrowseClick(Sender: TObject);
	procedure tbShowBorderClick(Sender: TObject);
	procedure tbFullScreenClick(Sender: TObject);
	procedure btnSaveImageClick(Sender: TObject);
	procedure imgGifTogglePlayClick(Sender: TObject);

	procedure OnUpdateTimer(Sender: TObject);

  private
	{ Private declarations }
	m_bExiting, m_bSettingUp: Boolean;
	m_settings: SETTINGS_IMAGE_READER;

	m_bCreatedImage: Boolean;
	m_imgLoaded: TRectangleImage;
	m_dwHideOutputMessageLabel: DWORD;

	// GIF images
	m_nLastGifFrame, m_nPausedGifFrame: Integer;

	{$IFDEF DBG}
	// Diagnostics
	m_dwResizeEvents: DWORD;
	{$ENDIF}

	procedure DoFormResize();
	procedure CreateImage(const cbNoImage: Boolean);
	procedure DoResizeImage();
	procedure SetGifImageControls(const cbShowGifControls: Boolean);
	procedure OnGifPaint(Sender: TObject);

  public
	{ Public declarations }
  end;

var
  frmWindowsMetaFile: TfrmWindowsMetaFile;

implementation

uses
  ExtDlgs, jpeg,
  CoreTypes, FormUtils, GifImage, SystemUtils;

const
  FORM_WIDTH_INITIAL: Integer		= 1200;		// Intended for a minimum dscreen size of 1280x1024
  FORM_HEIGHT_INITIAL: Integer		= 880;

  FORM_WIDTH_MIN: Integer			= 480;
  FORM_WIDTH_MAX: Integer			= 1600;
  FORM_HEIGHT_MIN: Integer			= 240;
  FORM_HEIGHT_MAX: Integer			= 1200;

  IMAGE_LEFT_MIN: Integer			= 5;
  IMAGE_TOP_MIN: Integer			= 102;
  IMAGE_WIDTH_DEFAULT: Integer		= 130;
  IMAGE_HEIGHT_DEFAULT: Integer		= 60;

  OUTPUT_MSG_VISIBLE_TICKS: DWORD	= 7500;

{$R *.dfm}

procedure TfrmWindowsMetaFile.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_bExiting := False;

	// Settings
	ZeroMemory(@m_settings, SizeOf(SETTINGS_IMAGE_READER));

	// Application details
	m_settings.szAppName := Application.ExeName;
	m_settings.szAppFolder := ExtractFilePath(Application.ExeName);

	// Control offsets. These are set at design-time, and saving these allows  Reference everything to allow them to be resized)
	m_settings.offsets.nGroupWidth := (Self.ClientWidth - gbImageFile.Width);
	m_settings.offsets.nGroupHeight := (Self.ClientHeight - gbImageFile.Height);

	m_settings.offsets.nFilePathWidth := (Self.ClientWidth - ebImageFile.Width);
	m_settings.offsets.nBrowseLeft := (Self.ClientWidth - btnBrowse.Left);
	m_settings.offsets.nDiagnosticsLeft := (Self.ClientWidth - lblDiagnostics.Left);
	m_settings.offsets.nBevelWidth := (Self.ClientWidth - bevelSeparator.Width);

	m_settings.offsets.nOutputMsgWidth := (Self.ClientWidth - lblOutputMessage.Width);
	m_settings.offsets.nOutputMsgTop := (Self.ClientHeight - lblOutputMessage.Top);
	m_settings.offsets.nExitButtonLeft := (Self.ClientWidth - btnExit.Left);
	m_settings.offsets.nExitButtonTop := (Self.ClientHeight - btnExit.Top);

	m_settings.szImage := '';

	m_settings.nTruePictureWidth := 0;
	m_settings.nTruePictureHeight := 0;

	// Main class members
	m_bCreatedImage := False;
	m_dwHideOutputMessageLabel := 0;
	m_nLastGifFrame := 0;
	m_nPausedGifFrame := -1;
end;

procedure TfrmWindowsMetaFile.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	// Called when the user clicks the "X" on the title bar. We could pop up a "Are you sure you
	// want to close?" or similar (and CanClose to False) if required.
	m_bExiting := True;
end;

procedure TfrmWindowsMetaFile.FormDestroy(Sender: TObject);
begin
	// Close form
	m_bExiting := True;
	if (m_bCreatedImage) then
		begin
		m_imgLoaded.Picture := nil;
		m_imgLoaded.Free();
		end;
end;

procedure TfrmWindowsMetaFile.FormShow(Sender: TObject);
begin
	// Show form
	{$IFDEF DBG}
	m_dwResizeEvents := 0;
	lblDiagnostics.Visible := True;
	{$ENDIF}

	// Resize the form to an initial size. We assume a minimum screen size of 1280x1024 though
	// strictly we could check the actual installed screen size.
	m_bSettingUp := True;
	Self.ClientWidth := FORM_WIDTH_INITIAL;
	Self.ClientHeight := FORM_HEIGHT_INITIAL;
	DoFormResize();
	m_bSettingUp := False;

	// Set up GIF image controls
	imgGifPlay.Left := imgGifPause.Left;

	// Prevent the TImage control from generating OnDblClick events
	imgGifPause.ControlStyle := (ControlStyle - [csDoubleClicks]);
	imgGifPlay.ControlStyle := (ControlStyle - [csDoubleClicks]);

	// Start an update timer
	UpdateTimer.Enabled := True;
end;

procedure TfrmWindowsMetaFile.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
	var Resize: Boolean);
begin
	// Don't allow the form to become too big or too small. If the user tries to
	if (	(NewWidth < FORM_WIDTH_MIN) or
			(NewWidth > FORM_WIDTH_MAX) or
			(NewHeight < FORM_HEIGHT_MIN) or
			(NewHeight > FORM_HEIGHT_MAX)) then
		begin
		// There are two approaches here (the latter is kinder to the user):
		// * If size is out-of-founds, then refuse to resize with "Resize := False;"
		// * Is size is out-of-bounds, set the set to the appropriate limit
		if (NewWidth < FORM_WIDTH_MIN) then
			NewWidth := FORM_WIDTH_MIN;
		if (NewWidth > FORM_WIDTH_MAX) then
			NewWidth := FORM_WIDTH_MAX;

		if (NewHeight < FORM_HEIGHT_MIN) then
			NewHeight := FORM_HEIGHT_MIN;
		if (NewHeight > FORM_HEIGHT_MAX) then
			NewHeight := FORM_HEIGHT_MAX;
		end;
end;

procedure TfrmWindowsMetaFile.FormResize(Sender: TObject);
begin
	if (m_bSettingUp) then
		Exit;

	// When the form is resized, move controls to keep the form neat
	DoFormResize();
end;

procedure TfrmWindowsMetaFile.btnExitClick(Sender: TObject);
begin
	if (m_bExiting) then
		Exit;

	m_bExiting := True;
	Close();
end;

procedure TfrmWindowsMetaFile.gbImageFileClick(Sender: TObject);
var
	nControlCloseClick: Integer;
	control: TControl;
begin
	// Find a nearby control...and click it!
	nControlCloseClick := GetCloseControlClick(gbImageFile,
		(CONTROL_TCHECKBOX + CONTROL_TBUTTON + CONTROL_TIMAGE));
	if (nControlCloseClick > -1) then
		begin
		// Close to a control
		control := gbImageFile.Controls[nControlCloseClick];
		if (IsControlType(control, CONTROL_TCHECKBOX)) then
			TCheckBox(control).OnClick(control)
		else if (IsControlType(control, CONTROL_TBUTTON)) then
			TButton(control).OnClick(control)
		else if (IsControlType(control, CONTROL_TIMAGE)) then
			TImage(control).OnClick(control);
		end;
end;

procedure TfrmWindowsMetaFile.btnBrowseClick(Sender: TObject);
var
	openPictureDlg: TOpenPictureDialog;
	bImageWasVisible: Boolean;
	strImageType: String;
begin
	if (m_bExiting) then
		Exit;

	// We may hide the image...
	bImageWasVisible := False;
	if (m_bCreatedImage) then
		bImageWasVisible := m_imgLoaded.Visible;

	// Browse for the image file
	openPictureDlg := TOpenPictureDialog.Create(Self);
	if (openPictureDlg.Execute) then
		begin
		// Check if file exists
		if (FileExists(openPictureDlg.FileName)) then
			begin
			// File exists, load the data into the image component
			if (m_bCreatedImage) then
				m_imgLoaded.Visible := False
			else
				begin
				CreateImage(False);
				bImageWasVisible := True;
				end;

			// Test method to detect the type of an image by reading its' header information
			strImageType := DetectImageType(openPictureDlg.FileName);

			// User has loaded an image...
			m_settings.szImage := openPictureDlg.FileName;

			// Reset variables related to GIF images
			m_nPausedGifFrame := -1;

			// Load the selected file
			AssignPictureFromFile(m_imgLoaded.Picture, openPictureDlg.FileName);
			if (m_imgLoaded.Picture.Graphic is TGifImage) then
				begin
				// Show the frame number for animated GIFs. The number of frames is:
				//		TGifImage(m_imgLoaded.Picture.Graphic).Images.Count
				lblGifFrame.Visible := True;
				SetGifImageControls(True);

				TGifImage(m_imgLoaded.Picture.Graphic).AnimationSpeed := 200;
				TGifImage(m_imgLoaded.Picture.Graphic).OnPaint := OnGifPaint;
				end
			else
				begin
				// Not a GIF image...
				lblGifFrame.Visible := False;
				SetGifImageControls(False);
				end;

			// Resize the TImage container
			if (IsFileExtType(openPictureDlg.FileName, '.ico')) then
				begin
				// Extract the size of icons separately (due to a bug in Delphi 7)
				GetTrueIconSize(openPictureDlg.FileName,
					m_settings.nTruePictureWidth,
					m_settings.nTruePictureHeight);
				end
			else
				begin
				// Save the size of the underlying raw data
				m_settings.nTruePictureWidth := m_imgLoaded.Picture.Width;
				m_settings.nTruePictureHeight := m_imgLoaded.Picture.Height;
				end;

			// Resize the TImage container
			DoResizeImage();

			// Show the full path to the image loaded
			ebImageFile.Text := openPictureDlg.FileName;
			end
		else
			begin
			// File does not exist
			MessageDlg('File does not exist', mtWarning, [mbOK], 0);
			end;
		end;

	// Clean up and show the image
	openPictureDlg.Free();
	if (m_bCreatedImage) and (bImageWasVisible) then
		m_imgLoaded.Visible := True;
end;

procedure TfrmWindowsMetaFile.tbShowBorderClick(Sender: TObject);
begin
	// Show (or hide) a border around the image
	if (not m_bCreatedImage) then
		CreateImage(True);

	m_imgLoaded.ShowNotSelected := tbShowBorder.Checked;
end;

procedure TfrmWindowsMetaFile.tbFullScreenClick(Sender: TObject);
begin
	if (not m_bCreatedImage) and (not tbShowBorder.Checked) then
		Exit;

	// Show the image (original size) or use the available space
	if (not m_bCreatedImage) then
		begin
		// No image has been loaded, so display a blank frame
		CreateImage(True);
		end
	else
		begin
		// Resize the image: either the actual image size, or use the full available space
		DoResizeImage();
		end;
end;

procedure TfrmWindowsMetaFile.btnSaveImageClick(Sender: TObject);
var
	strFilename, strFullPath: String;
begin
	if (m_bExiting) then
		Exit;

	if (m_bCreatedImage) and (Length(m_settings.szImage) > 0) then
		begin
		// Generate the output filename (a date/time-stamped bitmap)
		strFilename := ChangeFileExt(Format('%s_%s', [
			GetIsoDateTimeString(Now(), True),
			ExtractFileName(m_settings.szImage)]), '.bmp');
		strFullPath := Format('%s%s', [m_settings.szAppFolder, strFilename]);

		// Save the picture to disk
		// Note: Calling "TImage.Picture.SaveToFile(file);"( without conversion) gives the same
		// image / mime type (ie. a loaded .ico file is saved as an icon).
		SavePictureAsBMP(m_imgLoaded.Picture, strFullPath);
		lblOutputMessage.Caption := Format('%s saved to application folder', [strFilename]);
		lblOutputMessage.Visible := True;
		m_dwHideOutputMessageLabel := (GetTickCount() + OUTPUT_MSG_VISIBLE_TICKS);
		end
	else
		MessageDlg('Please load an image first...', mtWarning, [mbOK], 0);
end;

procedure TfrmWindowsMetaFile.imgGifTogglePlayClick(Sender: TObject);
begin
	// Pause or unpaused the GIF frame
	if (m_nPausedGifFrame <> - 1) then
		m_nPausedGifFrame := -1					// Playing image again
	else
		m_nPausedGifFrame := m_nLastGifFrame;	// Pause at the current frame

	SetGifImageControls(True);
end;

procedure TfrmWindowsMetaFile.OnUpdateTimer(Sender: TObject);
begin
	// Disable timer
	UpdateTimer.Enabled := False;
	if (m_bExiting) then
		Exit;

	// Time to hide the "file saved" label?
	if (m_dwHideOutputMessageLabel > 0) then
		begin
		if (GetTickCount() > m_dwHideOutputMessageLabel) then
			begin
			m_dwHideOutputMessageLabel := 0;
			lblOutputMessage.Visible := False;
			end;
		end;

	// Restart timer
	UpdateTimer.Enabled := True;
end;

// Private methods
procedure TfrmWindowsMetaFile.DoFormResize();
begin
	// Method used to resize the form and neaten the layout
	gbImageFile.Visible := False;

	gbImageFile.Width := (Self.ClientWidth - m_settings.offsets.nGroupWidth);
	gbImageFile.Height := (Self.ClientHeight - m_settings.offsets.nGroupHeight);

	ebImageFile.Width := (Self.ClientWidth - m_settings.offsets.nFilePathWidth);
	btnBrowse.Left := (Self.ClientWidth - m_settings.offsets.nBrowseLeft);

	{$IFDEF DBG}
	Inc(m_dwResizeEvents);
	lblDiagnostics.Caption := Format('Resize events: %d', [m_dwResizeEvents]);
	lblDiagnostics.Left := (gbImageFile.Width - m_settings.offsets.nDiagnosticsLeft);
	{$ENDIF}

	bevelSeparator.Width := (Self.ClientWidth - m_settings.offsets.nBevelWidth);

	lblOutputMessage.Width := (Self.ClientWidth - m_settings.offsets.nOutputMsgWidth);
	lblOutputMessage.Top := (Self.ClientHeight - m_settings.offsets.nOutputMsgTop);
	btnExit.Left := (Self.ClientWidth - m_settings.offsets.nExitButtonLeft);
	btnExit.Top := (Self.ClientHeight - m_settings.offsets.nExitButtonTop);

	gbImageFile.Visible := True;
end;

procedure TfrmWindowsMetaFile.CreateImage(const cbNoImage: Boolean);
begin
	if (m_bCreatedImage) then
		Exit;

	// Create the image for the first time
	m_imgLoaded := TRectangleImage.Create(Self);
	m_imgLoaded.Visible := False;
	with m_imgLoaded do
		begin
		// Main properties
		Name := 'imgLoaded';
		Parent := gbImageFile;
		Picture := nil;

		// Image position and size
		Left := IMAGE_LEFT_MIN;
		Top := IMAGE_TOP_MIN;
		if (tbFullScreen.Checked) then
			begin
			// Create using the full available space
			Width := (gbImageFile.Width - (IMAGE_LEFT_MIN * 2));
			Height := (gbImageFile.Height - IMAGE_TOP_MIN - 5);
			end
		else
			begin
			// Create with a default size
			Width := IMAGE_WIDTH_DEFAULT;
			Height := IMAGE_HEIGHT_DEFAULT;
			end;

		// Border colour (we only use the "not selected" feature)
		m_imgLoaded.SetNotSelectedColour := clBlack;
		m_imgLoaded.ShowNotSelected := tbShowBorder.Checked;
		end;

	m_imgLoaded.Visible := True;

	// Image is created!
	m_bCreatedImage := True;
end;

procedure TfrmWindowsMetaFile.DoResizeImage();
var
	bImageVisible: Boolean;
	nHorizontalSpace, nVerticalSpace: Integer;
	fImageRatio, fSpaceRatio: Single;
begin
	// Resize the image (either to the actual size of the picture or the full available space)
	bImageVisible := m_imgLoaded.Visible;
	m_imgLoaded.Visible := False;
	nHorizontalSpace := (gbImageFile.Width - (IMAGE_LEFT_MIN * 2));
	nVerticalSpace := (gbImageFile.Height - IMAGE_TOP_MIN - 5);

	fImageRatio := (m_imgLoaded.Picture.Width / m_imgLoaded.Picture.Height);
	fSpaceRatio :=  (nHorizontalSpace / nVerticalSpace);
	if (tbFullScreen.Checked) then
		begin
		// Use the entire available space
		if (fImageRatio < fSpaceRatio) then
			begin
			// Image is wider than the available space
			m_imgLoaded.Width := nHorizontalSpace;
			m_imgLoaded.Height := (100);
			end
		else
			begin
			// Image is taller than the available space
			end;
		end
	else
		begin
		// Show the picture in the original size (if possible)
		m_imgLoaded.Width := m_settings.nTruePictureWidth;
		m_imgLoaded.Height := m_settings.nTruePictureHeight;
		end;

	m_imgLoaded.Visible := bImageVisible;
end;

procedure TfrmWindowsMetaFile.SetGifImageControls(const cbShowGifControls: Boolean);
begin
	// Configure GIF image controls
	if (cbShowGifControls) then
		begin
		imgGifPlay.Visible := (m_nPausedGifFrame <> -1);
		imgGifPause.Visible := (not imgGifPlay.Visible);
		//ADAD trackerGifFrame
		end
	else
		begin
		imgGifPause.Visible := False;
		imgGifPlay.Visible := False;
		end;
end;

procedure TfrmWindowsMetaFile.OnGifPaint(Sender: TObject);
begin
	// When painting animated GIF images, show the frame number as a diagnostics label
	// Note: To show the same frame all the time:
	//		(Sender as TGIFPainter).ActiveImage := 11;

	// Paused?
	if (m_nPausedGifFrame <> -1) then
		(Sender as TGIFPainter).ActiveImage := m_nPausedGifFrame;

	// Show the current frame number
	m_nLastGifFrame := (Sender as TGIFPainter).ActiveImage;
	lblGifFrame.Caption := Format('Frame: %d', [m_nLastGifFrame]);
	lblGifFrame.Refresh();
end;

end.

