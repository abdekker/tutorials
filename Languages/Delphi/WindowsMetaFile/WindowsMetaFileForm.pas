unit WindowsMetaFileForm;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses
  Windows, Messages, Classes, Dialogs, ExtCtrls, Forms, Graphics, Controls, StdCtrls, SysUtils,
  CoreFormClasses;

const
  FORM_WIDTH_INITIAL: Integer		= 1200;		// Intended for a minimum dscreen size of 1280x1024
  FORM_HEIGHT_INITIAL: Integer		= 880;

  FORM_WIDTH_MIN: Integer			= 260;
  FORM_WIDTH_MAX: Integer			= 1600;
  FORM_HEIGHT_MIN: Integer			= 120;
  FORM_HEIGHT_MAX: Integer			= 1200;

  IMAGE_LEFT_MIN: Integer			= 5;
  IMAGE_TOP_MIN: Integer			= 80;
  IMAGE_WIDTH_DEFAULT: Integer		= 130;
  IMAGE_HEIGHT_DEFAULT: Integer		= 60;

type
  // Settings
  SETTINGS_OFFSETS = record
	// Size offsets (to assist with form resize events)
	nFilePathWidth, nBrowseLeft, nDiagnosticsLeft: Integer;
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

	lblDiagResizeEvents: TLabel;
    btnSaveImage: TButton;

	procedure FormCreate(Sender: TObject);
	procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
	procedure FormDestroy(Sender: TObject);
	procedure FormShow(Sender: TObject);
	procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
	procedure FormResize(Sender: TObject);

	procedure btnBrowseClick(Sender: TObject);
	procedure tbShowBorderClick(Sender: TObject);
	procedure tbFullScreenClick(Sender: TObject);
	procedure btnSaveImageClick(Sender: TObject);

  private
	{ Private declarations }
	m_bExiting: Boolean;
	m_settings: SETTINGS_IMAGE_READER;

	m_bCreatedImage: Boolean;
	m_imgLoaded: TRectangleImage;

	{$IFDEF DBG}
	// Diagnostics
	m_dwResizeEvents: DWORD;
	{$ENDIF}

	procedure CreateImage(const cbNoImage: Boolean);
	procedure ResizeImage();

  public
	{ Public declarations }
  end;

var
  frmWindowsMetaFile: TfrmWindowsMetaFile;

implementation

uses
  ExtDlgs, jpeg,
  CoreTypes, FormUtils, SystemUtils;

{$R *.dfm}

procedure TfrmWindowsMetaFile.FormCreate(Sender: TObject);
begin
	// Initialise form
	m_bExiting := False;

	ZeroMemory(@m_settings, SizeOf(SETTINGS_IMAGE_READER));
	m_settings.szAppName := Application.ExeName;
	m_settings.szAppFolder := ExtractFilePath(Application.ExeName);

	m_settings.offsets.nFilePathWidth := (gbImageFile.Width - ebImageFile.Width);
	m_settings.offsets.nBrowseLeft := (gbImageFile.Width - btnBrowse.Left);
	m_settings.offsets.nDiagnosticsLeft := (gbImageFile.Width - lblDiagResizeEvents.Left);

	m_settings.szImage := '';

	m_settings.nTruePictureWidth := 0;
	m_settings.nTruePictureHeight := 0;

	m_bCreatedImage := False;
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
var
	nWidthChange, nHeightChange: Integer;
begin
	// Show form
	{$IFDEF DBG}
	m_dwResizeEvents := 0;
	lblDiagResizeEvents.Visible := True;
	{$ENDIF}

	// Resize the form to its proper initial size. We assume a minimum screen size of 1280x1024
	// though strictly we could check the actual installed screen size.
	nWidthChange := (FORM_WIDTH_INITIAL - Self.ClientWidth);
	nHeightChange := (FORM_HEIGHT_INITIAL - Self.ClientHeight);

	Self.ClientWidth := FORM_WIDTH_INITIAL;
	Self.ClientHeight := FORM_HEIGHT_INITIAL;

	gbImageFile.Width := (gbImageFile.Width + nWidthChange);
	btnBrowse.Left := (btnBrowse.Left + nWidthChange);
	ebImageFile.Width := (ebImageFile.Width + nWidthChange);

	gbImageFile.Height := (gbImageFile.Height + nHeightChange);
end;

procedure TfrmWindowsMetaFile.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
	var Resize: Boolean);
begin
	// Don't allow the form to become too big or too small
	if (	(NewWidth < FORM_WIDTH_MIN) or
			(NewWidth > FORM_WIDTH_MAX) or
			(NewHeight < FORM_HEIGHT_MIN) or
			(NewHeight > FORM_HEIGHT_MAX)) then
		Resize := False;
end;

procedure TfrmWindowsMetaFile.FormResize(Sender: TObject);
begin
	// When the form is resized, move controls to keep the form neat
	gbImageFile.Visible := False;
	gbImageFile.Width := (frmWindowsMetaFile.ClientWidth - (gbImageFile.Left * 2));
	gbImageFile.Height := (frmWindowsMetaFile.ClientHeight - (gbImageFile.Top * 2));

	ebImageFile.Width := (gbImageFile.Width - m_settings.offsets.nFilePathWidth);
	btnBrowse.Left := (gbImageFile.Width - m_settings.offsets.nBrowseLeft);

	{$IFDEF DBG}
	Inc(m_dwResizeEvents);
	lblDiagResizeEvents.Caption := Format('Resize events: %d', [m_dwResizeEvents]);
	lblDiagResizeEvents.Left := (gbImageFile.Width - m_settings.offsets.nDiagnosticsLeft);
	{$ENDIF}

	gbImageFile.Visible := True;
end;

procedure TfrmWindowsMetaFile.btnBrowseClick(Sender: TObject);
var
	openPictureDlg: TOpenPictureDialog;
	strTest: String;
begin
	if (m_bExiting) then
		Exit;

	// Browse for the image file
	if (m_bCreatedImage) then
		m_imgLoaded.Visible := False;

	openPictureDlg := TOpenPictureDialog.Create(Self);
	if (openPictureDlg.Execute) then
		begin
		// Check if file exists
		if (FileExists(openPictureDlg.FileName)) then
			begin
			// File exists, load the data into the image component
			if (not m_bCreatedImage) then
				CreateImage(False);

			strTest := DetectImageType(openPictureDlg.FileName);

			// User has loaded an image...
			m_settings.szImage := openPictureDlg.FileName;

			// Load the selected file
			AssignPictureFromFile(m_imgLoaded.Picture, openPictureDlg.FileName);

			// Resize the TImage container
			if (IsFileExtType(openPictureDlg.FileName, '.ico')) then
				begin
				// Extract the size of icons separately due to a bug in Delphi 7
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
			ResizeImage();

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
	if (m_bCreatedImage) then
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
		ResizeImage();
		end;
end;

procedure TfrmWindowsMetaFile.btnSaveImageClick(Sender: TObject);
var
	strFilename, strFullPath: String;
	astrTest: array[1..10] of String;
begin
	if (m_bExiting) then
		Exit;

	if (m_bCreatedImage) and (Length(m_settings.szImage) > 0) then
		begin
		// Generate the output filename
		strFilename := Format('%s_%s', [
			GetIsoDateTimeString(Now(), True),
			ExtractFileName(m_settings.szImage)]);
		strFullPath := Format('%s%s', [m_settings.szAppFolder, strFilename]);

		astrTest[1] := ChangeFileExt(m_settings.szImage,'.abc');

		// Save the picture to a date/time-stamped bitmap
		m_imgLoaded.Picture.SaveToFile(strFullPath);
		strFullPath := Format('%sb%s', [m_settings.szAppFolder, strFilename]);
		//SavePictureAsBMP(m_imgLoaded.Picture, strFullPath);
		SavePictureAsBMP(m_imgLoaded.Picture, strFullPath);
		MessageDlg(Format('%s saved to application folder', [strFilename]), mtWarning, [mbOK], 0);
		end
	else
		MessageDlg('Please load an image first...', mtWarning, [mbOK], 0);
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

procedure TfrmWindowsMetaFile.ResizeImage();
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
		//nHorizontalSpace := (gbImageFile.Width - (IMAGE_LEFT_MIN * 2));
		//nVerticalSpace := (gbImageFile.Height - IMAGE_TOP_MIN - 5);
		end
	else
		begin
		// Show the picture in the original size (if possible)
		m_imgLoaded.Width := m_settings.nTruePictureWidth;
		m_imgLoaded.Height := m_settings.nTruePictureHeight;
		end;

	m_imgLoaded.Visible := bImageVisible;
end;

end.

