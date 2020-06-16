unit JPGTools;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

uses Windows, CoreTypes, Imaging, SysUtils;

  // Functions from JPGTools.dll
  function EncodeJpgFileFromDIB(lpszPathName: PChar; nWidth, nHeight: Integer;
	pbyBits: PBYTE; nQuality: Integer) : Integer; stdcall;
  function EncodeJpgFileFromDIBWithComment(lpszPathName: PChar; nWidth, nHeight: Integer;
	pbyBits: PBYTE; nQuality: Integer; pszComment: PChar) : Integer; stdcall;
  function DecodeJpgFileToDIB(lpszPathName: PChar; pnWidth, pnHeight: PInteger;
	pbyBits: PBYTE; pszComment: PChar; nMaxComment: Integer) : Integer; stdcall;
  function HackJpgComments(lpszPathName, lpszComment: PChar) : LongBool; stdcall;

  // Functions coded here (in Delphi, but using functions from JPGTools.dll)
  function LoadJPG(pDest: PTIMAGECELL; strFile: String;
	pszComments: PChar; nMaxComment: Integer) : Boolean;
  function LoadJpgNoComments24(pDest: PTIMAGECELL; strFile: String) : Boolean;
  function SaveJPG(pImage: PTIMAGECELL; strFile: String; nQuality: Integer) : Boolean;
  function SaveJpgWithComments(pImage: PTIMAGECELL; strFile: String;
	nQuality: Integer; pszComments: Pchar) : Boolean;

implementation

  function EncodeJpgFileFromDIB; external 'JPGTools.dll' name 'EncodeJpgFileFromDIB';
  function EncodeJpgFileFromDIBWithComment; external 'JPGTools.dll' name 'EncodeJpgFileFromDIBWithComment';
  function DecodeJpgFileToDIB; external 'JPGTools.dll' name 'DecodeJpgFileToDIB';
  function HackJpgComments; external 'JPGTools.dll' name 'HackJpgComments';

function LoadJPG(pDest: PTIMAGECELL; strFile: String; pszComments: PChar;
	nMaxComment: Integer) : Boolean;
var
	nWidth, nHeight: Integer;
begin
	// Load jpeg image
	Result := False;
	if (not FileExists(strFile)) then
		Exit;

	// Encode the JPG information in the "pBitmapBits" part of the image. The first call to
	// "DecodeJpgFileToDIB" is required to get the width / height of the image as well as to read
	// any comments in the header of the JPEG.
	if (DecodeJpgFileToDIB(PChar(strFile), @nWidth, @nHeight, nil, pszComments, nMaxComment) <> 0) then
		begin
		if (pDest.nWidth <> nWidth) or (pDest.nHeight <> nHeight) then
			begin
			// Image is not the right size
			PrepareImageCell(pDest, nWidth, nHeight);
			Expand24(pDest);
			end
		else
			begin
			// Image is aleady the right size
			if (not pDest.b24Valid) then
				Expand24(pDest);
			end;

		// Load the actual JPG data off disk
		if (DecodeJpgFileToDIB(PChar(strFile), @nWidth, @nHeight, pDest.pBitmapBits, nil, 0) <> 0) then
			begin
			// Success!
			Result := True;

			// If you require the 8-bit image data, uncomment this line to convert the 24-bit part
			//Reduce8(pDest);
			end;
		end;
end;

function LoadJpgNoComments24(pDest: PTIMAGECELL; strFile: String) : Boolean;
var
	nWidth, nHeight: Integer;
begin
	// Alternative function where an 8-bit B&W image is not required. Embedded comments, if any,
	// are ignored. See "LoadJPG" for additional details.

	// Note to developer: The LomaX4 has a feature called the "Inspection Log" where inspection
	// settings and results can be embedded directly into the reject JPG image. In addition, the
	// LomaX4 has a (rarely used) feature to export product settings to a JPG, with product
	// settings embedded directly into the JPG header. The user could then "import" this product
	// into the software.

	// Both of the above features are not available (by design) in the XrayMini. Provided 8-bit
	// image data is not required (which will generally be the case, except for the 3D view) or a
	// feature is implemented using embedded text information, the XrayMini project can use this
	// method instead of "LoadJPG". This method is ~3 times quicker.
	Result := False;
	if (not FileExists(strFile)) then
		Exit;

	if (DecodeJpgFileToDIB(PChar(strFile), @nWidth, @nHeight, nil, nil, 0) <> 0) then
		begin
		// Ensure dimensions of image are correct
		if (pDest.nWidth <> nWidth) or (pDest.nHeight <> nHeight) then
			PrepareImageCell(pDest, nWidth, nHeight);

		// Create 24-bit image
		if (not pDest.b24Valid) then
			Expand24(pDest);

		// Load JPG with the correct dimensions
		if (DecodeJpgFileToDIB(PChar(strFile), @nWidth, @nHeight, pDest.pBitmapBits, nil, 0) <> 0) then
			Result := True;
		end;
end;

function SaveJPG(pImage: PTIMAGECELL; strFile: String; nQuality: Integer) : Boolean;
begin
	// Save an image cell to a JPG on disk (no comments). Use intead of SaveBMP(...) when disk
	// access is a bottleneck (which it generally will be compared with in-memory manipulations).

	// An example use:
	//		SaveJPG(pImage, 'C:\Tmp\Image.jpg', 90);
	if (not pImage.b24Valid) then
		Expand24(pImage);

	Result := (EncodeJpgFileFromDIB(
		PChar(strFile), pImage.nWidth, pImage.nHeight, pImage.pBitmapBits, nQuality) <> 0);
end;

function SaveJpgWithComments(pImage: PTIMAGECELL; strFile: String;
	nQuality: Integer; pszComments: PChar) : Boolean;
begin
	// Save an image cell to a JPG on disk (with comments)
	if (not pImage.b24Valid) then
		Expand24(pImage);

	// The maximum length of "pszComments" is 5552 characters. If you need to write something
	// longer than this, first write a short non-zero comment using this function and then use
	// "HackJpgComments".

	// The reason for the 5552 limit is not known
	Result := (EncodeJpgFileFromDIBWithComment(PChar(strFile), pImage.nWidth,
		pImage.nHeight, pImage.pBitmapBits, nQuality, pszComments) <> 0);
end;

end.
