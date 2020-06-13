unit Imaging;
{$I ..\..\..\Languages\Delphi\Utils\CoreOptions.inc}

interface

{ This unit is for integration of this DLL into Delphi projects }

uses
  Windows, CoreTypes;

  // ImageCell initialisation
  procedure InitialiseImageCell(); stdcall;
  procedure FreeImageCell(); stdcall;

  // ImageCell Create/Delete functions
  function CreateImageCell(nWidth, nHeight: Integer; bClear: Integer;
	byInitColour: BYTE) : PTIMAGECELL; stdcall;
  function CreateImageCellFrom(pSource: PTIMAGECELL; pRct: PRECT) : PTIMAGECELL; stdcall;
  function CreateImageCellFromBMP(szBMP: PChar; hInst: Integer) : PTIMAGECELL; stdcall;

  procedure DeleteImageCell(pImage: PTIMAGECELL); stdcall;
  procedure PrepareImageCell(pImage: PTIMAGECELL; nWidth, nHeight: Integer); stdcall;
  procedure ClearImageCell(pImage: PTIMAGECELL; byGrey: BYTE); stdcall;

  procedure ExtractImageCell(pDest, pSource: PTIMAGECELL; pRct: PRECT); stdcall;
  procedure ExtractImageCellToPosition(pDest, pSource: PTIMAGECELL; nPosX, nPosY: Integer); stdcall;
  procedure ExtractImageCell24(pDest, pSource: PTIMAGECELL; pRct: PRECT); stdcall;
  procedure ExtractImageCellToPosition24(pDest, pSource: PTIMAGECELL; nPosX, nPosY: Integer); stdcall;

  // ImageCell Conversion functions
  procedure ImageCellMinSize(ppImage: PPTIMAGECELL; nMinWidth, nMinHeight: Integer); stdcall;
  procedure ImageCellConvertSize(ppImage: PPTIMAGECELL; nNewWidth, nNewHeight: Integer); stdcall;
  procedure ImageCellCorrectAspect(ppImage: PPTIMAGECELL; byGrey: BYTE;
	nDestWidth, nDestHeight: Integer); stdcall;
  procedure ImageCellCorrectAspect24(ppImage: PPTIMAGECELL; clRGB: COLORREF;
	nDestWidth, nDestHeight: Integer); stdcall;

  procedure InvertImageCell(pImage: PTIMAGECELL); stdcall;
  procedure InvertImageCell24(pImage: PTIMAGECELL); stdcall;

  procedure LogOpImageCell(pDest, pSource: PTIMAGECELL; byLogOp: BYTE); stdcall;
  procedure LogOpMultiImage(pDest: PTIMAGECELL; pList: PTPOINTERNODE; byLogOp: BYTE); stdcall;

  procedure RotateImageCell90(ppImage: PPTIMAGECELL; bRotateRight: Boolean); stdcall;
  procedure RotateImageCellFree(pDest, pSource: PTIMAGECELL; fAngle: Single); stdcall;

  // ImageCell Characterising / Utility functions
  procedure FindImageCellBounds(pImage: PTIMAGECELL; pRct: PRECT; byThreshold: BYTE); stdcall;

  // ImageCell Drawing functions
  procedure DrawImageCellBitmap(pImage: PTIMAGECELL; hDestDC: HDC; nX, nY: Integer); stdcall;
  procedure DrawPartImageCellBitmap(pImage: PTIMAGECELL; hDestDC: HDC; pRct: PRECT;
	nX, nY: Integer; clDesktop: DWORD); stdcall;
  procedure StretchImageCellBitmap(pImage: PTIMAGECELL; hDestDC: HDC; pRct: PRECT); stdcall;
  procedure DrawTransparentBitmap(pImage: PTIMAGECELL; hDestDC: HDC;
	nStartX, nStartY: Integer; clTransparent: COLORREF); stdcall;

  procedure Expand24(pImage: PTIMAGECELL); stdcall;
  procedure Expand24Coloured(pImage: PTIMAGECELL; clOverlay: COLORREF); stdcall;
  procedure Overlay24(pDest, pSource: PTIMAGECELL; clOverlay: COLORREF); stdcall;
  procedure OverlayBlend24(pDest, pOverlay: PTIMAGECELL; clOverlay: COLORREF; fAlpha: Single); stdcall;
  procedure Blend24(pImage: PTIMAGECELL; pRct: PRECT; clRGB: COLORREF; fAlpha: Single); stdcall;
  procedure ColouriseGrey24(pImage: PTIMAGECELL; clRGB: pointer); stdcall;

  procedure DrawBorder24(pImage: PTIMAGECELL; clBorderColour: COLORREF); stdcall;

  // Utility functions for loading and saving 24-bit bitmaps
  function LoadBMP(pImage: PTIMAGECELL; hInst: Integer; szBMP: PChar) : Boolean; stdcall;
  function SaveBMP(pImage: PTIMAGECELL; szBMP: PChar; pRct: PRECT) : Boolean; stdcall;

  // Convolution filters
  procedure InitialiseConvolution(); stdcall;
  procedure FreeConvolution(); stdcall;
  procedure DilateImageCell(pDest, pSource: PTIMAGECELL); stdcall;
  procedure ErodeImageCell(pDest, pSource: PTIMAGECELL); stdcall;
  procedure GrowImageCell(pDest: PTIMAGECELL; nGrow: Integer); stdcall;
  procedure CloseImageCell(pDest, pSource: PTIMAGECELL; byIterations: BYTE); stdcall;
  procedure OpenImageCell(pDest, pSource: PTIMAGECELL; byIterations: BYTE); stdcall;
  procedure SobelFilter(pDest, pSource: PTIMAGECELL; byGain: BYTE); stdcall;
  procedure ConditionalSobel(pDest, pSource: PTIMAGECELL;
	byGain, byLower, byUpper: BYTE); stdcall;
  procedure LaplaceFilter(pDest, pSource: PTIMAGECELL; byGain: BYTE); stdcall;
  procedure ConditionalLaplace(pDest, pSource: PTIMAGECELL;
	byGain, byLower, byUpper: BYTE); stdcall;
  procedure MedianFilterHistogram(pDest, pSource: PTIMAGECELL); stdcall;
  procedure MedianFilterSorting(pDest, pSource: PTIMAGECELL); stdcall;

  // Image Processing (Gamma/Range (%))
  procedure ApplyGammaPercRange(pDest, pSource: PTIMAGECELL;
	fGamma, fLowerRange, fUpperRange: Single); stdcall;
  procedure ErodeThresholdPixels(pDest, pSource: PTIMAGECELL;
	byLower, byUpper, byErosion: BYTE); stdcall;
  procedure ErodeScreenedPixels(pDest, pScreen: PTIMAGECELL; byErosion: BYTE); stdcall;
  procedure AddImages(pResult, pAddend1, pAddend2: PTIMAGECELL); stdcall;
  procedure SubtractImages(pDiff, pMinuend, pSubtrahend: PTIMAGECELL); stdcall;

  // OpenGL functions for the 3D view
  function InitImageCellHeightmap(pState: PTOGL_STATE; pImage, pTexture: PTIMAGECELL) : Integer; stdcall;
  function DeleteImageCellHeightmap(pState: PTOGL_STATE) : Integer; stdcall;
  function DrawImageCellHeightmap(pState: PTOGL_STATE;
	fDistance, fTwist, fElevation, fAzimuth: Single) : Integer; stdcall;

implementation

  // ImageCell initialisation
  procedure InitialiseImageCell; external 'Imaging.dll' name 'InitialiseImageCell';
  procedure FreeImageCell; external 'Imaging.dll' name 'FreeImageCell';

  // ImageCell Create/Delete functions
  function CreateImageCell; external 'Imaging.dll' name 'CreateImageCell';
  function CreateImageCellFrom; external 'Imaging.dll' name 'CreateImageCellFrom';
  function CreateImageCellFromBMP; external 'Imaging.dll' name 'CreateImageCellFromBMP';

  procedure DeleteImageCell; external 'Imaging.dll' name 'DeleteImageCell';
  procedure PrepareImageCell; external 'Imaging.dll' name 'PrepareImageCell';
  procedure ClearImageCell; external 'Imaging.dll' name 'ClearImageCell';

  procedure ExtractImageCell; external 'Imaging.dll' name 'ExtractImageCell';
  procedure ExtractImageCellToPosition; external 'Imaging.dll' name 'ExtractImageCellToPosition';
  procedure ExtractImageCell24; external 'Imaging.dll' name 'ExtractImageCell24';
  procedure ExtractImageCellToPosition24; external 'Imaging.dll' name 'ExtractImageCellToPosition24';

  // ImageCell Conversion functions
  procedure ImageCellMinSize; external 'Imaging.dll' name 'ImageCellMinSize';
  procedure ImageCellConvertSize; external 'Imaging.dll' name 'ImageCellConvertSize';
  procedure ImageCellCorrectAspect; external 'Imaging.dll' name 'ImageCellCorrectAspect';
  procedure ImageCellCorrectAspect24; external 'Imaging.dll' name 'ImageCellCorrectAspect24';

  procedure InvertImageCell; external 'Imaging.dll' name 'InvertImageCell';
  procedure InvertImageCell24; external 'Imaging.dll' name 'InvertImageCell24';

  procedure LogOpImageCell; external 'Imaging.dll' name 'LogOpImageCell';
  procedure LogOpMultiImage; external 'Imaging.dll' name 'LogOpMultiImage';

  procedure RotateImageCell90; external 'Imaging.dll' name 'RotateImageCell90';
  procedure RotateImageCellFree; external 'Imaging.dll' name 'RotateImageCellFree';

  // ImageCell Characterising / Utility functions
  procedure FindImageCellBounds; external 'Imaging.dll' name 'FindImageCellBounds';

  // ImageCell Drawing functions
  procedure DrawImageCellBitmap; external 'Imaging.dll' name 'DrawImageCellBitmap';
  procedure DrawPartImageCellBitmap; external 'Imaging.dll' name 'DrawPartImageCellBitmap';
  procedure StretchImageCellBitmap; external 'Imaging.dll' name 'StretchImageCellBitmap';
  procedure DrawTransparentBitmap; external 'Imaging.dll' name 'DrawTransparentBitmap';

  procedure Expand24; external 'Imaging.dll' name 'Expand24';
  procedure Expand24Coloured; external 'Imaging.dll' name 'Expand24Coloured';
  procedure Overlay24; external 'Imaging.dll' name 'Overlay24';
  procedure OverlayBlend24; external 'Imaging.dll' name 'OverlayBlend24';
  procedure Blend24; external 'Imaging.dll' name 'Blend24';
  procedure ColouriseGrey24; external 'Imaging.dll' name 'ColouriseGrey24';

  procedure DrawBorder24; external 'Imaging.dll' name 'DrawBorder24';

  // Utility functions for loading and saving 24-bit bitmaps
  function LoadBMP; external 'Imaging.dll' name 'LoadBMP';
  function SaveBMP; external 'Imaging.dll' name 'SaveBMP';

  // Convolution filters
  procedure InitialiseConvolution; external 'Imaging.dll' name 'InitialiseConvolution';
  procedure FreeConvolution; external 'Imaging.dll' name 'FreeConvolution';
  procedure DilateImageCell; external 'Imaging.dll' name 'DilateImageCell';
  procedure ErodeImageCell; external 'Imaging.dll' name 'ErodeImageCell';
  procedure GrowImageCell; external 'Imaging.dll' name 'GrowImageCell';
  procedure CloseImageCell; external 'Imaging.dll' name 'CloseImageCell';
  procedure OpenImageCell; external 'Imaging.dll' name 'OpenImageCell';
  procedure SobelFilter; external 'Imaging.dll' name 'SobelFilter';
  procedure ConditionalSobel; external 'Imaging.dll' name 'ConditionalSobel';
  procedure LaplaceFilter; external 'Imaging.dll' name 'LaplaceFilter';
  procedure ConditionalLaplace; external 'Imaging.dll' name 'ConditionalLaplace';
  procedure MedianFilterHistogram; external 'Imaging.dll' name 'MedianFilterHistogram';
  procedure MedianFilterSorting; external 'Imaging.dll' name 'MedianFilterSorting';

  // Image Processing (Gamma/Range (%))
  procedure ApplyGammaPercRange; external 'Vision3.dll' name 'ApplyGammaPercRange';
  procedure ErodeThresholdPixels; external 'Vision3.dll' name 'ErodeThresholdPixels';
  procedure ErodeScreenedPixels; external 'Vision3.dll' name 'ErodeScreenedPixels';
  procedure AddImages; external 'Vision3.dll' name 'AddImages';
  procedure SubtractImages; external 'Vision3.dll' name 'SubtractImages';

  // OpenGL functions for the 3D view
  function InitImageCellHeightmap; external 'Imaging.dll' name 'InitImageCellHeightmap';
  function DeleteImageCellHeightmap; external 'Imaging.dll' name 'DeleteImageCellHeightmap';
  function DrawImageCellHeightmap; external 'Imaging.dll' name 'DrawImageCellHeightmap';

end.

