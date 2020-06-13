#ifndef __IMAGECELLH
#define __IMAGECELLH

#include "Imaging.h"

#ifdef __cplusplus
extern "C" {
#endif		// __cplusplus

// Initialisation
void IMAGING_API InitialiseImageCell();
void IMAGING_API FreeImageCell();

// ImageCell Create/Delete functions
PIMAGECELL IMAGING_API CreateImageCell(int wWidth, int nHeight, BOOL bClear, BYTE byInitColour);
PIMAGECELL IMAGING_API CreateImageCellFrom(PIMAGECELL pSource, RECT* pRct);
PIMAGECELL IMAGING_API CreateImageCellFromBMP(char* pszBMP, HINSTANCE hInst);

void IMAGING_API DeleteImageCell(PIMAGECELL pImage);
void IMAGING_API PrepareImageCell(PIMAGECELL pImage, int nWidth, int nHeight);
void IMAGING_API ClearImageCell(PIMAGECELL pImage, BYTE byGrey);

void IMAGING_API ExtractImageCell(PIMAGECELL pDest, PIMAGECELL pSource, RECT* pRct);
void IMAGING_API ExtractImageCellToPosition(PIMAGECELL pDest, PIMAGECELL pSource, int nPosX, int nPosY);
void IMAGING_API ExtractImageCell24(PIMAGECELL pDest, PIMAGECELL pSource, RECT* pRct);
void IMAGING_API ExtractImageCellToPosition24(PIMAGECELL pDest, PIMAGECELL pSource, int nPosX, int nPosY);

// ImageCell Conversion functions
void IMAGING_API ImageCellMinSize(PIMAGECELL* ppImage, int nMinWidth, int nMinHeight);
void IMAGING_API ImageCellConvertSize(PIMAGECELL* ppImage, int nNewWidth, int nNewHeight);
void IMAGING_API ImageCellCorrectAspect(PIMAGECELL* ppImage, BYTE byGrey, int nDestWidth, int nDestHeight);
void IMAGING_API ImageCellCorrectAspect24(PIMAGECELL* ppImage, COLORREF clRGB,
				int nDestWidth, int nDestHeight);
void IMAGING_API InvertImageCell(PIMAGECELL pImage);
void IMAGING_API InvertImageCell24(PIMAGECELL pImage);
void IMAGING_API LogOpImageCell(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byLogOp);
void IMAGING_API LogOpMultiImage(PIMAGECELL pDest, PPOINTERNODE pList, BYTE byLogOp);
void IMAGING_API RotateImageCell90(PIMAGECELL* ppImage, bool bRotateRight);
void IMAGING_API RotateImageCellFree(PIMAGECELL pDest, PIMAGECELL pSource, float fAngle);

// ImageCell Characterising / Utility functions
void IMAGING_API FindImageCellBounds(PIMAGECELL pImage, RECT* pRct, BYTE byThreshold);

// ImageCell Drawing functions
void IMAGING_API DrawImageCellBitmap(PIMAGECELL pImage, HDC hDestDC, int nX, int nY);
void IMAGING_API DrawPartImageCellBitmap(PIMAGECELL pImage, HDC hDestDC, RECT* pRct,
				int nX, int nY, DWORD clDesktop);
void IMAGING_API StretchImageCellBitmap(PIMAGECELL pImage, HDC hDestDC, RECT* pRct);
void IMAGING_API DrawTransparentBitmap(PIMAGECELL pImage, HDC hDestDC, int nStartX, int nStartY,
				COLORREF clTransparent);
void IMAGING_API Expand24(PIMAGECELL pImage);
void IMAGING_API Expand24Coloured(PIMAGECELL pImage, COLORREF clRGB);
void IMAGING_API Overlay24(PIMAGECELL pDest, PIMAGECELL pSource, COLORREF clOverlay);
void IMAGING_API OverlayBlend24(PIMAGECELL pDest, PIMAGECELL pOverlay, COLORREF clOverlay, float fAlpha);
void IMAGING_API ColouriseGrey24(PIMAGECELL pImage, COLORREF* paclTable);
void IMAGING_API Blend24(PIMAGECELL pImage, RECT* pRct, COLORREF clRGB, float fAlpha);
void IMAGING_API DrawBorder24(PIMAGECELL pImage, COLORREF clBorderColour);

// Utility functions for loading and saving 24-bit bitmaps
int IMAGING_API LoadBMP(PIMAGECELL pImage, HINSTANCE hInst, char* pszBMP);
int IMAGING_API SaveBMP(PIMAGECELL pImage, char* pszBMP, RECT* pRct);

// Functions not exported
void Reduce8(PIMAGECELL pImage);
void CacheImageYpos(int nWidth, int nHeight);
void CreateImageCellBitmap(PIMAGECELL pImage, HDC hSourceDC);
void CopyEdgesImageCell(PIMAGECELL pDest, PIMAGECELL pSource);
bool WithinImageCell(PIMAGECELL pImage, int nX, int nY);
float GetBoxAngle(PIMAGECELL pImage);

#ifdef __cplusplus
}
#endif		// __cplusplus
#endif		//__IMAGECELLH
