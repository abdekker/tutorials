#include "Imaging.h"
#include "Utilities.h"
#include <math.h>
#include <vector>
using std::vector;

#define WIDTHBYTES(i) ((i+31)/32*4)

// ImageCell cache
typedef struct
{
	// For conversion of 24-bit images to 8-bit images (Reduce8)
	float afReduce8_R[256];
	float afReduce8_G[256];
	float afReduce8_B[256];

	// General purpose cache of the start of each line in an image; if used, this is set by the
	// function processing the image
	int nYposCacheWidth, nYposCacheHeight;
	int anYpos[MAX_IMG_LENGTH];
} ImageCell_Cache;
ImageCell_Cache m_cacheImageCell;

// Initialisation
#pragma optimize( "", off )
void IMAGING_API InitialiseImageCell()
{
	// Initialise the local ImageCell cache

	// Conversion factors for reducing a (colour) 24-bit to an 8-bit image
	int nTmp;
	for (nTmp=0; nTmp < 256; nTmp++)
	{
		m_cacheImageCell.afReduce8_R[nTmp] = (0.299f * static_cast<float>(nTmp));
		m_cacheImageCell.afReduce8_G[nTmp] = (0.587f * static_cast<float>(nTmp));
		m_cacheImageCell.afReduce8_B[nTmp] = (0.114f * static_cast<float>(nTmp));
	}

	// Start of each line in the image
	m_cacheImageCell.nYposCacheWidth = -1;
	m_cacheImageCell.nYposCacheHeight = -1;
	m_cacheImageCell.anYpos[0] = 0;
}
#pragma optimize( "", on )

void IMAGING_API FreeImageCell()
{
	// Closing the imaging functions (free memory if required)
}

// Start: ImageCell Create/Delete functions
PIMAGECELL IMAGING_API CreateImageCell(int nWidth, int nHeight, BOOL bClear, BYTE byInitColour)
{
	// Create a new image of a given size and colour
	IMAGECELL* pNew = new IMAGECELL;
	if (pNew)
	{
		ZeroMemory(pNew, sizeof(IMAGECELL));
		PrepareImageCell(pNew, nWidth, nHeight);
		BYTE byColour = (bClear) ? byInitColour : byWHITE;
		memset(pNew->pBits, byColour, pNew->dwSize);
	}

	return pNew;
}

PIMAGECELL IMAGING_API CreateImageCellFrom(PIMAGECELL pSource, RECT* pRct)
{
	// Create and copy an existing image
	PIMAGECELL pNew = new IMAGECELL;
	if (pNew)
	{
		ZeroMemory(pNew, sizeof(IMAGECELL));
		ExtractImageCell(pNew, pSource, pRct);
	}

	return pNew;
}

PIMAGECELL IMAGING_API CreateImageCellFromBMP(char* pszBMP, HINSTANCE hInst)
{
	// Create a new image from a BMP loaded off disk
	PIMAGECELL pNew = new IMAGECELL;
	if (pNew)
	{
		ZeroMemory(pNew, sizeof(IMAGECELL));
		LoadBMP(pNew, hInst, pszBMP);
	}

	return pNew;
}

void IMAGING_API DeleteImageCell(PIMAGECELL pImage)
{
	// Free the memory assigned for an image
	if (pImage)
	{
		if (pImage->handleBitmap)
		{
			SelectObject(pImage->handleDC, NULL);
			DeleteObject(pImage->handleBitmap);
			DeleteDC(pImage->handleDC);
			pImage->handleBitmap = NULL;
		}

		if (pImage->pBits)
			delete[] pImage->pBits;

		delete pImage;
		pImage = NULL;
	}
}

void IMAGING_API PrepareImageCell(PIMAGECELL pImage, int nWidth, int nHeight)
{
	// Delete the data for an existing image and reconstitute it with a new size
	if (pImage)
	{
		if ((pImage->nWidth != nWidth) || (pImage->nHeight != nHeight))
		{
			// Image is not already the right size...
			if (pImage->pBits)
				delete[] pImage->pBits;

			pImage->nWidth = nWidth;
			pImage->nHeight = nHeight;
			pImage->dwByteWidth = WIDTHBYTES(nWidth * 24);
			pImage->dwSize = (nWidth * nHeight);
			pImage->pBits = new BYTE[pImage->dwSize];

			// The 24-bit copy of this image will need to be created (if required)...
			pImage->b24Valid = false;
		}
	}
}

void IMAGING_API ClearImageCell(PIMAGECELL pImage, BYTE byGrey)
{
	// Set the entire 8-bit image to a specific greyscale value (usually white)
	if (pImage)
	{
		memset(pImage->pBits, byGrey, pImage->dwSize);
		pImage->b24Valid = false;
	}
}

void IMAGING_API ExtractImageCell(PIMAGECELL pDest, PIMAGECELL pSource, RECT* pRct)
{
	// Make a copy of an image (resizes destination image if required)
	if (pDest && pSource)
	{
		if (pRct)
		{
			// Keep rect within bounds!
			if (pRct->top < 0)
				pRct->top = 0;

			if (pRct->left < 0)
				pRct->left = 0;

			if (pRct->right >= pSource->nWidth)
				pRct->right = (pSource->nWidth-1);

			if (pRct->bottom >= pSource->nHeight)
				pRct->bottom = (pSource->nHeight-1);

			// Prepare the destination image prior to the copy
			PrepareImageCell(pDest, (pRct->right - pRct->left + 1), (pRct->bottom - pRct->top + 1));

			// Now copy over the source bits, line by line
			PBYTE pDestBits = pDest->pBits;
			PBYTE pSourceBits = &pSource->pBits[pRct->top*pSource->nWidth + pRct->left];
			for (int nY=pRct->top; nY<=pRct->bottom;
					nY++, pDestBits += pDest->nWidth, pSourceBits += pSource->nWidth)
				memcpy(pDestBits, pSourceBits, pDest->nWidth);
		}
		else
		{
			// No ROI, just copy the entire source image
			PrepareImageCell(pDest, pSource->nWidth, pSource->nHeight);
			memcpy(pDest->pBits, pSource->pBits, pDest->dwSize);
		}

		pDest->b24Valid = false;
	}
}

void IMAGING_API ExtractImageCellToPosition(PIMAGECELL pDest, PIMAGECELL pSource, int nPosX, int nPosY)
{
	// Similar to ExtractImageCell but must be used on a destination image that:
	// 1) Has already been created the correct size
	// 2) Is larger than (and can accomadate entirely in both dimensions) the source image
	if (pDest && pSource)
	{
		if ((pDest->nWidth < (nPosX + pSource->nWidth)) ||
			(pDest->nHeight < (nPosY + pSource->nHeight)))
			return;

		PBYTE pDestBits = &pDest->pBits[(nPosY*pDest->nWidth) + nPosX];
		PBYTE pSourceBits = pSource->pBits;

		for (int nY=nPosY; nY<=(nPosY + pSource->nHeight - 1);
				nY++, pDestBits += pDest->nWidth, pSourceBits += pSource->nWidth)
			memcpy(pDestBits, pSourceBits, pSource->nWidth);

		pDest->b24Valid = false;
	}
}

void IMAGING_API ExtractImageCell24(PIMAGECELL pDest, PIMAGECELL pSource, RECT* pRct)
{
	// Extract a 24-bit image with an optional ROI
	if (pDest && pSource)
	{
		if (pRct)
		{
			// Keep rect within bounds!
			if (pRct->top < 0)
				pRct->top = 0;

			if (pRct->left < 0)
				pRct->left = 0;

			if (pRct->right >= pSource->nWidth)
				pRct->right = (pSource->nWidth-1);

			if (pRct->bottom >= pSource->nHeight)
				pRct->bottom = (pSource->nHeight-1);

			// Prepare the destination image prior to the copy
			PrepareImageCell(pDest, (pRct->right - pRct->left + 1), (pRct->bottom - pRct->top + 1));
		}
		else
		{
			// No ROI, just copy the entire image
			PrepareImageCell(pDest, pSource->nWidth, pSource->nHeight);
		}

		// Create the device context for the destination image
		HDC hDC = GetDC(NULL);
		CreateImageCellBitmap(pDest, hDC);
		ReleaseDC(NULL, hDC);

		// Blit source image onto the DC of the destination image
		if (pRct)
			BitBlt(pDest->handleDC, 0, 0, pDest->nWidth, pDest->nHeight,
				pSource->handleDC, pRct->left, pRct->top, SRCCOPY);
		else
			BitBlt(pDest->handleDC, 0, 0, pDest->nWidth, pDest->nHeight,
				pSource->handleDC, 0, 0, SRCCOPY);

		pDest->b24Valid = true;
	}
}

void IMAGING_API ExtractImageCellToPosition24(PIMAGECELL pDest, PIMAGECELL pSource, int nPosX, int nPosY)
{
	// Similar to ExtractImageCell24 but must be used on a destination image that:
	// 1) Has already been created the correct size
	// 2) Is larger than (and can accomadate entirely in both dimensions) the source image
	// 3) Both images must already be 24-bit colour images
	if (pDest && pSource)
	{
		if ((pDest->nWidth < (nPosX + pSource->nWidth)) ||
			(pDest->nHeight < (nPosY + pSource->nHeight)))
			return;

		PBYTE pDestBits = &pDest->pBitmapBits[nPosX*3 + (nPosY*pDest->dwByteWidth)];
		PBYTE pSourceBits = pSource->pBitmapBits;

		// We have to put in compensation for rotated images
		BYTE byComp = 0;
		if (pDest->nWidth > pSource->nWidth)
			byComp = (pSource->nWidth % 4);

		for (int nY=nPosY; nY<(nPosY + pSource->nHeight);
				nY++,
				pDestBits += (pDest->dwByteWidth),
				pSourceBits += (pSource->dwByteWidth))
			memcpy(pDestBits, pSourceBits, pSource->dwByteWidth-byComp);

		pDest->b24Valid = true;
	}
}
// End: ImageCell Create/Delete functions

// Start: ImageCell Conversion functions
void IMAGING_API ImageCellMinSize(PIMAGECELL* ppImage, int nMinWidth, int nMinHeight)
{
	// Increase the image size in one or both directions (if required)
	if (ppImage)
	{
		int nSourceWidth = (*ppImage)->nWidth;
		int nSourceHeight = (*ppImage)->nHeight;
		if ((nSourceWidth >= nMinWidth) && (nSourceHeight >= nMinHeight))
			return;		// Image is already large enough...

		PIMAGECELL pMinImage = CreateImageCell(max(nSourceWidth, nMinWidth),
			max(nSourceHeight, nMinHeight), TRUE, byWHITE);
		if (pMinImage)
		{
			PBYTE pCopiedBits = pMinImage->pBits;
			PBYTE pSourceBits = (*ppImage)->pBits;

			for (int nY=0; nY<nSourceHeight;
					nY++, pCopiedBits += nMinWidth, pSourceBits += nSourceWidth)
				memcpy(pCopiedBits, pSourceBits, nSourceWidth);

			DeleteImageCell(*ppImage);
			*ppImage = pMinImage;
		}
	}
}

void IMAGING_API ImageCellConvertSize(PIMAGECELL* ppImage, int nNewWidth, int nNewHeight)
{
	// Convert the image to a new size (8-bit)
	// Note: This does not stretch the image!
	if (ppImage)
	{
		int nSourceWidth = (*ppImage)->nWidth;
		int nSourceHeight = (*ppImage)->nHeight;
		if ((nSourceWidth == nNewWidth) && (nSourceHeight == nNewHeight))
			return;		// Image is already the right size...

		// Create new image
		PIMAGECELL pConvertImage = CreateImageCell(nNewWidth, nNewHeight, TRUE, byWHITE);
		if (pConvertImage)
		{
			PBYTE pCopiedBits = pConvertImage->pBits;
			PBYTE pSourceBits = (*ppImage)->pBits;

			for (int nY=0; nY<min(nSourceHeight, nNewHeight);
					nY++, pCopiedBits += nNewWidth, pSourceBits += nSourceWidth)
				memcpy(pCopiedBits, pSourceBits, min(nSourceWidth, nNewWidth));

			DeleteImageCell(*ppImage);
			*ppImage = pConvertImage;
		}
	}
}

void IMAGING_API ImageCellCorrectAspect(PIMAGECELL* ppImage, BYTE byGrey, int nDestWidth, int nDestHeight)
{
	// Correct the aspect ratio of the current image based on the output canvas size (8-bit)
	if (ppImage)
	{
		// Get the dimensions of the current image
		int nSourceWidth = (*ppImage)->nWidth;
		int nSourceHeight = (*ppImage)->nHeight;
		if ((nSourceWidth == nDestWidth) && (nSourceHeight == nDestHeight))
			return;		// Image is already the right size...

		if ((nSourceHeight == 0) || (nDestHeight == 0))
			return;		// One of the images has invalid dimensions...

		// What is the aspect ratio of this image and the destination canvas?
		float fAspectSource = static_cast<float>(nSourceWidth) / static_cast<float>(nSourceHeight);
		float fAspectDest = static_cast<float>(nDestWidth) / static_cast<float>(nDestHeight);

		// Do we need to add some height or width?
		int nNewWidth, nNewHeight;
		int nPosX, nPosY;
		if (fAspectSource > fAspectDest)
		{
			// Make source longer (add height)
			nNewWidth = nSourceWidth;
			nNewHeight = static_cast<int>((static_cast<float>(nSourceWidth) / fAspectDest) + 0.5f);
			nPosX = 0;
			nPosY = ((nNewHeight - nSourceHeight) / 2);
		}
		else
		{
			// Make source wider (add width)
			nNewWidth = static_cast<int>((static_cast<float>(nSourceHeight) * fAspectDest) + 0.5f);
			nNewHeight = nSourceHeight;
			nPosX = ((nNewWidth - nSourceWidth) / 2);
			nPosY = 0;
		}

		// Create the aspect-corrected image
		PIMAGECELL pCorrectedImage = CreateImageCell(nNewWidth, nNewHeight, TRUE, byGrey);
		if (pCorrectedImage)
		{
			ExtractImageCellToPosition(pCorrectedImage, (*ppImage), nPosX, nPosY);
			DeleteImageCell(*ppImage);
			*ppImage = pCorrectedImage;
		}
	}
}

void IMAGING_API ImageCellCorrectAspect24(PIMAGECELL* ppImage, COLORREF clRGB, int nDestWidth, int nDestHeight)
{
	// Correct the aspect ratio of the current image based on the output canvas size (24-bit)
	if (ppImage)
	{
		// Get the dimensions of the current image
		int nSourceWidth = (*ppImage)->nWidth;
		int nSourceHeight = (*ppImage)->nHeight;
		if ((nSourceWidth == nDestWidth) && (nSourceHeight == nDestHeight))
			return;		// Image is already the right size...

		if ((nSourceHeight == 0) || (nDestHeight == 0))
			return;		// One of the images has invalid dimensions...

		// What is the aspect ratio of this image and the destination canvas?
		float fAspectSource = static_cast<float>(nSourceWidth) / static_cast<float>(nSourceHeight);
		float fAspectDest = static_cast<float>(nDestWidth) / static_cast<float>(nDestHeight);

		// Get the difference between the aspect ratios. If they are essentially the same, then no
		// change will be required.
		float fDiff = (fAspectSource > fAspectDest)
			? (fAspectSource - fAspectDest)
			: (fAspectDest - fAspectSource);
		if (fDiff > MIN_FLOAT)
		{
			// Do we need to add height or width?
			int nNewWidth, nNewHeight;
			int nPosX, nPosY;
			if (fAspectSource > fAspectDest)
			{
				// Make source longer (add height)
				nNewWidth = nSourceWidth;
				nNewHeight = static_cast<int>((static_cast<float>(nSourceWidth) / fAspectDest) + 0.5f);
				nPosX = 0;
				nPosY = ((nNewHeight - nSourceHeight) / 2);
			}
			else
			{
				// Make source wider (add width)
				nNewWidth = static_cast<int>((static_cast<float>(nSourceHeight) * fAspectDest) + 0.5f);
				nNewHeight = nSourceHeight;
				nPosX = ((nNewWidth - nSourceWidth) / 2);
				nPosY = 0;
			}

			// Create the aspect-corrected image
			PIMAGECELL pCorrectedImage = CreateImageCell(nNewWidth, nNewHeight, TRUE, byWHITE);
			if (pCorrectedImage)
			{
				Expand24Coloured(pCorrectedImage, clRGB);
				ExtractImageCellToPosition24(pCorrectedImage, (*ppImage), nPosX, nPosY);
				DeleteImageCell(*ppImage);
				*ppImage = pCorrectedImage;
			}
		}
	}
}

void IMAGING_API InvertImageCell(PIMAGECELL pImage)
{
	// Convert lighter pixels to darker pixels and vice versa (8-bit)
	if (pImage)
	{
		PBYTE pSourceBits = pImage->pBits;
		DWORD dwByteCount = pImage->dwSize;
		while (dwByteCount--)
		{
			*pSourceBits = (255 - *pSourceBits);
			pSourceBits++;
		}

		pImage->b24Valid = false;
	}
}

void IMAGING_API InvertImageCell24(PIMAGECELL pImage)
{
	// Convert lighter pixels to darker pixels and vice versa (24-bit)
	if (pImage)
	{
		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		// Perform the calculations of inverting each pixel in the original B&W image
		PBYTE pSourceBits = pImage->pBits;
		PBYTE pDestBits = pImage->pBitmapBits;
		if (pDestBits)
		{
			DWORD dwNextLineOffset = (pImage->dwByteWidth - (pImage->nWidth*3));
			for (int nY=0; nY<pImage->nHeight; nY++)
			{
				for (int nX=0; nX<pImage->nWidth; nX++, pSourceBits++)
				{
					*pDestBits++ = (255 - *pSourceBits);
					*pDestBits++ = (255 - *pSourceBits);
					*pDestBits++ = (255 - *pSourceBits);
				}

				pDestBits += dwNextLineOffset;
			}
		}
	}
}

void IMAGING_API LogOpImageCell(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byLogOp)
{
	// There are two classes of logical operations in combining images:

	// OR/AND: Scan through the image pixels and if both (opAND) or either (opOR) is black then
	// set the destination image pixel to black. (opOR_MASK) is a variant of (opOR) where the
	// target pixel is set to white; useful for preparing source images with an overlaid Mask.

	// SUBTRACT: Scan through the image and remove (ie. set to white) any destination iamge pixels
	// where the source is black

	// MIN/MAX: Scan through the image pixels and select the smallest (opMIN) or the largest
	// (opMAX) of the two pixels for the destination image
	if (pDest && pSource)
	{
		PBYTE pDestBits = pDest->pBits;
		PBYTE pSourceBits = pSource->pBits;
		DWORD dwByteCount = pSource->dwSize;
		switch (byLogOp)
		{
			case opAND:
				// * If source pixel is white, clear destination pixel
				// * Operates on binary (thresholded) images
				while (dwByteCount--)
				{
					if (*pSourceBits == byWHITE)
						*pDestBits = byWHITE;

					// Note: Above only modifies destination pixels if the source pixel is white.
					// If a guaranteed binary image is required use:
					/*if ((*pDestBits == byBLACK) && (*pSourceBits == byBLACK))
						*pDestBits = byBLACK;
					else
						*pDestBits = byWHITE;*/
			
					pDestBits++;
					pSourceBits++;
				}
				break;

			case opOR:
				// * If source pixel is black, set destination pixel
				// * Operates on binary (thresholded) images
				// * Note: This is like the logical "add" operator
				while (dwByteCount--)
				{
					if (*pSourceBits == byBLACK)
						*pDestBits = byBLACK;

					// Note: Above only modifies destination pixels if the source pixel is black.
					// If a guaranteed binary image is required use:
					/*if ((*pDestBits == byBLACK) || (*pSourceBits == byBLACK))
						*pDestBits = byBLACK;
					else
						*pDestBits = byWHITE;*/
			
					pDestBits++;
					pSourceBits++;
				}
				break;

			case opOR_MASK:
				// Special version of (opOR) for use with a Mask
				// * Operates on binary (thresholded) images
				while (dwByteCount--)
				{
					if (*pSourceBits == byBLACK)
						*pDestBits = byWHITE;
			
					pDestBits++;
					pSourceBits++;
				}
				break;

			case opSUBTRACT:
				// * Remove pixels from the destination if set in the source
				// * Operates on binary (thresholded) images
				// * Note: This is like the logical "subtract" operator
				while (dwByteCount--)
				{
					if (*pSourceBits == byBLACK)
						*pDestBits = byWHITE;
			
					pDestBits++;
					pSourceBits++;
				}
				break;

			case opMIN:
				// * Use the smallest (ie. darkest) value
				// * Operates on greyscale (0-255) images
				while (dwByteCount--)
				{
					if (*pSourceBits < *pDestBits)
						*pDestBits = *pSourceBits;
			
					pDestBits++;
					pSourceBits++;
				}
				break;

			case opMAX:
				// * Use the largest (ie. lightest) value
				// * Operates on greyscale (0-255) images
				while (dwByteCount--)
				{
					if (*pSourceBits > *pDestBits)
						*pDestBits = *pSourceBits;
			
					pDestBits++;
					pSourceBits++;
				}
				break;
		}
	}
}

void IMAGING_API LogOpMultiImage(PIMAGECELL pDest, PPOINTERNODE pList, BYTE byLogOp)
{
	// Similar to "LogOpImageCell" but takes an arbitrarily long linked list of images. Output of
	// the logical operations is written to the destination image. The caller should provide the
	// destination image separately.
	if (pDest && pList)
	{
		// Place pointers to all image bits into a temporary container
		vector<PBYTE> vecImageBits;
		if (pList->pData != NULL)
			vecImageBits.push_back(PIMAGECELL(pList->pData)->pBits);

		PPOINTERNODE pTmp = pList->pNextLink;
		while (pTmp != NULL)
		{
			// Does this node have an image?
			if (pTmp->pData != NULL)
				vecImageBits.push_back(PIMAGECELL(pTmp->pData)->pBits);

			// Onto the next node!
			pTmp = pTmp->pNextLink;
		}

		// Now many images do we have?
		size_t nTotalImages = vecImageBits.size();
		if (nTotalImages == 0)
			return;

		// Now apply the logical operation
		// Note: Only a subset of the logical operators defined in "LogOpImageCell" can be applied
		PBYTE pDestBits = pDest->pBits;
		DWORD dwByteCount = pDest->dwSize;
		size_t nImage;
		bool bApplyOperation;
		BYTE byPixel;
		switch (byLogOp)
		{
			case opAND:
				// * If any source pixel is white, clear destination pixel
				// * Operates on binary (thresholded) images
				while (dwByteCount--)
				{
					bApplyOperation = false;
					for (nImage=0; nImage < nTotalImages; nImage++)
					{
						if (*vecImageBits[nImage] == byWHITE)
							bApplyOperation = true;

						vecImageBits[nImage]++;
					}

					if (bApplyOperation)
						*pDestBits = byWHITE;

					pDestBits++;
				}
				break;

			case opOR:
				// * If any source pixel is black, set destination pixel
				// * Operates on binary (thresholded) images
				// * Note: This is like the logical "add" operator
				while (dwByteCount--)
				{
					bApplyOperation = false;
					for (nImage=0; nImage < nTotalImages; nImage++)
					{
						if (*vecImageBits[nImage] == byBLACK)
							bApplyOperation = true;

						vecImageBits[nImage]++;
					}

					if (bApplyOperation)
						*pDestBits = byBLACK;

					pDestBits++;
				}
				break;

			case opMIN:
				// * Use the smallest (ie. darkest) value
				// * Operates on greyscale (0-255) images
				while (dwByteCount--)
				{
					byPixel = 255;
					for (size_t nImage=0; nImage < nTotalImages; nImage++)
					{
						if (*vecImageBits[nImage] < byPixel)
							byPixel = *vecImageBits[nImage];

						vecImageBits[nImage]++;
					}

					*pDestBits = byPixel;
					pDestBits++;
				}
				break;

			case opMAX:
				// * Use the largest (ie. lightest) value
				// * Operates on greyscale (0-255) images
				while (dwByteCount--)
				{
					byPixel = 0;
					for (nImage=0; nImage < nTotalImages; nImage++)
					{
						if (*vecImageBits[nImage] > byPixel)
							byPixel = *vecImageBits[nImage];

						vecImageBits[nImage]++;
					}

					*pDestBits = byPixel;
					pDestBits++;
				}
				break;
		}

		#ifdef _DEBUG
		// Uncomment to test saving all images in the linked list to disk
		/*TCHAR atcTmp[100];
		if (pList->pData != NULL)
		{
			sprintf(atcTmp, "C:\\Image_%02d.bmp", nImage++);
			SaveBMP(PIMAGECELL(pList->pData), atcTmp, NULL);
		}

		pTmp = pList->pNextLink;
		while (pTmp != NULL)
		{
			if (pTmp->pData != NULL)
			{
				sprintf(atcTmp, "C:\\Image_%02d.bmp", nImage++);
				SaveBMP(PIMAGECELL(pTmp->pData), atcTmp, NULL);
			}

			pTmp = pTmp->pNextLink;
		}*/
		#endif

		// Clean up
		vecImageBits.clear();
	}
}

void IMAGING_API RotateImageCell90(PIMAGECELL* ppImage, bool bRotateRight)
{
	// Rotate the image (90° left or right only)
	if (ppImage)
	{
		int nWidth = (*ppImage)->nWidth;
		int nHeight = (*ppImage)->nHeight;
		PIMAGECELL pRotatedImage = CreateImageCell(nHeight, nWidth, TRUE, byWHITE);
		if (pRotatedImage)
		{
			// Cache the start of each line
			CacheImageYpos(nWidth, nHeight);

			// Rotate the image
			int nX, nY, nXpos;
			for (nX=0; nX<nWidth; nX++)
			{
				if (bRotateRight)
					nXpos = (nX*nHeight);
				else
					nXpos = ((nWidth-nX-1)*nHeight);

				for (nY=0; nY<nHeight; nY++)
				{
					if (bRotateRight)
						pRotatedImage->pBits[nXpos + (nHeight-nY-1)] =
							(*ppImage)->pBits[nX + m_cacheImageCell.anYpos[nY]];
					else
						pRotatedImage->pBits[nXpos + nY] =
							(*ppImage)->pBits[nX + m_cacheImageCell.anYpos[nY]];
				}
			}

			// Set rotated image
			DeleteImageCell(*ppImage);
			*ppImage = pRotatedImage;
		}
	}
}

void IMAGING_API RotateImageCellFree(PIMAGECELL pDest, PIMAGECELL pSource, float fAngle)
{
	// Perform a free rotation of the given source image
	// Note: For a fixed rotation of 90° see "RotateImageCell90"
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Start off with nothing
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Following algorithm adapted from:
		// https://stackoverflow.com/questions/22022484
		float fCos = cosf(-fAngle);
		float fSin = sinf(-fAngle);
		float H[] = {
			fCos,	-fSin,
				(static_cast<float>(nWidth) / 2.0f) - (static_cast<float>(nWidth * fCos) / 2.0f),
			fSin,	fCos,
				(static_cast<float>(nHeight) / 2.0f) - (static_cast<float>(nHeight * fCos) / 2.0f) };

		PBYTE pDestBits = pDest->pBits;
		int nX, nY, nSourceX, nSourceY;
		float fTmpX, fTmpY;
		for (nY=0; nY<nHeight; nY++)
		{
			fTmpX = ((H[1] * nY) + H[2] + 0.5f);
			fTmpY = ((H[4] * nY) + H[5] + 0.5f);
			for (nX=0; nX<nWidth; nX++, pDestBits++)
			{
				// Rotate source pixel to the required position in the destination image
				nSourceX = clamp(static_cast<int>((H[0] * nX) + fTmpX), 0, (nWidth - 1));
				nSourceY = clamp(static_cast<int>((H[3] * nX) + fTmpY), 0, (nHeight - 1));
				*pDestBits = pSource->pBits[(nSourceY * nWidth) + nSourceX];

				// To generate a random image:
				//*pDestBits = (rand() % 256);
			}
		}
	}
}
// End: ImageCell Conversion functions

// Start: ImageCell Characterising function
void IMAGING_API FindImageCellBounds(PIMAGECELL pImage, RECT* pRct, BYTE byThreshold)
{
	// Find the maximum bounding rect
	if (pImage && pRct)
	{
		// Start off with the entire image
		pRct->left = 0;
		pRct->top = 0;
		pRct->right = (pImage->nWidth - 1);
		pRct->bottom = (pImage->nHeight - 1);

		// Find top edge
		bool bFoundTopEdge = false;
		PBYTE pPixel = pImage->pBits;
		int nX, nY;
		for (nY=0; nY<pImage->nHeight; nY++)
		{
			for (nX=0; nX<pImage->nWidth; nX++, pPixel++)
			{
				if (*pPixel <= byThreshold)
				{
					bFoundTopEdge = true;
					pRct->top = nY;
					nY = pImage->nHeight;
					break;
				}
			}
		}

		// If we didn't find the top edge, there are no pixels below the threshold in the image...
		if (!bFoundTopEdge)
			return;

		// Find bottom edge
		pPixel = &pImage->pBits[(pImage->nHeight*pImage->nWidth)-1];
		for (nY=pImage->nHeight-1; nY>=0; nY--)
		{
			for (nX=pImage->nWidth; nX>0; nX--, pPixel--)
			{
				if (*pPixel <= byThreshold)
				{
					pRct->bottom = nY;
					nY = 0;
					break;
				}
			}
		}

		// Find left edge
		int nTopPos = (pRct->top * pImage->nWidth);
		for (nX=0; nX<=pRct->right; nX++)
		{
			pPixel = &pImage->pBits[nTopPos + nX];
			for (nY=pRct->top; nY<pRct->bottom; nY++, pPixel += pImage->nWidth)
			{
				if (*pPixel <= byThreshold)
				{
					pRct->left = nX;
					nX = pImage->nWidth;
					break;
				}
			}
		}

		// Find right edge
		for (nX=pImage->nWidth-1; nX>=pRct->left; nX--)
		{
			pPixel = &pImage->pBits[nTopPos + nX];
			for (nY=pRct->top; nY<pRct->bottom; nY++, pPixel += pImage->nWidth)
			{
				if (*pPixel <= byThreshold)
				{
					pRct->right = nX;
					nX = 0;
					break;
				}
			}
		}
	}
}
// End: ImageCell Characterising function

// Start: ImageCell Drawing functions
void IMAGING_API DrawImageCellBitmap(PIMAGECELL pImage, HDC hDestDC, int nX, int nY)
{
	if (pImage)
	{
		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		BitBlt(hDestDC, nX, nY, pImage->nWidth, pImage->nHeight,
			pImage->handleDC, 0, 0, SRCCOPY);
	}
}

void IMAGING_API DrawPartImageCellBitmap(PIMAGECELL pImage, HDC hDestDC, RECT* pRct,
										int nX, int nY, DWORD clDesktop)
{
	if (pImage && pRct)
	{
		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);
		
		int nBltWidth = (pRct->right - pRct->left + 1);
		int nBltHeight = (pRct->bottom - pRct->top + 1);

		BitBlt(hDestDC, nX, nY, nBltWidth, nBltHeight,
			pImage->handleDC, pRct->left, pRct->top, SRCCOPY);

		HBRUSH hBrush = CreateSolidBrush(clDesktop);
		HBRUSH hOldBrush = (HBRUSH)SelectObject(hDestDC, hBrush);

		RECT rct;
		if (pImage->nWidth < nBltWidth)
		{
			rct.left = pImage->nWidth;
			rct.top = 0;
			rct.right = nBltWidth;
			rct.bottom = nBltHeight;
			FillRect(hDestDC, &rct, hBrush);
		}

		if (pImage->nHeight < nBltHeight)
		{
			rct.left = 0;
			rct.top = pImage->nHeight;
			rct.right = nBltWidth;
			rct.bottom = nBltHeight;
			FillRect(hDestDC, &rct, hBrush);
		}

		// Clean up
		SelectObject(hDestDC, hOldBrush);
		DeleteObject(hBrush);
	}
}

void IMAGING_API StretchImageCellBitmap(PIMAGECELL pImage, HDC hDestDC, RECT* pRct)
{
	if (pImage && pRct)
	{
		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		int nPrevMode = SetStretchBltMode(hDestDC, HALFTONE);
		StretchBlt(hDestDC, pRct->left, pRct->top,
			pRct->right - pRct->left + 1, pRct->bottom - pRct->top + 1,
			pImage->handleDC, 0, 0,
			pImage->nWidth, pImage->nHeight, SRCCOPY);

		SetStretchBltMode(hDestDC, nPrevMode);
	}
}

void IMAGING_API DrawTransparentBitmap(PIMAGECELL pImage, HDC hDestDC,
									int nStartX, int nStartY,
									COLORREF clTransparent)
{
	// Draw a bitmap onto a device context and remove background pixels from the bitmap defined
	// by the transparent colour
	if (pImage)
	{
		// Get the RGB components of the transparent colour
		BYTE byR = GetRValue(clTransparent);
		BYTE byG = GetGValue(clTransparent);
		BYTE byB = GetBValue(clTransparent);

		// Move through the bitmap identifying the pixels to copy to the DC
		PBYTE pbyR;
		PBYTE pbyG;
		PBYTE pbyB;

		int nX, nY;
		for (nY=0; nY<pImage->nHeight; nY++)
		{
			pbyB = pImage->pBitmapBits;
			pbyB += (nY*pImage->dwByteWidth);
			for (nX=0; nX<pImage->nWidth; nX++)
			{
				pbyG = pbyB;
				pbyG++;
				pbyR = pbyG;
				pbyR++;

				// The transparent pixel is usually RGB(255,0,255), a shade of pink
				if ((*pbyR != byR) || (*pbyG != byG) || (*pbyB != byB))
					SetPixel(hDestDC, nStartX + nX, nStartY + nY, RGB(*pbyR,*pbyG,*pbyB));

				pbyB += 3;
			}
		}
	}
}

void IMAGING_API Expand24(PIMAGECELL pImage)
{
	// Convert 8-bit image to 24-bit (with exactly the same greyscale ie. R=G=B == B&W)
	if (pImage && !pImage->b24Valid)
	{
		// Create a DC for the destination image (if required)
		HDC hDC = GetDC(NULL);
		CreateImageCellBitmap(pImage, hDC);
		ReleaseDC(NULL, hDC);

		// Perform the calculations
		PBYTE pSourceBits = pImage->pBits;
		PBYTE pDestBits = pImage->pBitmapBits;
		DWORD dwNextLineOffset = (pImage->dwByteWidth - (pImage->nWidth*3));
		for (int nY=0; nY<pImage->nHeight; nY++)
		{
			for (int nX=0; nX<pImage->nWidth; nX++, pSourceBits++)
			{
				*pDestBits++ = *pSourceBits;
				*pDestBits++ = *pSourceBits;
				*pDestBits++ = *pSourceBits;
			}

			pDestBits += dwNextLineOffset;
		}

		// Image is now 24-bit
		pImage->b24Valid = true;
	}
}

void IMAGING_API Expand24Coloured(PIMAGECELL pImage, COLORREF clRGB)
{
	// Convert 8-bit image to 24-bit, using the specified colour.
	// Note: The expansion does not retain any image data from the source image.
	if (pImage && !pImage->b24Valid)
	{
		HDC hDC = GetDC(NULL);
		CreateImageCellBitmap(pImage, hDC);
		ReleaseDC(NULL, hDC);

		PBYTE pSourceBits = pImage->pBits;
		PBYTE pDesBits = pImage->pBitmapBits;
		if (pDesBits)
		{
			BYTE byR = GetRValue(clRGB);
			BYTE byG = GetGValue(clRGB);
			BYTE byB = GetBValue(clRGB);

			DWORD dwNextLineOffset = (pImage->dwByteWidth - (pImage->nWidth*3));
			for (int nY=0; nY<pImage->nHeight; nY++)
			{
				for (int nX=0; nX<pImage->nWidth; nX++, pSourceBits++)
				{
					*pDesBits++ = byB;
					*pDesBits++ = byG;
					*pDesBits++ = byR;
				}

				pDesBits += dwNextLineOffset;
			}
		}

		pImage->b24Valid = true;
	}
}

void IMAGING_API Overlay24(PIMAGECELL pDest, PIMAGECELL pSource, COLORREF clOverlay)
{
	// Overlay one image on top of another
	if (pDest && pSource)
	{
		// Make sure the images are the same size
		if ((pSource->nWidth != pDest->nWidth) ||
			(pSource->nHeight != pDest->nHeight))
			return;

		// Make sure the destination image is 24-bit
		if (!pDest->b24Valid)
			Expand24(pDest);

		// Get the colours for the overlay
		BYTE byR = GetRValue(clOverlay);
		BYTE byG = GetGValue(clOverlay);
		BYTE byB = GetBValue(clOverlay);

		// Perform the calculations
		PBYTE pSourceBits = pSource->pBits;
		PBYTE pDestBits = pDest->pBitmapBits;
		DWORD dwNextLineOffset = (pDest->dwByteWidth - (pDest->nWidth*3));
		for (int nY=0; nY<pDest->nHeight; nY++)
		{
			for (int nX=0; nX<pDest->nWidth; nX++, pSourceBits++)
			{
				if (*pSourceBits == byBLACK)
				{
					*pDestBits++ = byB;
					*pDestBits++ = byG;
					*pDestBits++ = byR;
				}
				else
					pDestBits += 3;
			}

			pDestBits += dwNextLineOffset;
		}
	}
}

void IMAGING_API OverlayBlend24(PIMAGECELL pDest, PIMAGECELL pOverlay, COLORREF clOverlay, float fAlpha)
{
	// Blend one image over another (eg. the Mask)
	if (pDest && pOverlay)
	{
		// Make sure the images are the same size
		if ((pOverlay->nWidth != pDest->nWidth) ||
			(pOverlay->nHeight != pDest->nHeight))
			return;

		// Make sure the destination image is 24-bit
		if (!pDest->b24Valid)
			Expand24(pDest);

		// Get the colours for the overlay
		BYTE byR = (BYTE)((float)GetRValue(clOverlay) * fAlpha);
		BYTE byG = (BYTE)((float)GetGValue(clOverlay) * fAlpha);
		BYTE byB = (BYTE)((float)GetBValue(clOverlay) * fAlpha);
		float fDestAlpha = (1.0f - fAlpha);

		// Cache calculations for the blend operation
		int nX, nY;
		BYTE abyBlend[256];
		for (nX=0; nX<256; nX++)
			abyBlend[nX] = (BYTE)(fDestAlpha * nX);

		// Perform the calculations
		PBYTE pDestBits;
		PBYTE pOverlayBits = pOverlay->pBits;
		for (nY=0; nY<pDest->nHeight; nY++)
		{
			pDestBits = &pDest->pBitmapBits[nY*pDest->dwByteWidth];
			for (nX=0; nX<pDest->nWidth; nX++, pOverlayBits++)
			{
				if (*pOverlayBits != byWHITE)
				{
					*pDestBits++ = (abyBlend[*pDestBits] + byB);
					*pDestBits++ = (abyBlend[*pDestBits] + byG);
					*pDestBits++ = (abyBlend[*pDestBits] + byR);
				}
				else
					pDestBits += 3;
			}
		}
	}
}

void IMAGING_API Blend24(PIMAGECELL pImage, RECT* pRct, COLORREF clRGB, float fAlpha)
{
	// Add an overlay to the 24-bit image in the specified area. The alpha value determines the
	// transparency of the overlay.
	if (pImage)
	{
		// Make sure the destination image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		// Find the area of interest
		if (pRct == NULL)
		{
			RECT rctLocal;
			rctLocal.left = 0;
			rctLocal.top = 0;
			rctLocal.right = (pImage->nWidth-1);
			rctLocal.bottom = (pImage->nHeight-1);
			pRct = &rctLocal;
		}
		else
		{
			if (pRct->left < 0)
				pRct->left = 0;

			if (pRct->top < 0)
				pRct->top = 0;

			if (pRct->right >= (pImage->nWidth-1))
				pRct->right = (pImage->nWidth-1);

			if (pRct->bottom >= (pImage->nHeight-1))
				pRct->bottom = (pImage->nHeight-1);
		}

		// Get the colours for the blend (overlay)
		BYTE byR = (BYTE)((float)GetRValue(clRGB) * fAlpha);
		BYTE byG = (BYTE)((float)GetGValue(clRGB) * fAlpha);
		BYTE byB = (BYTE)((float)GetBValue(clRGB) * fAlpha);
		float fDestAlpha = (1.0f - fAlpha);

		// Cache calculations for the blend operation
		int nX, nY;
		BYTE abyBlend[256];
		for (nX=0; nX<256; nX++)
			abyBlend[nX] = (BYTE)(fDestAlpha * nX);

		// Perform the calculations
		PBYTE pImageBits = NULL;
		for (nY=pRct->top; nY<=pRct->bottom; nY++)
		{
			pImageBits = &pImage->pBitmapBits[(nY*pImage->dwByteWidth) + (pRct->left*3)];
			for (nX=pRct->left; nX<=pRct->right; nX++)
			{
				*pImageBits++ = (abyBlend[*pImageBits] + byB);
				*pImageBits++ = (abyBlend[*pImageBits] + byG);
				*pImageBits++ = (abyBlend[*pImageBits] + byR);
			}
		}
	}
}

void IMAGING_API ColouriseGrey24(PIMAGECELL pImage, COLORREF* paclTable)
{
	// Using a colour lookup table, colourise a B&W image
	if (pImage)
	{
		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		// Perform the calculations
		int nX, nY;
		PBYTE pSourceBits = pImage->pBits;
		PBYTE pDestBits = pImage->pBitmapBits;
		DWORD dwNextLineOffset = (pImage->dwByteWidth - (pImage->nWidth*3));
		for (nY=0; nY<pImage->nHeight; nY++)
		{
			for (nX=0; nX<pImage->nWidth; nX++, pSourceBits++)
			{
				*pDestBits++ = GetBValue(paclTable[*pSourceBits]);
				*pDestBits++ = GetGValue(paclTable[*pSourceBits]);
				*pDestBits++ = GetRValue(paclTable[*pSourceBits]);
			}

			pDestBits += dwNextLineOffset;
		}
	}
}

void IMAGING_API DrawBorder24(PIMAGECELL pImage, COLORREF clBorderColour)
{
	// Draw a simple border around the edge of an ImageCell. Usually cosmetic.
	if (pImage)
	{
		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		// Get width and height of target image
		int nWidth = pImage->nWidth;
		int nHeight = pImage->nHeight;

		// Get the RGB components of the transparent colour
		BYTE byR = GetRValue(clBorderColour);
		BYTE byG = GetGValue(clBorderColour);
		BYTE byB = GetBValue(clBorderColour);

		// Top row
		int nX, nY;
		DWORD dwTmp = 0;
		PBYTE pDestBits;
		for (nX=0; nX<nWidth; nX++)
		{
			pDestBits = &pImage->pBitmapBits[nX*3];
			*pDestBits++ = byB;
			*pDestBits++ = byG;
			*pDestBits++ = byR;
		}

		// Left side
		for (nY=0; nY<nHeight; nY++)
		{
			pDestBits = &pImage->pBitmapBits[nY*pImage->dwByteWidth];
			*pDestBits++ = byB;
			*pDestBits++ = byG;
			*pDestBits++ = byR;
		}

		// Right side
		for (nY=0; nY<nHeight; nY++)
		{
			pDestBits = &pImage->pBitmapBits[(nY*pImage->dwByteWidth) + ((nWidth - 1)*3)];
			*pDestBits++ = byB;
			*pDestBits++ = byG;
			*pDestBits++ = byR;
		}

		// Bottom row
		for (nX=0; nX<nWidth; nX++)
		{
			pDestBits = &pImage->pBitmapBits[((nHeight - 1)*pImage->dwByteWidth) + (nX*3)];
			*pDestBits++ = byB;
			*pDestBits++ = byG;
			*pDestBits++ = byR;
		}
	}
}
// End: ImageCell Drawing functions

// Start: Utility functions for loading and saving 24-bit bitmaps
int IMAGING_API LoadBMP(PIMAGECELL pImage, HINSTANCE hInst, char* pszBMP)
{
	// Note: This function does not faithfully reproduce the source image, though it is fairly
	// close. For example, a 24-bit BMP with RGB = (0,64,192) is converted to RGB = (0,65,198) in
	// the new image. The reason for this is unknown, possibly related to the BMP colour table.
	if (pImage)
	{
		HBITMAP hBMP = (HBITMAP)LoadImage(hInst, pszBMP, IMAGE_BITMAP, 0, 0,
			(hInst == NULL) ? LR_LOADFROMFILE : 0);
		if (hBMP == NULL)
			return false;

		BITMAP bitmap;
		GetObject(hBMP, sizeof(BITMAP), &bitmap);
		PrepareImageCell(pImage, bitmap.bmWidth, bitmap.bmHeight);

		HDC hDC = GetDC(NULL);
		CreateImageCellBitmap(pImage, hDC);
		ReleaseDC(NULL, hDC);

		HDC hBmpDC = CreateCompatibleDC(pImage->handleDC);
		HBITMAP hOldBMP = (HBITMAP)SelectObject(hBmpDC, hBMP);
		BitBlt(pImage->handleDC, 0, 0, bitmap.bmWidth, bitmap.bmHeight, hBmpDC, 0, 0, SRCCOPY);

		SelectObject(hBmpDC, hOldBMP);
		DeleteDC(hBmpDC);
		DeleteObject(hBMP);

		// The loaded BMP is 24-bit...
		pImage->b24Valid = true;

		// Uncomment this line if you need the 8-bit image as well
		//Reduce8(pImage);
	}

	return true;
}

int IMAGING_API SaveBMP(PIMAGECELL pImage, char* pszBMP, RECT* pRct)
{
	// Save an image to disk...very useful for debugging! To use:
	// * SaveBMP(pImage, "C:\\Filename.bmp", NULL);		// C++
	// * SaveBMP(pImage, 'C:\Filename.bmp', nil);		// Delphi

	// To use with a timestamp (C++):
	/*SYSTEMTIME time;
	GetSystemTime(&time);
	TCHAR atcTmp[100];
	sprintf(atcTmp, "C:\\Image_%02d-%02d-%02d.bmp", time.wHour, time.wMinute, time.wSecond);
	SaveBMP(pImage, atcTmp, NULL);*/
	if (pImage)
	{
		BITMAPFILEHEADER bmfh;
		BITMAPINFOHEADER bmih;
		HANDLE hFile;
		DWORD dwTemp;

		// Make sure the image is 24-bit
		if (!pImage->b24Valid)
			Expand24(pImage);

		if (!pRct)
		{
			// No ROI specified so use the whole image
			RECT rctAll;
			pRct = &rctAll;

			pRct->left = 0;
			pRct->top = 0;
			pRct->right = (pImage->nWidth - 1);
			pRct->bottom = (pImage->nHeight - 1);
		}

		int nHeight = ((pRct->bottom - pRct->top) + 1);
		int nWidth = ((pRct->right - pRct->left) + 1);
		int nWidthBytes = WIDTHBYTES(nWidth * 24);

		ZeroMemory(&bmih, sizeof(BITMAPINFOHEADER));
		bmih.biSize = sizeof(BITMAPINFOHEADER);
		bmih.biWidth = nWidth;
		bmih.biHeight = nHeight;
		bmih.biPlanes = 1;
		bmih.biBitCount = 24;
		bmih.biCompression = BI_RGB,
		bmih.biSizeImage = (nWidthBytes * bmih.biHeight);
		bmih.biXPelsPerMeter = 0;
		bmih.biYPelsPerMeter = 0;
		bmih.biClrUsed = 0;
		bmih.biClrImportant = 0;

		hFile = CreateFile(pszBMP, GENERIC_WRITE, 0, NULL,
			CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
		if (hFile == INVALID_HANDLE_VALUE)
			return false;

		bmfh.bfType = 0x4d42;		// 0x42 = "B" 0x4d = "M"

		// Compute the size of the entire file
		bmfh.bfSize = (DWORD)(sizeof(BITMAPFILEHEADER) + bmih.biSize + bmih.biSizeImage);
		bmfh.bfReserved1 = 0;
		bmfh.bfReserved2 = 0;

		// Compute the offset to the array of color indices
		bmfh.bfOffBits = (DWORD)(sizeof(BITMAPFILEHEADER) + bmih.biSize);

		// Copy the BITMAPFILEHEADER into the BMP file
		if (!WriteFile(hFile, &bmfh, sizeof(BITMAPFILEHEADER), &dwTemp, NULL))
		{
			CloseHandle(hFile);
			return false;
		}

		// Copy the BITMAPINFOHEADER and RGBQUAD array into the file
		if (!WriteFile(hFile, &bmih, sizeof(BITMAPINFOHEADER), &dwTemp, NULL))
		{
			CloseHandle(hFile);
			return false;
		}

		if (nHeight < 0)
		{
			// Copy the array of colour indices into the BMP file
			if (!WriteFile(hFile, pImage->pBitmapBits, (int)bmih.biSizeImage, &dwTemp, NULL))
			{
				CloseHandle(hFile);
				return false;
			}
		}
		else
		{
			// Ooh! Top down isn't really supported by shell, so flip bits to make bottom up...
			nHeight = abs(nHeight);		// Make height positive

			// Starting at last line...
			PBYTE pBottomUp = pImage->pBitmapBits + (pRct->bottom * pImage->dwByteWidth);
			pBottomUp += (pRct->left * 3);

			// Write out each line...
			while (nHeight--)
			{
				WriteFile(hFile, pBottomUp, nWidthBytes, &dwTemp, NULL);

				// Stepping down (up?) the image
				pBottomUp -= pImage->dwByteWidth;
			}
		}

		// Close the BMP file
		CloseHandle(hFile);
	}
	else
		return false;	// pImage is NULL

	return true;
}
// End: Utility functions for loading and saving 24-bit bitmaps

// Start: Functions not exported
void Reduce8(PIMAGECELL pImage)
{
	// Convert a 24-bit image into the 8-bit equivalent.

	// The conversion of a colour image to a B&W image is quite complex. The equation is defined
	// by the International Telecommunications Union (ITU) that models the way light interacts with
	// the human eye. The human eye is much more sensitive to GREEN than RED and especially BLUE
	// and this is reflected in the equations.

	// Two equations are defined. The first an older recommendation, the second a more
	// mathematically "correct" version. I've chosen the former. The simple method of just taking
	// the "R" (red) component is fine for images actually created by the X4 but will not be
	// optimal for images from alternate ("real") sources.

	// (1) B&W = 0.299Red + 0.587Green + 0.114Blue
	// (2) B&W = 0.222Red + 0.707Green + 0.071Blue

	// If the image is genuinely B&W (ie. R=G=B), then just use the red component.
	if (pImage && pImage->b24Valid)
	{
		PBYTE pSourceBits = pImage->pBitmapBits;
		PBYTE pDestBits = pImage->pBits;
		if (pSourceBits)
		{
			// Define the gap between the end of the line and start of the next (which should be
			// equal to 0).
			DWORD dwNextLineOffset = (pImage->dwByteWidth - (pImage->nWidth*3));

			int nX, nY;
			BYTE byR, byG, byB;
			for (nY=0; nY<pImage->nHeight; nY++)
			{
				for (nX=0; nX<pImage->nWidth; nX++)
				{
					byR = *pSourceBits++;
					byG = *pSourceBits++;
					byB = *pSourceBits++;
					if ((byR == byB) && (byR == byG))
						*pDestBits++ = byR;
					else
						*pDestBits++ = static_cast<BYTE>(
							m_cacheImageCell.afReduce8_R[byR] +
							m_cacheImageCell.afReduce8_G[byG] +
							m_cacheImageCell.afReduce8_B[byB] + 0.5f);

					// Previous code using just the "R" component. Does not produce good B&W images
					// when the source is a real colour image.
					// *pDestBits++ = *pSourceBits;
					// pSourceBits += 3;
				}

				pSourceBits += dwNextLineOffset;
			}
		}
	}
}

void CacheImageYpos(int nWidth, int nHeight)
{
	// Cache the start of each line
	if ((m_cacheImageCell.nYposCacheWidth != nWidth) ||
		(m_cacheImageCell.nYposCacheHeight != nHeight))
	{
		m_cacheImageCell.nYposCacheWidth = nWidth;
		m_cacheImageCell.nYposCacheHeight = nHeight;
		for (int nY=0; nY<nHeight; nY++)
			m_cacheImageCell.anYpos[nY] = (nY*nWidth);
	}
}
void CreateImageCellBitmap(PIMAGECELL pImage, HDC hSourceDC)
{
	// Clear the bitmap used in the 24-bit image
	// Note: Called internally only so pointers are guaranteed to be valid
	if (pImage->handleBitmap)
	{
		SelectObject(pImage->handleDC, NULL);
		DeleteObject(pImage->handleBitmap);
		DeleteDC(pImage->handleDC);
		pImage->handleBitmap = NULL;
	}

	// Create a new bitmap
	ZeroMemory(&pImage->bmi, sizeof(BITMAPINFO));
	pImage->bmi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
	pImage->bmi.bmiHeader.biWidth = pImage->nWidth;
	pImage->bmi.bmiHeader.biHeight = -pImage->nHeight;
	pImage->bmi.bmiHeader.biPlanes = 1;
	pImage->bmi.bmiHeader.biBitCount = 24;
	pImage->bmi.bmiHeader.biCompression = BI_RGB,
	pImage->bmi.bmiHeader.biSizeImage = (pImage->nWidth * pImage->nHeight * 3);
	pImage->bmi.bmiHeader.biXPelsPerMeter = 0;
	pImage->bmi.bmiHeader.biYPelsPerMeter = 0;
	pImage->bmi.bmiHeader.biClrUsed = 0;
	pImage->bmi.bmiHeader.biClrImportant = 0;

	pImage->handleBitmap = CreateDIBSection(hSourceDC, &pImage->bmi, DIB_RGB_COLORS,
		(void**)&pImage->pBitmapBits, NULL, 0);
	if (pImage->handleBitmap)
	{
		pImage->handleDC = CreateCompatibleDC(hSourceDC);
		(void) SelectObject(pImage->handleDC, pImage->handleBitmap);
	}
	else
		pImage->handleBitmap = NULL;

	pImage->b24Valid = false;
}

void CopyEdgesImageCell(PIMAGECELL pDest, PIMAGECELL pSource)
{
	// Copy the edge pixels of the image. This is useful when using 3x3 convolution filters (such
	// as the median filter) where the edge pixels are not explicitly processed.
	// Note: Called internally only so pointers are guaranteed to be valid
	int nX, nY;

	// Top edge
	PBYTE pDestBits = pDest->pBits;
	PBYTE pSourceBits = pSource->pBits;
	for (nX=0; nX<pSource->nWidth; nX++)
		*pDestBits++ = *pSourceBits++;

	// Left edge
	pDestBits = &pDest->pBits[pSource->nWidth];
	pSourceBits = &pSource->pBits[pSource->nWidth];
	for (nY=0; nY<(pSource->nHeight-2); nY++, pDestBits += pSource->nWidth, pSourceBits += pSource->nWidth)
		*pDestBits = *pSourceBits;

	// Right edge
	pDestBits = &pDest->pBits[2*pSource->nWidth - 1];
	pSourceBits = &pSource->pBits[2*pSource->nWidth - 1];
	for (nY=0; nY<(pSource->nHeight-2); nY++, pDestBits += pSource->nWidth, pSourceBits += pSource->nWidth)
		*pDestBits = *pSourceBits;

	// Bottom edge
	pDestBits = &pDest->pBits[(pSource->nHeight-1)*pSource->nWidth];
	pSourceBits = &pSource->pBits[(pSource->nHeight-1)*pSource->nWidth];
	for (nX=0; nX<pSource->nWidth; nX++)
		*pDestBits++ = *pSourceBits++;
}

bool WithinImageCell(PIMAGECELL pImage, int nX, int nY)
{
	// Find out if the given (x,y) co-ordinate is within the image
	bool bWithin = false;
	if (pImage)
	{
		if ((nX >= 0) && (nX < pImage->nWidth) && (nY >= 0) && (nY < pImage->nHeight))
			bWithin = true;
	}

	return bWithin;
}

float GetBoxAngle(PIMAGECELL pImage)
{
	// Quick method applicable to an object with straight edges (like a box). Drops two vertical
	// lines to find left and right top edges:

	//      L         R
	//	____|____|____|____
	// |    |         |    |
	// |    |              |
	// |    |              |
	// |                   |
	// |                   |
	// |___________________|
	// Note: The image must be binary (ie. BLACK and WHITE)
	float fAngle = 0.0f;
	if (pImage)
	{
		// Find left hand top row (1/4 of the way in from left hand edge)
		int xLeft, yLeft;
		xLeft = (pImage->nWidth / 4);
		PBYTE pImageBits = &pImage->pBits[xLeft];
		for (yLeft=0; yLeft<pImage->nHeight; yLeft++, pImageBits += pImage->nWidth)
		{
			if (*pImageBits == byBLACK)
				break;		// Found top pixel on left side...
		}

		// Same deal for the right hand edge
		int xRight, yRight;
		xRight = (pImage->nWidth - xLeft);
		pImageBits = &pImage->pBits[xRight];
		for (yRight=0; yRight<pImage->nHeight; yRight++, pImageBits += pImage->nWidth)
		{
			if (*pImageBits == byBLACK)
				break;		// Found top pixel on right side...
		}

		// Get the angle between the top left and top right edge
		float fX = (float)(xRight - xLeft);
		float fY = (float)(yRight - yLeft);
		if (fX == 0.0f)
		{
			if (fY > 0.0f)
				fAngle = 90.0f;
			else
				fAngle = -90.0f;
		}
		else if (fY == 0.0f)
		{
			if (fX > 0.0f)
				fAngle = 0.0f;
			else
				fAngle = 180.0f;
		}
		else
			fAngle = (atanf(fY / fX) * f180_over_PI);
	}

	return fAngle;
}
// End: Functions not exported
