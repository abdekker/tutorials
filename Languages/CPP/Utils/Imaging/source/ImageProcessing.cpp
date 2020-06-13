#include "Imaging.h"
#include <math.h>

/* Image Processing deals with the process of applying Gamma and Range (%) to the 14-bit data
directly from the detector as it is converted to the 8-bit data used in the actual image.
This process is done by using a lookup table which defines the mapping from each number in
the 0-16383 (14-bit) range to the 8-bit number (0-255) used in the image.

Formula (14-bit to 8-bit):

where	I = image pixel (8-bit)
		N = number on the 14-bit scale
		uR = upper range (%) (usually in range 20-120, and in this formula, divided by 100)
		G = gamma

I = 255 * (N/[255*uR])^(1/G)

This formula must be modified when considering lower range (%). */

// ApplyGammaPercRange: Performs an 8-bit to 8-bit conversion which gives a rough
// approximation of what the full 14-bit to 8-bit conversion would give.
void IMAGING_API ApplyGammaPercRange(PIMAGECELL pDest, PIMAGECELL pSource,
								float fGamma, float fLowerRange, float fUpperRange)
{
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Start off with nothing
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);
		if (!pDest)
			return;

		// Initialise the 8-bit to 8-bit lookup table which maps source pixel values
		// to new output pixel values
		BYTE abyLookupTable[256];
		float fLower = (255.0f*fLowerRange);
		float fUpper = (255.0f*fUpperRange);
		float fFullRange = (fUpper - fLower);

		float fCalc = 0.0f;
		float fData = 0.0f;
		int nSource = 0;

		// Standard conversion
		for (nSource = 0; nSource < 256; nSource++)
		{
			fData = (static_cast<float>(nSource) - fLower);
			if (fData < 0.0f)
				abyLookupTable[nSource] = 0;
			else
			{
				fCalc = (255.0f * powf((fData/fFullRange), (1.0f/fGamma)));
				if (fCalc < 0.0f)
					abyLookupTable[nSource] = 0;
				else if (fCalc > 255.0f)
					abyLookupTable[nSource] = 255;
				else
					abyLookupTable[nSource] = static_cast<BYTE>(fCalc + 0.5f);
			}
		}

		// Point to output image.
		PBYTE pOutputBits = pDest->pBits;
		int nX, nY;
		for (nY=0; nY<nHeight; nY++)
		{
			for (nX=0; nX<nWidth; nX++, pOutputBits++)
			{
				*pOutputBits = abyLookupTable[pSource->pBits[nY*nWidth + nX]];

				// This produces an interesting pattern!
				//*pOutputBits = ((nX*nY) % 256);
			}
		}
	}
}

// ErodeThresholdPixels: Erode pixels between certain threshold levels
void IMAGING_API ErodeThresholdPixels(PIMAGECELL pDest, PIMAGECELL pSource,
									BYTE byLower, BYTE byUpper, BYTE byErosion)
{
	// Note: The percentage erosion must be in the range 0-100%
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Start off with nothing
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);
		if (!pDest)
			return;

		PBYTE pDestBits = pDest->pBits;			// Pointer to output (result) image bits
		PBYTE pSourceBits = pSource->pBits;		// Pointer to source image bits
		DWORD dwNewPixel = 0;

		DWORD dwByteCount = pSource->dwSize;
		while (dwByteCount--)
		{
			if ((*pSourceBits < byLower) || (*pSourceBits > byUpper))
			{
				dwNewPixel = ((byWHITE - *pSourceBits) * byErosion) / 100;
				*pDestBits = (*pSourceBits + static_cast<BYTE>(dwNewPixel));
			}
			else
				*pDestBits = *pSourceBits;
	
			pDestBits++;
			pSourceBits++;
		}
	}
}

void IMAGING_API ErodeScreenedPixels(PIMAGECELL pDest, PIMAGECELL pScreen, BYTE byErosion)
{
	// Erode only pixels defined by a binary screen image of pure Black/White, otherwise use the
	// original destination pixel
	// Note: The percentage erosion must be in the range 0-100%
	if (pDest && pScreen)
	{
		if ((pDest->nWidth != pScreen->nWidth) ||
			(pDest->nHeight != pScreen->nHeight))
			return;

		PBYTE pDestBits = pDest->pBits;			// Pointer to output (result) image
		PBYTE pScreenBits = pScreen->pBits;		// Pointer to screened image
		DWORD dwNewPixel = 0;

		DWORD dwByteCount = pDest->dwSize;
		while (dwByteCount--)
		{
			if (*pScreenBits == byBLACK)
			{
				// Masked pixel! Erode the destination pixel a small amount...
				dwNewPixel = ((byWHITE - *pDestBits) * byErosion) / 100;
				*pDestBits = (*pDestBits + static_cast<BYTE>(dwNewPixel));
			}
	
			pDestBits++;
			pScreenBits++;
		}
	}
}

void IMAGING_API AddImages(PIMAGECELL pResult, PIMAGECELL pAddend1, PIMAGECELL pAddend2)
{
	// Add one image to another
	if (pResult && pAddend1 && pAddend2)
	{
		// Note: All images must be the same size!
		if ((pResult->nWidth != pAddend1->nWidth) ||
			(pResult->nWidth != pAddend2->nWidth))
			return;

		if ((pResult->nHeight != pAddend1->nHeight) ||
			(pResult->nHeight != pAddend2->nHeight))
			return;

		// Get the dimensions of the source image
		int nWidth = pAddend1->nWidth;
		int nHeight = pAddend1->nHeight;

		// Get a pointer to all images
		PBYTE pResultBits = pResult->pBits;
		PBYTE pAddend1Bits = pAddend1->pBits;
		PBYTE pAddend2Bits = pAddend2->pBits;
		int nX, nY;
		for (nY=0; nY<nHeight; nY++)
		{
			for (nX=0; nX<nWidth; nX++, pResultBits++, pAddend1Bits++, pAddend2Bits++)
			{
				*pResultBits = ((*pAddend1Bits) + (*pAddend2Bits))/255;
			}
		}
	}
}

void IMAGING_API SubtractImages(PIMAGECELL pDiff, PIMAGECELL pMinuend, PIMAGECELL pSubtrahend)
{
	// Subtract one image from another
	if (pDiff && pMinuend && pSubtrahend)
	{
		// Note: All images must be the same size!
		if ((pMinuend->nWidth != pSubtrahend->nWidth) || (pMinuend->nHeight != pSubtrahend->nHeight))
			return;

		if ((pDiff->nWidth != pMinuend->nWidth) || (pDiff->nHeight != pMinuend->nHeight))
			PrepareImageCell(pDiff, pMinuend->nWidth, pMinuend->nHeight);

		// Get the dimensions of the source image
		int nWidth = pMinuend->nWidth;
		int nHeight = pMinuend->nHeight;

		// Get a pointer to all images
		PBYTE pDiffBits = pDiff->pBits;
		PBYTE pMinuendBits = pMinuend->pBits;
		PBYTE pSubtrahendBits = pSubtrahend->pBits;
		int nX, nY;
		for (nY=0; nY<nHeight; nY++)
		{
			for (nX=0; nX<nWidth; nX++, pDiffBits++, pMinuendBits++, pSubtrahendBits++)
			{
				if (*pMinuendBits < *pSubtrahendBits)
					*pDiffBits = (255 - (*pSubtrahendBits - *pMinuendBits));
				else
					*pDiffBits = 255;
			}
		}
	}
}
