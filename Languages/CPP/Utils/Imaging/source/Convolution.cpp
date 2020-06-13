#include "Convolution.h"
#include "Utilities.h"
#include <math.h>

// Convolution cache
typedef struct
{
	// General purpose temporary image
	PIMAGECELL pImgTmp;
} Convolution_Cache;
Convolution_Cache m_cacheConvolution;

// Initialisation
void IMAGING_API InitialiseConvolution()
{
	// Initialise the Convolution cache

	// General purpose temporary image
	m_cacheConvolution.pImgTmp = CreateImageCell(1, 1, FALSE, 0);
}

void IMAGING_API FreeConvolution()
{
	// Free memory
	DeleteImageCell(m_cacheConvolution.pImgTmp);
}

// Start: Exported functions
void IMAGING_API DilateImageCell(PIMAGECELL pDest, PIMAGECELL pSource)
{
	// Identify blobbed pixels in a binary image and isotropically grow in all directions. This is
	// equivalent to convolving (applying) the 3x3 filter:
	// 1  1  1
	// 1  S  1		=> if S=BLACK then all neighbours become BLACK, else leave neighbour alone
	// 1  1  1
	// where "S" is the source pixel in the binary image. We ignore edge pixels to simplify the
	// algorithm.
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Start with a blank destination image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Note: Finding the bounding rectangle first actually costs time for all image types. This
		// is because only one check on pSource->pBits is required for each (x,y) position, but you
		// are paying the (significant) cost of finding the bounding rectangle itself.

		// Note that edge pixels are ignored
		int nYminus1pos = (-1 * nWidth);
		int nYpos = 0;
		int nYplus1pos = (nWidth);
		for (int nY=1; nY<(nHeight-1); nY++)
		{
			nYminus1pos += nWidth;
			nYpos += nWidth;
			nYplus1pos += nWidth;
			for (int nX=1; nX<(nWidth-1); nX++)
			{
				if (pSource->pBits[nYpos + nX] == byBLACK)
				{
					// Grow the cell in all directions
					pDest->pBits[nYminus1pos + (nX-1)] = byBLACK;		// Up-left
					pDest->pBits[nYminus1pos + nX] = byBLACK;			// Up
					pDest->pBits[nYminus1pos + (nX+1)] = byBLACK;		// Up-right
					pDest->pBits[nYpos + (nX+1)] = byBLACK;				// Right
					pDest->pBits[nYplus1pos + (nX+1)] = byBLACK;		// Down-right
					pDest->pBits[nYplus1pos + nX] = byBLACK;			// Down
					pDest->pBits[nYplus1pos + (nX-1)] = byBLACK;		// Down-left
					pDest->pBits[nYpos + (nX-1)] = byBLACK;				// Left
					pDest->pBits[nYpos + nX] = byBLACK;					// Centre
				}
			}
		}
	}
}

void IMAGING_API ErodeImageCell(PIMAGECELL pDest, PIMAGECELL pSource)
{
	// Identify blobbed pixels in a binary image and isotropically erode in all directions. This
	// is equivalent to convolving (applying) the 3x3 filter:
	// N1 N2 N3
	// N8 S  N4		=> S=BLACK if all neighbours Nx are BLACK, else S=WHITE
	// N7 N6 N5
	// where "S" is the source pixel in the binary image. We ignore edge pixels to simplify the
	// algorithm.

	// Note that erosion is the reverse of dilation (see "DilateImageCell"), but in both cases
	// given below NewImage is NOT exactly the same as SourceImage:
	//		* NewImage = Dilate(Erode(SourceImage));	-> the "closing" morphological operator
	//		* NewImage = Erode(Dilate(SourceImage));	-> the "opening" morphological operator
	// The reason is that dilating an image fills in cracks and other small gaps that cannot be
	// recovered by erosion. Similarly, eroding an image removes fine structures that cannot be
	// recovered by dilating.
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Start with a blank destination image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Note: Finding the bounding rectangle first can save time if there are significant
		// numbers of BLACK pixels in the source image. This is because for each (x,y) position,
		// up to 8 checks are required on pSource->pBits. However, for images where there are few
		// (or no) BLACK pixels, finding the bounding rectangle costs time because you still need
		// to scanning the entire image and erosions (if any) will typically require only a single
		// check of pSource->pBits due to compiler optimisations.

		// Consequently, pre-calculating the bounding rectangle is not effective.

		// Note that edge pixels are ignored
		int nYminus1pos = (-1 * nWidth);
		int nYpos = 0;
		int nYplus1pos = (nWidth);
		for (int nY=1; nY<(nHeight-1); nY++)
		{
			nYminus1pos += nWidth;
			nYpos += nWidth;
			nYplus1pos += nWidth;
			for (int nX=1; nX<(nWidth-1); nX++)
			{
				// Erode the cell in all directions
				if ((pSource->pBits[nYminus1pos + (nX-1)] == byBLACK) &&	// Up-left
					(pSource->pBits[nYminus1pos + nX] == byBLACK) &&		// Up
					(pSource->pBits[nYminus1pos + (nX+1)] == byBLACK) &&	// Up-right
					(pSource->pBits[nYpos + (nX+1)] == byBLACK) &&			// Right
					(pSource->pBits[nYplus1pos + (nX+1)] == byBLACK) &&		// Down-right
					(pSource->pBits[nYplus1pos + nX] == byBLACK) &&			// Down
					(pSource->pBits[nYplus1pos + (nX-1)] == byBLACK) &&		// Down-left
					(pSource->pBits[nYpos + (nX-1)] == byBLACK) &&			// Left
					(pSource->pBits[nYpos + nX] == byBLACK))				// Centre
				{
					// All surrounding pixels are BLACK, so this pixel should be too!
					pDest->pBits[nYpos + nX] = byBLACK;
				}
			}
		}
	}
}


void IMAGING_API GrowImageCell(PIMAGECELL pDest, int nGrow)
{
	// Dilate (or erode) to a temporary image and then back to the main result image in double
	// steps (to save processing). Finally, perform the final dilation (or erosion).
	int nIteration = nGrow;
	if (nGrow > 0)
	{
		// Dilate (grow) image
		while (nIteration > 1)
		{
			DilateImageCell(m_cacheConvolution.pImgTmp, pDest);
			DilateImageCell(pDest, m_cacheConvolution.pImgTmp);
			nIteration -= 2;
		}

		// Perform the final dilation if not complete. This final step is only required if "nGrow"
		// is odd.
		if (nIteration > 0)
		{
			DilateImageCell(m_cacheConvolution.pImgTmp, pDest);
			ExtractImageCell(pDest, m_cacheConvolution.pImgTmp, NULL);
		}
	}
	else if (nGrow < 0)
	{
		// Erode (shrink) image
		while (nIteration < -1)
		{
			ErodeImageCell(m_cacheConvolution.pImgTmp, pDest);
			ErodeImageCell(pDest, m_cacheConvolution.pImgTmp);
			nIteration += 2;
		}

		// Perform the final erosion
		if (nIteration < 0)
		{
			ErodeImageCell(m_cacheConvolution.pImgTmp, pDest);
			ExtractImageCell(pDest, m_cacheConvolution.pImgTmp, NULL);
		}
	}
}

void IMAGING_API CloseImageCell(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byIterations)
{
	// Performs the morphological operation "Dilate(Erode(SourceImage))". This has the effect of
	// removing fine structure (outgrowths) in the image.
	// Note: For multiple iterations: Image is first eroded N times, then dilated N times
	if (byIterations > 0)
	{
		// Erode image
		PIMAGECELL pTmp = NULL;
		PIMAGECELL pTmpSource = pSource;
		PIMAGECELL pTmpDest = pDest;
		BYTE byIterationsTmp = byIterations;
		while (byIterationsTmp > 0)
		{
			ErodeImageCell(pTmpDest, pTmpSource);
			pTmp = pTmpDest;
			pTmpDest = pTmpSource;
			pTmpSource = pTmp;
			byIterationsTmp--;
		}

		// Dilate image
		byIterationsTmp = byIterations;
		while (byIterationsTmp > 0)
		{
			DilateImageCell(pTmpDest, pTmpSource);
			pTmp = pTmpDest;
			pTmpDest = pTmpSource;
			pTmpSource = pTmp;
			byIterationsTmp--;
		}
	}
	else
	{
		// No iterations, just copy the image over
		// Note: There is not much point in calling this method with zero iterations!
		ExtractImageCell(pDest, pSource, NULL);
	}
}

void IMAGING_API OpenImageCell(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byIterations)
{
	// Performs the morphological operation "Erode(Dilate(Dilate))". This has the effect of
	// filling in (small) holes in the image.
	// Note: For multiple iterations: Image is first dilated N times, then eroded N times
	if (byIterations > 0)
	{
		// Dilate image
		PIMAGECELL pTmp = NULL;
		PIMAGECELL pTmpSource = pSource;
		PIMAGECELL pTmpDest = pDest;
		BYTE byIterationsTmp = byIterations;
		while (byIterationsTmp > 0)
		{
			DilateImageCell(pTmpDest, pTmpSource);
			pTmp = pTmpDest;
			pTmpDest = pTmpSource;
			pTmpSource = pTmp;
			byIterationsTmp--;
		}

		// Erode image
		byIterationsTmp = byIterations;
		while (byIterationsTmp > 0)
		{
			ErodeImageCell(pTmpDest, pTmpSource);
			pTmp = pTmpDest;
			pTmpDest = pTmpSource;
			pTmpSource = pTmp;
			byIterationsTmp--;
		}
	}
	else
	{
		// No iterations, just copy the image over
		// Note: There is not much point in calling this method with zero iterations!
		ExtractImageCell(pDest, pSource, NULL);
	}
}

void IMAGING_API SobelFilter(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byGain)
{
	// Apply the discrete differentiation Sobel operator to the image. Computes an approximation
	// of the 1st order gradient of the image intensity function. Approximately isotropic with
	// zero impulse (ie. in areas where the image is homogenous, response is low).

	// The 3x3 Sobel filter is (Sx = gradient in x-direction; Sy in y-direction):
	//		1  0 -1					 1  2  1
	// Sx = 2  0 -2		and		Sy = 0  0  0
	//		1  0 -1					-1 -2 -1

	// The magnitude of the gradient is given by:
	// Mag = SQRT(Sx^2 + Sy^2)

	// But this is computationally expensive, so can be approximated by:
	// Mag ~ |Sx| + |Sy|

	// An alternative 5x5 Sobel filter mentioned in the literature:
	//		1   2   0  -2  -1					 1   4   6   4   1
	//		4   8   0  -8  -4					 2   8  12   8   2
	// Sx = 6  12   0 -12  -6		and		Sy = 0   0   0   0   0
	//		4   8   0  -8  -4					-2  -8 -12  -8  -2
	//		1   2   0  -2  -1					-1  -4  -6  -4  -1
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Prepare output image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Pointers to image pixels
		PBYTE pDestBits;
		PBYTE pTopL, pMidL, pBotL;

		// Gradient and magnitude
		int nSx, nSy;
		float fMag;

		// Convolve Sobel filter with image
		float fGain = ((float)byGain / 100.0f);
		int nX, nY;
		for (nY=0; nY<(nHeight-2); nY++)
		{
			// Set up pointers to pixels
			pDestBits = &pDest->pBits[(nY+1)*nWidth + 1];

			pTopL = &pSource->pBits[nY*nWidth];
			pMidL = (pTopL + nWidth);
			pBotL = (pMidL + nWidth);

			// Scan along line (ignoring first and last pixels)
			for (nX=1; nX<(nWidth-1); nX++)
			{
				// Calulate gradients (integer) and magnitude (float)
				nSx = (
					(*pTopL + 2*(*pMidL) + *pBotL) -
					(*(pTopL + 2) + 2*(*(pMidL + 2)) + *(pBotL + 2)));
				nSy = (
					(*pTopL + 2*(*(pTopL + 1)) + *(pTopL + 2)) -
					(*pBotL + 2*(*(pBotL + 1)) + *(pBotL + 2)));
				fMag = ((float)(abs(nSx) + abs(nSy))) * fGain;

				// To use the "correct" magnitude:
				//fMag = sqrt(nSx*nSx + nSy*nSy) * fGain;

				// Write data into image (inverted, in range 0-255)
				if (fMag < 0.0f)
					*pDestBits++ = byWHITE;
				else if (fMag > 255.0f)
					*pDestBits++ = byBLACK;
				else
					*pDestBits++ = static_cast<BYTE>(255.0f - fMag + 0.5);

				// Update all pointers
				pTopL++;
				pMidL++;
				pBotL++;
			}
		}
	}
}

void IMAGING_API ConditionalSobel(PIMAGECELL pDest, PIMAGECELL pSource,
							BYTE byGain, BYTE byLower, BYTE byUpper)
{
	// Apply a conditional Sobel filter. See "SobelFilter" for further details.
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Prepare output image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Pointers to image pixels (L=Left; M=Middle; R=Right)
		PBYTE pDestBits;
		PBYTE pTopL, pMidL, pBotL;

		BYTE byTopL, byTopM, byTopR;
		BYTE byMidL, byMidM, byMidR;
		BYTE byBotL, byBotM, byBotR;

		// Gradient and magnitude
		int nSx, nSy;
		float fMag;

		// Convolve Sobel filter with image
		float fGain = ((float)byGain / 100.0f);
		int nX, nY;
		for (nY=0; nY<(nHeight-2); nY++)
		{
			// Set up pointers to pixels
			pDestBits = &pDest->pBits[(nY+1)*nWidth + 1];

			pTopL = &pSource->pBits[nY*nWidth];
			pMidL = (pTopL + nWidth);
			pBotL = (pMidL + nWidth);

			// Scan along line (ignoring first and last pixels)
			for (nX=1; nX<(nWidth-1); nX++)
			{
				// Ignore pixels which have a greyscale larger than the given threshold range
				byMidM = *(pMidL + 1);
				if ((byMidM >= byLower) && (byMidM <= byUpper))
				{
					byTopL = *pTopL;
					byTopM = *(pTopL + 1);
					byTopR = *(pTopL + 2);

					byMidL = *pMidL;
					byMidR = *(pMidL + 2);

					byBotL = *pBotL;
					byBotM = *(pBotL + 1);
					byBotR = *(pBotL + 2);

					// To limit the pixels around the central pixel (of the 3x3 filter), uncomment
					// this code:
					/*if (byTopL < byLower)
						byTopL = byMidM;
					if (byTopL > byUpper)
						byTopL = byMidM;

					if (byTopM < byLower)
						byTopM = byMidM;
					if (byTopM > byUpper)
						byTopM = byMidM;

					if (byTopR < byLower)
						byTopR = byMidM;
					if (byTopR > byUpper)
						byTopR = byMidM;

					if (byMidL < byLower)
						byMidL = byMidM;
					if (byMidL > byUpper)
						byMidL = byMidM;

					if (byMidR < byLower)
						byMidR = byMidM;
					if (byMidR > byUpper)
						byMidR = byMidM;

					if (byBotL < byLower)
						byBotL = byMidM;
					if (byBotL > byUpper)
						byBotL = byMidM;

					if (byBotM < byLower)
						byBotM = byMidM;
					if (byBotM > byUpper)
						byBotM = byMidM;

					if (byBotR < byLower)
						byBotR = byMidM;
					if (byBotR > byUpper)
						byBotR = byMidM;*/

					// Calulate gradients (integer) and magnitude (float)
					nSx = ((byTopL + 2*byMidL + byBotL) - (byTopR + 2*byMidR + byBotR));
					nSy = ((byTopL + 2*byTopM + byTopR) - (byBotL + 2*byBotM + byBotR));
					fMag = ((float)(abs(nSx) + abs(nSy)) * fGain);

					// Write data into image (inverted, in range 0-255)
					if (fMag < 0.0f)
						*pDestBits++ = byWHITE;
					else if (fMag > 255.0f)
						*pDestBits++ = byBLACK;
					else
						*pDestBits++ = static_cast<BYTE>(255.0f - fMag + 0.5);
				}
				else
					pDestBits++;

				// Update all pointers
				pTopL++;
				pMidL++;
				pBotL++;
			}
		}
	}
}

void IMAGING_API LaplaceFilter(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byGain)
{
	// Apply the discrete differentiation Laplance operator to the image. Computes an approximation
	// of the 2nd order derivative of the image intensity function. Approximately isotropic with
	// zero impulse (ie. in areas where the image is homogenous, response is low).

	// The 3x3 Laplacian filter is:
	// 1  1  1
	// 1 -8  1
	// 1  1  1

	// Some alternatives mentioned in the literature:
	//				0 -1  0					1 -2  1						-2  1 -2
	// 3x3 Alt1 =  -1  4 -1		3x3 Alt2 = -2  4 -2			3x3 Alt3 =   1  4  1
	//				0 -1  0					1 -2  1						-2  1 -2

	//			0  0 -1  0  0
	//			0 -1 -2 -1  0
	// 5x5 =   -1 -2 16 -2 -1
	//			0 -1 -2 -1  0
	//			0  0 -1  0  0
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Prepare output image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Pointers to image pixels (L=Left; M=Middle; R=Right)
		PBYTE pDestBits;
		PBYTE pTopL, pMidL, pBotL;

		// Gradient
		float fGradient;

		// Convolve Laplace filter with image
		float fGain = ((float)byGain / 100.0f);
		int nX, nY;
		for (nY=0; nY<(nHeight-2); nY++)
		{
			// Set up pointers to pixels
			pDestBits = &pDest->pBits[(nY+1)*nWidth + 1];

			pTopL = &pSource->pBits[nY*nWidth];
			pMidL = (pTopL + nWidth);
			pBotL = (pMidL + nWidth);

			// Scan along line (ignoring first and last pixels)
			for (nX=1; nX<(nWidth-1); nX++)
			{
				// Calulate gradients (integer) and magnitude (float)
				fGradient = (fabsf((float)
					(	*pTopL		+ *(pTopL + 1)		+ *(pTopL + 2) +
						*pMidL		- 8*(*(pMidL + 1))	+ *(pMidL + 2) +
						*pBotL		+ *(pBotL + 1)		+ *(pBotL + 2)))) * fGain;

				// Write data into image (inverted, in range 0-255)
				if (fGradient > 255.0f)
					*pDestBits++ = byBLACK;
				else
					*pDestBits++ = static_cast<BYTE>(255.0f - fGradient + 0.5f);

				// Update all pointers
				pTopL++;
				pMidL++;
				pBotL++;
			}
		}
	}
}

void IMAGING_API ConditionalLaplace(PIMAGECELL pDest, PIMAGECELL pSource,
								BYTE byGain, BYTE byLower, BYTE byUpper)
{
	// Apply a conditional Laplace filter. See "LaplaceFilter" for further details.
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Prepare output image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);

		// Pointers to image pixels (L=Left; M=Middle; R=Right)
		PBYTE pDestBits;
		PBYTE pTopL, pMidL, pBotL;

		BYTE byTopL, byTopM, byTopR;
		BYTE byMidL, byMidM, byMidR;
		BYTE byBotL, byBotM, byBotR;

		// Gradient
		float fGradient;

		// Convolve conditional Laplace filter with image
		float fGain = ((float)byGain / 100.0f);
		int nX, nY;
		for (nY=0; nY<(nHeight-2); nY++)
		{
			// Set up pointers to pixels
			pDestBits = &pDest->pBits[(nY+1)*nWidth + 1];

			pTopL = &pSource->pBits[nY*nWidth];
			pMidL = (pTopL + nWidth);
			pBotL = (pMidL + nWidth);

			// Scan along line (ignoring first and last pixels)
			for (nX=1; nX<(nWidth-1); nX++)
			{
				// Ignore pixels which have a greyscale larger than the given threshold range
				byMidM = *(pMidL + 1);
				if ((byMidM >= byLower) && (byMidM <= byUpper))
				{
					byTopL = *pTopL;
					byTopM = *(pTopL + 1);
					byTopR = *(pTopL + 2);

					byMidL = *pMidL;
					byMidR = *(pMidL + 2);

					byBotL = *pBotL;
					byBotM = *(pBotL + 1);
					byBotR = *(pBotL + 2);

					// To limit the pixels around the central pixel (of the 3x3 filter), uncomment
					// this code:
					/*if (byTopL < byLower)
						byTopL = byMidM;
					if (byTopL > byUpper)
						byTopL = byMidM;

					if (byTopM < byLower)
						byTopM = byMidM;
					if (byTopM > byUpper)
						byTopM = byMidM;

					if (byTopR < byLower)
						byTopR = byMidM;
					if (byTopR > byUpper)
						byTopR = byMidM;

					if (byMidL < byLower)
						byMidL = byMidM;
					if (byMidL > byUpper)
						byMidL = byMidM;

					if (byMidR < byLower)
						byMidR = byMidM;
					if (byMidR > byUpper)
						byMidR = byMidM;

					if (byBotL < byLower)
						byBotL = byMidM;
					if (byBotL > byUpper)
						byBotL = byMidM;

					if (byBotM < byLower)
						byBotM = byMidM;
					if (byBotM > byUpper)
						byBotM = byMidM;

					if (byBotR < byLower)
						byBotR = byMidM;
					if (byBotR > byUpper)
						byBotR = byMidM;*/

					// Calulate gradients (integer) and magnitude (float)
					fGradient = (fabsf((float)
						(byTopL + byTopM + byTopR + byMidL + byMidR + byBotL + byBotM + byBotR) - (8*byMidM))) * fGain;

					// Write data into image (inverted, in range 0-255)
					if (fGradient > 255.0f)
						*pDestBits++ = byBLACK;
					else
						*pDestBits++ = static_cast<BYTE>(255.0f - fGradient + 0.5f);
				}
				else
					*pDestBits++;

				// Update all pointers
				pTopL++;
				pMidL++;
				pBotL++;
			}
		}
	}
}

void IMAGING_API MedianFilterHistogram(PIMAGECELL pDest, PIMAGECELL pSource)
{
	// Apply the median filter to the image. This replaces each pixel with the median pixel in a
	// 3x3 neighbourhood, centred on the pixel to be replaced. The median filter is meant to
	// reduce noise (ie. smooth the image) while preserving edges.

	// This version uses a histogram-based algorithm which is updated as the window is moved over
	// the image. Most X-ray images have large areas of white pixels, so this algorithm generally
	// outperforms the version using a sorting algorithm (see "MedianFilterSorting").
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Prepare output image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);
		CopyEdgesImageCell(pDest, pSource);

		// Pointers to image pixels (L=Left; M=Middle; R=Right)
		PBYTE pDestBits;
		PBYTE pTopL, pMidL, pBotL;

		// Histogram for the window
		BYTE abyHistogram[256];
		BYTE byPixel, byCountSoFar;

		// Perform the median filter
		int nX, nY;
		for (nY=0; nY<(nHeight-2); nY++)
		{
			// Set up pointers to pixels
			pDestBits = &pDest->pBits[(nY+1)*nWidth + 1];

			pTopL = &pSource->pBits[nY*nWidth];
			pMidL = pTopL + nWidth;
			pBotL = pMidL + nWidth;

			// Build up the histogram
			ZeroMemory(&abyHistogram, 256);
			abyHistogram[*pTopL]++;
			abyHistogram[*(pTopL + 1)]++;

			abyHistogram[*pMidL]++;
			abyHistogram[*(pMidL + 1)]++;

			abyHistogram[*pBotL]++;
			abyHistogram[*(pBotL + 1)]++;

			// Scan along line (ignoring first and last pixels)
			for (nX=1; nX<(nWidth-1); nX++)
			{
				// Complete the histogram
				abyHistogram[*(pTopL + 2)]++;
				abyHistogram[*(pMidL + 2)]++;
				abyHistogram[*(pBotL + 2)]++;

				// Find median pixel in the 3x3 window
				// Note: Count back from 255 (not forward from 0) because X-ray images typically
				// contain many more light than dark pixels. Same result, but more efficient.
				byCountSoFar = abyHistogram[255];
				byPixel = 255;
				while (byCountSoFar < 5)
					byCountSoFar += abyHistogram[--byPixel];

				// Write median pixel into image
				*pDestBits++ = byPixel;

				// Update all pointers (and remove pixels from the histogram that are about to
				// slide out of the window)
				abyHistogram[*pTopL++]--;
				abyHistogram[*pMidL++]--;
				abyHistogram[*pBotL++]--;
			}
		}
	}
}

void IMAGING_API MedianFilterSorting(PIMAGECELL pDest, PIMAGECELL pSource)
{
	// Apply the median filter to the image, using an alternative sorting algorithm (see
	// "MedianFilter" which uses a histogram-based algorithm for further details).

	// This algorithm has much better "worst case" performance compared with the histogram-based
	// version. However, since X-ray images usually have large areas of white pixels, the
	// histogram-based version (starting at 255) generally works faster. If images with low average
	// pixel value are processed, this algorithm may be faster.
	if (pDest && pSource)
	{
		// Get the image size
		int nWidth = pSource->nWidth;
		int nHeight = pSource->nHeight;

		// Prepare output image
		PrepareImageCell(pDest, nWidth, nHeight);
		ClearImageCell(pDest, byWHITE);
		CopyEdgesImageCell(pDest, pSource);

		// Pointers to image pixels (L=Left; M=Middle; R=Right)
		PBYTE pDestBits;
		PBYTE pTopL, pMidL, pBotL;

		// Window
		BYTE abyWindow[9];

		// Perform the median filter
		int nX, nY;
		for (nY=0; nY<(nHeight-2); nY++)
		{
			// Set up pointers to pixels
			pDestBits = &pDest->pBits[(nY+1)*nWidth + 1];

			pTopL = &pSource->pBits[nY*nWidth];
			pMidL = pTopL + nWidth;
			pBotL = pMidL + nWidth;

			// Scan along line (ignoring first and last pixels)
			for (nX=1; nX<(nWidth-1); nX++)
			{
				// Set up window
				abyWindow[0] = *pTopL;
				abyWindow[1] = *(pTopL + 1);
				abyWindow[2] = *(pTopL + 2);

				abyWindow[3] = *pMidL;
				abyWindow[4] = *(pMidL + 1);
				abyWindow[5] = *(pMidL + 2);

				abyWindow[6] = *pBotL;
				abyWindow[7] = *(pBotL + 1);
				abyWindow[8] = *(pBotL + 2);
				InsertionSort(abyWindow);

				// Write median pixel into image
				*pDestBits++ = abyWindow[4];

				// Update all pointers
				pTopL++;
				pMidL++;
				pBotL++;
			}
		}
	}
}
// End: Exported functions

// Start: Functions not exported
// End: Functions not exported
