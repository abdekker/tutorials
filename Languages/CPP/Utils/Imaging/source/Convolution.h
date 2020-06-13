/*=============================================================================
File:
  Convolution.h

SubSystem:
  Imaging

Notes:
  Perform convolution of 3x3 or 5x5 filters with the image

© DekkerSoft. All rights reserved.
=============================================================================*/

#ifndef CONVOLUTION_H
#define CONVOLUTION_H

#include "Imaging.h"

#ifdef __cplusplus
extern "C" {
#endif		// __cplusplus

// Initialisation
void IMAGING_API InitialiseConvolution();
void IMAGING_API FreeConvolution();

// Exported functions
void IMAGING_API DilateImageCell(PIMAGECELL pDest, PIMAGECELL pSource);
void IMAGING_API ErodeImageCell(PIMAGECELL pDest, PIMAGECELL pSource);
void IMAGING_API GrowImageCell(PIMAGECELL pDest, int nGrow);
void IMAGING_API CloseImageCell(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byIterations);
void IMAGING_API OpenImageCell(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byIterations);

void IMAGING_API SobelFilter(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byGain);
void IMAGING_API ConditionalSobel(PIMAGECELL pDest, PIMAGECELL pSource,
					BYTE byGain, BYTE byLower, BYTE byUpper);
void IMAGING_API LaplaceFilter(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byGain);
void IMAGING_API ConditionalLaplace(PIMAGECELL pDest, PIMAGECELL pSource,
					BYTE byGain, BYTE byLower, BYTE byUpper);

void IMAGING_API MedianFilterHistogram(PIMAGECELL pDest, PIMAGECELL pSource);
void IMAGING_API MedianFilterSorting(PIMAGECELL pDest, PIMAGECELL pSource);

// Functions not exported
#ifdef __cplusplus
}
#endif		// __cplusplus
#endif		CONVOLUTION_H
