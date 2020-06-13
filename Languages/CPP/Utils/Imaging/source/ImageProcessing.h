/*=============================================================================
File:
  ImageProcessing.h

SubSystem:
  Imaging

Notes:
  Performs the Gamma and Range (%) transformations (8-bit to 8-bit only) as well
  as other advanced image processing tasks

© DekkerSoft. All rights reserved.
=============================================================================*/

#ifndef IMAGE_PROCESSING_H
#define IMAGE_PROCESSING_H

#include "Imaging.h"

#ifdef __cplusplus
extern "C" {
#endif		// __cplusplus

// Gamma / Range (%)
void IMAGING_API ApplyGammaPercRange(PIMAGECELL pDest, PIMAGECELL pSource, float fGamma,
					float fLowerRange, float fUpperRange);

// Pixel erosion
void IMAGING_API ErodeThresholdPixels(PIMAGECELL pDest, PIMAGECELL pSource,
					BYTE byLower, BYTE byUpper, BYTE byErosion);
void IMAGING_API ErodeScreenedPixels(PIMAGECELL pDest, PIMAGECELL pSource, BYTE byErosion);

// Image addition / subtraction
void IMAGING_API AddImages(PIMAGECELL pResult, PIMAGECELL pAddend1, PIMAGECELL pAddend2);
void IMAGING_API SubtractImages(PIMAGECELL pDiff, PIMAGECELL pMinuend, PIMAGECELL pSubtrahend);

#ifdef __cplusplus
}
#endif		// __cplusplus
#endif		// IMAGE_PROCESSING_H
