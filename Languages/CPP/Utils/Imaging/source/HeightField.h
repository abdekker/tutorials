/*=============================================================================
File:
  HeightField.h

SubSystem:
  Imaging

Notes:
  Performs the drawing of the OpenGL 3D images

© DekkerSoft. All rights reserved.
=============================================================================*/

#ifndef HEIGHT_FIELD_H
#define HEIGHT_FIELD_H

#ifdef __cplusplus
extern "C" {
#endif		// __cplusplus

// Exported functions
// Note: "InitImageCellHeightmap" used to have a 4th parameter, int nStepSize, but this was always 1 and was removed
BOOL IMAGING_API InitImageCellHeightmap(POGL_STATE pState, PIMAGECELL pImage, PIMAGECELL pTextureImage);
BOOL IMAGING_API DeleteImageCellHeightmap(POGL_STATE pState);
BOOL IMAGING_API DrawImageCellHeightmap(POGL_STATE pState, float fDistance, float fTwist,
				float fElevation, float fAzimuth);

// Private functions
void PrepareArrays(void);
void DrawArrays(void);

#ifdef __cplusplus
}
#endif		// __cplusplus
#endif		// HEIGHT_FIELD_H
