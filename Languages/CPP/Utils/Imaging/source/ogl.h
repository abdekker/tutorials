/*=============================================================================
File:
  ogl.h

SubSystem:
  Imaging

Notes:
  Drives the OpenGL library for display of the 3D image

© DekkerSoft. All rights reserved.
=============================================================================*/

#ifndef OGL_H
#define OGL_H

typedef struct {
	HDC			handleDC;
	HGLRC		handleRC;
	HINSTANCE	handleInstance;
	HWND		handleWnd;
	BOOL		bFullScreen;
	int			nWidth;
	int			nHeight;
} OGL_STATE, *POGL_STATE;

#ifdef __cplusplus
extern "C" {
#endif		// __cplusplus

BOOL CreateGLWindow(POGL_STATE pState, int nWidth, int nHeight, BYTE byBits, BYTE byZBits, bool bFullScreen);
void KillGLWindow(POGL_STATE pState);
void ResizeGLScene(int nWidth, int nHeight);
BOOL glTexImageCell(PIMAGECELL pImage);

#ifdef __cplusplus
}
#endif		// __cplusplus
#endif		// OGL_H
