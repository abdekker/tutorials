#include <windows.h>
#include <gl\gl.h>
#include <gl\glu.h>
#if (_MSC_VER <= 1310)
	#include <gl\glaux.h>
#endif

#include "Imaging.h"

#pragma comment(lib, "opengl32.lib")
#pragma comment(lib, "glu32.lib")

// OpenGL function used for rendering the 3D image in the Loma X4
BOOL CreateGLWindow(POGL_STATE pState, int nWidth, int nHeight, BYTE byBits, BYTE byZBits, bool bFullScreen)
{
	if (pState)
	{
		GLuint PixelFormat;							// Holds the results after searching for a match
		pState->handleInstance = GetModuleHandle(NULL);
		pState->bFullScreen = bFullScreen;			// Set the global fullscreen flag

		// Attempt fullscreen mode?
		if (pState->bFullScreen)
		{
			// Set the device mode
			DEVMODE dmScreenSettings;
			memset(&dmScreenSettings, 0, sizeof(dmScreenSettings));		// Makes sure memory's cleared
			dmScreenSettings.dmSize = sizeof(dmScreenSettings);			// Size of DEVMODE structure
			dmScreenSettings.dmPelsWidth = nWidth;						// Selected screen width
			dmScreenSettings.dmPelsHeight = nHeight;					// Selected screen height
			dmScreenSettings.dmBitsPerPel = byBits;						// Selected bits per pixel
			dmScreenSettings.dmFields = DM_BITSPERPEL | DM_PELSWIDTH | DM_PELSHEIGHT;

			// Try to set selected mode and get results.
			// NOTE: CDS_FULLSCREEN gets rid of the start bar
			if (ChangeDisplaySettings(&dmScreenSettings, CDS_FULLSCREEN) != DISP_CHANGE_SUCCESSFUL)
			{
				// If the mode fails, offer two options: Quit or Use Windowed Mode.
				if (MessageBox(NULL, "Requested Fullscreen Mode is not supported by\nyour video card. Use Windowed mode instead?",
						"NeHe GL", MB_YESNO | MB_ICONEXCLAMATION) == IDYES)
				{
					pState->bFullScreen = FALSE;	// Windowed mode selected => Fullscreen = FALSE
				}
				else
				{
					// Pop up a messagebox letting the user know that the program is closing
					MessageBox(NULL, "Program will now close.", "ERROR", MB_OK | MB_ICONSTOP);
					return FALSE;
				}
			}
		}

		// pfd test Windows how we want things to be
		static PIXELFORMATDESCRIPTOR pfd =
		{
			sizeof(PIXELFORMATDESCRIPTOR),		// Size of this pixel format descriptor
			1,									// Version number
			PFD_DRAW_TO_WINDOW |				// Format must support window
				PFD_SUPPORT_OPENGL |			// Format must support OpenGL
				PFD_DOUBLEBUFFER,				// Must support double buffering
			PFD_TYPE_RGBA,						// Request an RGBA format
			byBits,								// Select our colour depth
			0, 0, 0, 0, 0, 0,					// Colour bits ignored
			0,									// No alpha buffer
			0,									// Shift bit ignored
			0,									// No accumulation buffer
			0, 0, 0, 0,							// Accumulation bits ignored
			byZBits,							// 16Bit Z-Buffer (Depth Buffer)
			0,									// No stencil buffer
			0,									// No auxiliary buffer
			PFD_MAIN_PLANE,						// Main drawing layer
			0,									// Reserved
			0, 0, 0								// Layer masks ignored
		};

		// Did we get a Device Context?
		if (!(pState->handleDC = GetDC(pState->handleWnd)))
		{
			// No, so reset the display
			KillGLWindow(pState);
			//MessageBox(NULL, "Can't create GL device context", "ERROR", MB_OK | MB_ICONEXCLAMATION);
			return FALSE;
		}

		// Did Windows find a matching pixel format?
		if (!(PixelFormat = ChoosePixelFormat(pState->handleDC, &pfd)))
		{
			// No, so reset the display
			KillGLWindow(pState);
			//MessageBox(NULL, "Can't find suitable PixelFormat", "OpenGL Error", MB_OK | MB_ICONEXCLAMATION);
			return FALSE;
		}

		// Are we able to set the pixel format?
		if (!SetPixelFormat(pState->handleDC, PixelFormat, &pfd))
		{
			// No, so reset the display
			KillGLWindow(pState);
			//MessageBox(NULL, "Can't set the PixelFormat", "OpenGL Error", MB_OK | MB_ICONEXCLAMATION);
			return FALSE;
		}

		// Are we able to get a Rendering Context?
		if (!(pState->handleRC = wglCreateContext(pState->handleDC)))
		{
			// No, so reset the display
			KillGLWindow(pState);
			//MessageBox(NULL, "Can't create GL rendering context", "OpenGL Error", MB_OK | MB_ICONEXCLAMATION);
			return FALSE;
		}

		// Try to activate the Rendering Context
		if (!wglMakeCurrent(pState->handleDC, pState->handleRC))
		{
			// No, so reset the display
			KillGLWindow(pState);
			//MessageBox(NULL, "Can't activate GL rendering context", "OpenGL Error", MB_OK | MB_ICONEXCLAMATION);
			return FALSE;
		}

		if (!pState->bFullScreen)
		{
			RECT rct;
			GetClientRect(pState->handleWnd, &rct);
			nWidth = (rct.right - rct.left + 1);
			nHeight = (rct.bottom - rct.top + 1);
		}

		pState->nWidth = nWidth;
		pState->nHeight = nHeight;

		// Set up our perspective GL screen
		ResizeGLScene(nWidth, nHeight);
	}

	return TRUE;
}

// Properly kill the window
void KillGLWindow(POGL_STATE pState)
{
	if (pState)
	{
		// Are we in fullscreen mode?
		if (pState->bFullScreen)
		{
			// If so switch back to the desktop
			ChangeDisplaySettings(NULL, 0);
			ShowCursor(TRUE);
		}

		// Do we have a rendering context?
		if (pState->handleRC)
		{
			// Are we able to release the DC and RC contexts?
			if (!wglMakeCurrent(NULL, NULL))
			{
				//MessageBox(NULL, "Release DC and RC failed", "SHUTDOWN ERROR", MB_OK | MB_ICONINFORMATION);
			}

			// Are we able to delete the RC?
			if (!wglDeleteContext(pState->handleRC))
			{
				//MessageBox(NULL, "Release rendering context failed", "SHUTDOWN ERROR", MB_OK | MB_ICONINFORMATION);
			}

			// Set rendering context handle to NULL
			pState->handleRC = NULL;
		}

		// Are we able to release the DC?
		if (pState->handleDC && !ReleaseDC(pState->handleWnd, pState->handleDC))
		{
			//MessageBox(NULL, "Release device context failed", "SHUTDOWN ERROR", MB_OK | MB_ICONINFORMATION);
		}

		// Set device context handle to NULL
		pState->handleDC = NULL;
	}
}

// Resize and initialize the GL window
void ResizeGLScene(int nWidth, int nHeight)
{
	// Prevent a divide by zero error
	if (nHeight == 0)
		nHeight = 1;

	// Reset the current viewport
	glViewport(0, 0, nWidth, nHeight);

	glMatrixMode(GL_PROJECTION);		// Select the Projection Matrix
	glLoadIdentity();					// Reset the Projection Matrix

	// Calculate the Aspect Ratio of the Window. Farthest distance changed to 500.0f (NEW)
	gluPerspective(45.0f, (GLfloat)nWidth/(GLfloat)nHeight, 0.1f, 5000.0f);

	glMatrixMode(GL_MODELVIEW);			// Select the Modelview Matrix
	glLoadIdentity();					// Reset the Modelview Matrix
}

BOOL glTexImageCell(PIMAGECELL pImage)
{
	// Create the texture for the 3D view
	if (pImage)
	{
		if (!pImage->b24Valid)
			Expand24(pImage);

		PIMAGECELL pTextureImage = CreateImageCell(256, 256, FALSE, 0);
		if (pTextureImage)
		{
			CreateImageCellBitmap(pTextureImage, pImage->handleDC);

			SetStretchBltMode(pTextureImage->handleDC, HALFTONE);
			StretchBlt(pTextureImage->handleDC, 0, 0, 256, 256, pImage->handleDC, 0, 0,
				pImage->nWidth, pImage->nHeight, SRCCOPY);

			glTexImage2D(GL_TEXTURE_2D, 0, 3, 256, 256, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE,
				pTextureImage->pBitmapBits);
			DeleteImageCell(pTextureImage);
			pTextureImage = NULL;
		}
	}

	return FALSE;
}
