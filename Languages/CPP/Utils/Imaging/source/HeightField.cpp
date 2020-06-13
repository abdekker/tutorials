#include <windows.h>
#include <stdio.h>
#include <gl\gl.h>

#include "Imaging.h"

static bool bInitialisedGL = false;
static GLuint glTexture;
static PIMAGECELL pHeightImage;

static int nHeightFieldWidth = -1;
static int nHeightFieldHeight = -1;
static int nLastImageWidth = 0;
static int nLastImageHeight = 0;

static GLfloat* pafVertices = NULL;
static GLfloat* pafTextureCoords = NULL;
static DWORD* padwIndices = NULL;

BOOL IMAGING_API InitImageCellHeightmap(POGL_STATE pState, PIMAGECELL pImage, PIMAGECELL pTextureImage)
{
	// Note: I don't know how to draw a background image in OpenGL, so a Loma blue background is used
	if (!CreateGLWindow(pState, 0, 0, 32, 24, FALSE))
		return FALSE;

	// Set the image
	pHeightImage = pImage;

	// Set the background colour (this needs to be done each time the image is updated)
	glClearColor(0.8549f, 0.8941f, 0.9765f, 0.5000f);	// 'Loma' blue = RGB(218,228,249) or 0xF9E4DA

	// Enable depth testing; this needs to be set up each time the image is drawn
	glClearDepth(1.0f);
	glEnable(GL_DEPTH_TEST);

	// Initialise OpenGL?
	if (!bInitialisedGL)
	{
		// Enable Smooth Shading
		glShadeModel(GL_SMOOTH);

		// If you want a wierd effect where you can 'look through' the image, try this
		//glEnable(GL_CULL_FACE);

		// Type of depth testing to do
		glDepthFunc(GL_LEQUAL);

		// Really nice perspective calculations
		glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

		// OpenGL initialised
		bInitialisedGL = true;
	}

	if (pTextureImage)
	{
		// Create the texture
		glGenTextures(1, &glTexture);

		// Typical texture generation using data from the bitmap
		glBindTexture(GL_TEXTURE_2D, glTexture);
		glTexImageCell(pTextureImage);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

		// Enable texture mapping
		glEnable(GL_TEXTURE_2D);
	}
	else
	{
		glTexture = 0;
		glDisable(GL_TEXTURE_2D);
	}

	PrepareArrays();
	return TRUE;		// Initialization went OK
}

BOOL IMAGING_API DeleteImageCellHeightmap(POGL_STATE pState)
{
	if (glTexture)
	{
		glDeleteTextures(1, &glTexture);
		glTexture = 0;
	}

	KillGLWindow(pState);
	return TRUE;
}

BOOL IMAGING_API DrawImageCellHeightmap(POGL_STATE pState,
										float fDistance,
										float fTwist,
										float fElevation,
										float fAzimuth)
{
	if (pState)
	{
		wglMakeCurrent(pState->handleDC, pState->handleRC);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity();

		glTranslatef(0, 0, -fDistance);
		glRotatef(-fTwist, 0.0f, 0.0f, 1.0f);
		glRotatef(-fElevation, 1.0f, 0.0f, 0.0f);
		glRotatef(fAzimuth, 0.0f, 1.0f, 0.0f);

		// Note to developer: Drawing the 3D view is time-consuming (typically several hundred ms).
		// Nearly all of the time is consumed by "DrawArrays", so could be the focus of performance
		// improvements. Displaying the 3D view is, however, not typical.
		DrawArrays();
		SwapBuffers(pState->handleDC);
	}

	return TRUE;
}

void PrepareArrays(void)
{
	// Has the image dimension changed?
	if ((nLastImageWidth != pHeightImage->nWidth) ||
		(nLastImageHeight != pHeightImage->nHeight))
	{
		// Get the dimensions of the image we are drawing
		nHeightFieldWidth = pHeightImage->nWidth;
		nHeightFieldHeight = pHeightImage->nHeight;

		// Save these dimensions
		nLastImageWidth = nHeightFieldWidth;
		nLastImageHeight = nHeightFieldHeight;

		// Delete arrays
		if (pafVertices)
			delete[] pafVertices;

		if (pafTextureCoords)
			delete[] pafTextureCoords;

		if (padwIndices)
			delete[] padwIndices;

		// How many vertices?
		DWORD dwVertices = (nHeightFieldWidth * nHeightFieldHeight);
		pafVertices = new GLfloat[dwVertices * 3];			// 3 floats per vertex (x,y,z)
		pafTextureCoords = new GLfloat[dwVertices * 2];		// 2 floats per text (u,v)

		// Generate OpenGL arrays
		GLfloat fCentreX = ((GLfloat)pHeightImage->nWidth / 2.0f);
		GLfloat fCentreZ = ((GLfloat)pHeightImage->nHeight / 2.0f);

		GLfloat* pfVertex = pafVertices;
		GLfloat* pfTextureCoord = pafTextureCoords;
		int nX, nY;
		for (nY=0; nY<nHeightFieldHeight; nY++)
		{
			for (nX=0; nX<nHeightFieldWidth; nX++, pfVertex+=3, pfTextureCoord+=2)
			{
				// Note: "pfVertex[1]" needs to be re-calculated each time this function is called
				pfVertex[0] = ((GLfloat)nX - fCentreX);
				pfVertex[2] = ((GLfloat)nY - fCentreZ);

				pfTextureCoord[0] = ((GLfloat)nX / (GLfloat)nHeightFieldWidth);
				pfTextureCoord[1] = ((GLfloat)nY / (GLfloat)nHeightFieldHeight);
			}
		}

		// Make an index buffer
		DWORD dwIndices = ((nHeightFieldWidth * 2) * nHeightFieldHeight);
		padwIndices = new DWORD[dwIndices];
		DWORD* pdwIndex = padwIndices;
		for (nY=0; nY<nHeightFieldHeight-1; nY++)
		{
			for (nX=0; nX<nHeightFieldWidth; nX++)
			{
				DWORD dwJump = (nY*nHeightFieldWidth + nX);

				*pdwIndex++ = (dwJump + nHeightFieldWidth);
				*pdwIndex++ = dwJump;
			}
		}
	}

	// Draw the image into pafVertices
	GLfloat* pfVertex = pafVertices;
	GLfloat* pfTextureCoord = pafTextureCoords;
	int nX, nY;
	for (nY=0; nY<nHeightFieldHeight; nY++)
	{
		for (nX=0; nX<nHeightFieldWidth; nX++, pfVertex+=3, pfTextureCoord+=2)
		{
			// Note: "pfVertex[0]" and "...[2]" only need to be calculated when the image size changes
			pfVertex[1] = (255.0f - (GLfloat)pHeightImage->pBits[nY*nHeightFieldWidth + nX]);
		}
	}

	// Send the arrays over...
	glEnableClientState(GL_VERTEX_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);

	glVertexPointer(3, GL_FLOAT, 0, pafVertices);
	glTexCoordPointer(2, GL_FLOAT, 0, pafTextureCoords);
}

void DrawArrays(void)
{
	// Construct the 3D image
	for (int nY=0; nY<nHeightFieldHeight-1; nY++)
		glDrawElements(GL_TRIANGLE_STRIP, nHeightFieldWidth*2,
			GL_UNSIGNED_INT, &padwIndices[nY*(nHeightFieldWidth*2)]);
}
