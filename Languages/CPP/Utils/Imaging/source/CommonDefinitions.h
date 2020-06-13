#ifndef __COMMONDEFINITIONSH
#define __COMMONDEFINITIONSH

// Black / White colour
#define byBLACK		0
#define byWHITE		255

// Logical operations for combining images
#define opAND		0		// Binary (ie. thresholded) image
#define opOR		1		// Binary image; logical "add"
#define opOR_MASK	2		// Binary image
#define opSUBTRACT	3		// Binary image; logical "subtract"
#define opMIN		10		// Greyscale (ie. 0-255) image
#define opMAX		11		// Greyscale image

// Degrees <--> Radians conversions
#define fPI				3.14159265359f
#define dPI				3.14159265359

#define fPI_over_180	0.0174532925199433f		// (PI / 180.0)
#define dPI_over_180	0.0174532925199433

#define f180_over_PI	57.2957795130823f		// (180.0 / PI)
#define d180_over_PI	57.2957795130823

// D2R = "degrees to radians"
// R2D = "radians to degrees"
#define D2Rf(d)			(d * fPI_over_180)
#define R2Df(r)			(r * f180_over_PI)
#define D2Rd(d)			(d * dPI_over_180)
#define R2Dd(r)			(r * d180_over_PI)

// Minimum floating point number
#define MIN_FLOAT		1e-6f
#define MIN_DOUBLE		1e-9

// Used for perimeter calculations and the Box Remover
#define SQRT2			1.41421356f
#define SQRT2_LESS1		0.41421356f

// Degrees <--> Radians conversions
#define fPI			3.14159265359f
#define dPI			3.14159265359

// Generic quadrilateral (can be used to draw a grid cell, for example)
typedef struct tagQuadrilateral
{
	// The quadrilateral starts in the upper-left (0). Draw lines in the sequence 0>1>2>3>0.
	POINT	aPts[4];
} Quadrilateral, *PQuadrilateral;

// Generic linked list (or container) node for pointers (eg. for image analysis)
typedef struct _tagPOINTERNODE
{
	void*				pData;
	_tagPOINTERNODE*	pNextLink;
	_tagPOINTERNODE*	pPreviousLink;
} _POINTERNODE, *PPOINTERNODE;

// Generic DWORD linked list type (for holes analysis)
typedef struct _tagDWORDLIST
{
	DWORD				dwValue;
	_tagDWORDLIST*		pNextLink;
	_tagDWORDLIST*		pPreviousLink;
} _DWORDLIST, *PDWORDLIST;

// Image
typedef struct tagIMAGECELL
{
	HDC			handleDC;			// Device context for the bitmap
	HBITMAP		handleBitmap;
	BITMAPINFO	bmi;
	BYTE*		pBitmapBits;
	bool		b24Valid;
	BYTE*		pBits;
	int			nWidth, nHeight;
	DWORD		dwByteWidth, dwSize;
} IMAGECELL, *PIMAGECELL;

#endif		// __COMMONDEFINITIONSH
