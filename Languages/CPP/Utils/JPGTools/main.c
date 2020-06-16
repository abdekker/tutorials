#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#include "ijl.h"
#include "JPGTools.h"

static HANDLE g_hMod = NULL;

// DllMain; Dll main entry point
BOOL JPG_API DllMain(HANDLE hDLL, DWORD dwReason, LPVOID lpReserved)
{
	g_hMod = hDLL;
	switch (dwReason)
	{
		case DLL_PROCESS_ATTACH:
			break;
		case DLL_PROCESS_DETACH:
			break;
		default:
			break;
	}

	return TRUE;
}

BOOL JPG_API EncodeJpgFileFromDIB(LPCSTR lpszPathName, int nWidth, int nHeight, BYTE* pbyBits, int nQuality)
{
	BOOL bResult = TRUE;
	IJLERR jerr;
	DWORD dib_pad_bytes;

	// Allocate the IJL JPEG_CORE_PROPERTIES structure.
	JPEG_CORE_PROPERTIES jcprops;
	__try
	{
		// Initialize the IntelR JPEG library
		jerr = ijlInit(&jcprops);
		if (IJL_OK != jerr)
		{
			bResult = FALSE;
			__leave;
		}

		dib_pad_bytes = IJL_DIB_PAD_BYTES(nWidth, 3);

		// Set up information to write from the pixel buffer
		jcprops.DIBWidth = nWidth;
		jcprops.DIBHeight = nHeight;			// Implies a bottom-up DIB
		jcprops.DIBBytes = pbyBits;
		jcprops.DIBPadBytes = dib_pad_bytes;

		// Note: the following are default values and thus
		// do not need to be set.
		jcprops.DIBChannels = 3;
		jcprops.DIBColor = IJL_BGR;
		jcprops.JPGFile = (LPSTR)lpszPathName;

		// Specify JPEG file creation parameters
		jcprops.JPGWidth = nWidth;
		jcprops.JPGHeight = nHeight;

		// Note: the following are default values and thus
		// do not need to be set.
		jcprops.JPGChannels = 3;
		jcprops.JPGColor = IJL_YCBCR;
		jcprops.JPGSubsampling = IJL_411;		// 4:1:1 subsampling
		jcprops.jquality = nQuality;

		// Write the actual JPEG image from the pixel buffer
		jerr = ijlWrite(&jcprops, IJL_JFILE_WRITEWHOLEIMAGE);
		if (IJL_OK != jerr)
		{
			bResult = FALSE;
			__leave;
		}
	} // __try

	__finally
	{
		// Clean up the IntelR JPEG library
		ijlFree(&jcprops);
	}

	return bResult;
} // EncodeJpgFileFromDIB

BOOL JPG_API EncodeJpgFileFromDIBWithComment(LPCSTR lpszPathName, int nWidth, int nHeight,
												BYTE* pbyBits, int nQuality, char* pszComment)
{
	BOOL bResult = TRUE;
	IJLERR jerr;
	DWORD dib_pad_bytes;

	// Allocate the IJL JPEG_CORE_PROPERTIES structure
	JPEG_CORE_PROPERTIES jcprops;
	__try
	{
		// Initialize the IntelR JPEG library
		jerr = ijlInit(&jcprops);
		if (IJL_OK != jerr)
		{
			bResult = FALSE;
			__leave;
		}

		dib_pad_bytes = IJL_DIB_PAD_BYTES(nWidth, 3);

		// Set up information to write from the pixel buffer
		jcprops.DIBWidth = nWidth;
		jcprops.DIBHeight = nHeight;			// Implies a bottom-up DIB
		jcprops.DIBBytes = pbyBits;
		jcprops.DIBPadBytes = dib_pad_bytes;

		// Note: the following are default values and thus
		// do not need to be set.
		jcprops.DIBChannels = 3;
		jcprops.DIBColor = IJL_BGR;
		jcprops.JPGFile = (LPSTR)lpszPathName;

		// Specify JPEG file creation parameters
		jcprops.JPGWidth = nWidth;
		jcprops.JPGHeight = nHeight;

		// Note: The following are default values and do not need to be set.
		jcprops.JPGChannels = 3;
		jcprops.JPGColor = IJL_YCBCR;
		jcprops.JPGSubsampling = IJL_411;		// 4:1:1 subsampling
		jcprops.jquality = nQuality;

		jcprops.jprops.jpeg_comment = pszComment;
		jcprops.jprops.jpeg_comment_size = (unsigned short)strlen(pszComment);

		// Write the actual JPEG image from the pixel buffer
		jerr = ijlWrite(&jcprops, IJL_JFILE_WRITEWHOLEIMAGE);
		if (IJL_OK != jerr)
		{
			bResult = FALSE;
			__leave;
		}
	} // __try

	__finally
	{
		// Clean up the IntelR JPEG library
		ijlFree(&jcprops);
	}

	return bResult;
} // EncodeJpgFileFromDIB

BOOL JPG_API DecodeJpgFileToDIB(LPCSTR lpszPathName, int* pnWidth, int* pnHeight,
							BYTE* pbyBits, char* pszComment, int nMaxComment)
{
	int nImageSize;
	JPEG_CORE_PROPERTIES p;
	BOOL bResult = TRUE;

	__try
	{
		if (ijlInit(&p) != IJL_OK)
		{
			bResult = FALSE;
			__leave;
		}

		p.JPGFile = lpszPathName;
		p.jprops.jpeg_comment = pszComment;
		p.jprops.jpeg_comment_size = nMaxComment;
		if (ijlRead(&p, IJL_JFILE_READPARAMS) != IJL_OK)
		{
			bResult = FALSE;
			__leave;
		}

		// Add NULL character onto comment
		if (pszComment)
			pszComment[p.jprops.jpeg_comment_size] = '\0';

		switch (p.JPGChannels)
		{
		case 1:
			p.JPGColor = IJL_G;
			p.DIBChannels = 3;
			p.DIBColor = IJL_BGR;
			break;
		case 3:
			p.JPGColor = IJL_YCBCR;
			p.DIBChannels = 3;
			p.DIBColor = IJL_BGR;
			break;
		case 4:
			p.JPGColor = IJL_YCBCRA_FPX;
			p.DIBChannels = 4;
			p.DIBColor = IJL_RGBA_FPX;
			break;
		default:
			p.DIBColor = (IJL_COLOR)IJL_OTHER;
			p.JPGColor = (IJL_COLOR)IJL_OTHER;
			p.DIBChannels = p.JPGChannels;
			break;
		}

		p.DIBWidth = p.JPGWidth;
		p.DIBHeight = p.JPGHeight;
		p.DIBPadBytes = IJL_DIB_PAD_BYTES(p.DIBWidth, p.DIBChannels);
		nImageSize = ((p.DIBWidth * p.DIBChannels) + p.DIBPadBytes);
		nImageSize *= p.DIBHeight;
		if (pbyBits)
		{
			p.DIBBytes = (BYTE*)malloc(nImageSize);
			if (ijlRead(&p, IJL_JFILE_READWHOLEIMAGE) != IJL_OK)
			{
				bResult = FALSE;
				__leave;
			}
		}

		if (p.DIBColor == IJL_RGBA_FPX)
		{
			bResult = FALSE;
			__leave;
		}
	}

	__finally
	{
		if (bResult)
		{
			*pnWidth = p.DIBWidth;
			*pnHeight = p.DIBHeight;

			if (pbyBits)
				memcpy(pbyBits, p.DIBBytes, nImageSize);
		}

		if (p.DIBBytes)
			free(p.DIBBytes);

		// Clean up the IntelR JPEG library
		ijlFree(&p);
	}

	return bResult;
}	// DecodeJpgFileToDIB

BOOL JPG_API HackJpgComments(LPCSTR lpszPathName, LPCSTR lpszComment)
{
	// Insert comments into the header block of the JPG file
	long lOrgFileSize, lOrgFileOffset;
	char* pszOrgFile = NULL;
	int nCommentLength = 0;

	FILE* fp = fopen(lpszPathName, "rb");
	if (fp == NULL)
		return FALSE;

	lOrgFileSize = _filelength(_fileno(fp));
	pszOrgFile = (char*)malloc(lOrgFileSize);
	if (pszOrgFile == NULL)
	{
		fclose(fp);
		return FALSE;
	}

	fread(pszOrgFile, lOrgFileSize, 1, fp);
	fclose(fp);

	fp = fopen(lpszPathName, "wb");
	if (fp == NULL)
	{
		free(pszOrgFile);
		return FALSE;
	}

	fwrite(pszOrgFile, 22, 1, fp);

	nCommentLength = (strlen(lpszComment) + 2);
	fputc(nCommentLength >> 8, fp);
	fputc(nCommentLength & 0xff, fp);

	fwrite(lpszComment, strlen(lpszComment), 1, fp);

	lOrgFileOffset = (22 + (pszOrgFile[22] >> 8) + pszOrgFile[23]);
	fwrite(&pszOrgFile[lOrgFileOffset], lOrgFileSize-lOrgFileOffset, 1, fp);
	fclose(fp);

	return TRUE;
}	// HackJpgComments
