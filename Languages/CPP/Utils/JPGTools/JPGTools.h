#ifndef JPG_TOOLS_H
#define JPG_TOOLS_H

#include <windows.h>

#ifdef JPGTOOLS_EXPORTS
	// export these functions when used in .DLL
	#define JPG_API __declspec(dllexport) __stdcall
#else
	#ifdef JPGTOOLS_IMPORTS
		// import these functions when used in .DLL
		#define JPG_API __declspec(dllimport) __stdcall
	#endif	// JPGTOOLS_IMPORTS
#endif	// JPGTOOLS_EXPORTS

#ifdef __cplusplus
extern "C" {
#endif	// __cplusplus

BOOL JPG_API EncodeJpgFileFromDIB(LPCSTR lpszPathName, int nWidth, int nHeight,
			BYTE* pbyBits, int nQuality);
BOOL JPG_API EncodeJpgFileFromDIBWithComment(LPCSTR lpszPathName, int nWidth, int nHeight,
			BYTE* pbyBits, int nQuality, char* pszComment);
BOOL JPG_API DecodeJpgFileToDIB(LPCSTR lpszPathName, int* pnWidth, int* pnHeight,
			BYTE* pbyBits, char* pszComment, int nMaxComment);

BOOL JPG_API HackJpgComments(LPCSTR lpszPathName, LPCSTR lpszComment);

#ifdef __cplusplus
}
#endif	// __cplusplus
#endif	// JPG_TOOLS_H
