/*=============================================================================
File:
  Imaging.h

SubSystem:
  Imaging

Notes:
  Main entry point for the Imaging DLL

© DekkerSoft. All rights reserved.
=============================================================================*/

#pragma once

#ifndef __IMAGINGH
#define __IMAGINGH

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef IMAGING_EXPORTS
	// export these functions when used in .DLL
	#define IMAGING_API __declspec(dllexport) __stdcall
#else
	// import these functions when used in .DLL
	#define IMAGING_API __declspec(dllimport) __stdcall
#endif

#include "CommonDefinitions.h"
#include "ImageCell.h"
#include "ImageProcessing.h"
#include "ogl.h"
#include "HeightField.h"

// Maximum image height
#define MAX_IMG_LENGTH 3000

#endif		//__IMAGINGH
