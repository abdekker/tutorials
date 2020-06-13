/*=============================================================================
File:
  Utilities.h

SubSystem:
  Imaging

Notes:
  A collection of mathematical and other utilities. No functions are exported.

© DekkerSoft. All rights reserved.
=============================================================================*/

#ifndef UTILITIES_H
#define UTILITIES_H

#include "Imaging.h"
// Note: "C" linkage is only required where functions are being exported from this DLL

// Functions not exported
template <typename T>
T clamp(T value, T lower_bound, T upper_bound) { return min(max(value, lower_bound), upper_bound); }

bool FitLineToData(BYTE byPoints, double* padX, double* padY, double* pdSlope, double* pdConstant);
BYTE GetIntersection(POINT ptLnA1, POINT ptLnA2, POINT ptLnB1, POINT ptLnB2, float* pfX, float* pfY);
void InsertionSort(BYTE abyWindow[]);

#endif		// UTILITIES_H
