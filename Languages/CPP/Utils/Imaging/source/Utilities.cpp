#include "Utilities.h"
#include <math.h>

// Start: Functions not exported
bool FitLineToData(BYTE byPoints, double* padX, double* padY, double* pdSlope, double* pdConstant)
{
	// Fit a best-fit linear regression line to the given data. This code was adapted from:
	// http://stackoverflow.com/questions/5083465
	// Note: Called internally only so pointers are guaranteed to be valid
	double dSumX = 0.0f;	// Sum of x
	double dSumX2 = 0.0f;	// Sum of x^2
	double dSumXY = 0.0f;	// Sum of (x * y)
	double dSumY = 0.0f;	// Sum of y
	double dSumY2 = 0.0f;	// Sum of y^2
	for (BYTE byPoint = 0; byPoint < byPoints; byPoint++)
	{
		dSumX	+= padX[byPoint];
		dSumX2	+= (padX[byPoint] * padX[byPoint]);
		dSumXY	+= (padX[byPoint] * padY[byPoint]);
		dSumY	+= padY[byPoint];
		dSumY2	+= (padY[byPoint] * padY[byPoint]);
	}

	double dPoints = static_cast<double>(byPoints);
	double dDenominator = ((dPoints * dSumX2) - (dSumX * dSumX));
	if (fabs(dDenominator) < MIN_DOUBLE)
	{
		// Singular matrix => problem cannot be solved
		*pdSlope = 0.0f;
		*pdConstant = 0.0f;
		return false;
	}

	*pdSlope = ((dPoints * dSumXY) - (dSumX * dSumY)) / dDenominator;
	*pdConstant = ((dSumY * dSumX2) - (dSumX * dSumXY)) / dDenominator;

	// To compute the correlation co-efficient (measure of how well the regression line fits the
	// data), uncomment the lines below:
	/*double dTerm1 = ((dPoints * dSumXY) - (dSumX * dSumY));
	double dTerm2 = ((dPoints * dSumX2) - (dSumX * dSumX));
	double dTerm3 = ((dPoints * dSumY2) - (dSumY * dSumY));
	double dTerm23 = (dTerm2 * dTerm3);
	double dRsquared = 1.0f;
	if (fabs(dTerm23) > MIN_FLOAT)
		dRsquared = ((dTerm1 * dTerm1) / dTerm23);*/

	return true;
}

BYTE GetIntersection(POINT ptLn1A, POINT ptLn1B, POINT ptLn2C, POINT ptLn2D, float* pfX, float* pfY)
{
	// Determine the intersection point between two lines. Line 1 goes from A-B and line 2 goes
	// from C-D. If the lines intersect, the function returns 1. If the lines are parallel or
	// co-linear, the function returns 0. Adapted from: https://stackoverflow.com/questions/563198
	// Note: Called internally only so pointers are guaranteed to be valid
	float fDiffLn1_x = static_cast<float>(ptLn1B.x - ptLn1A.x);
	float fDiffLn1_y = static_cast<float>(ptLn1B.y - ptLn1A.y);

	float fDiffLn2_x = static_cast<float>(ptLn2D.x - ptLn2C.x);
	float fDiffLn2_y = static_cast<float>(ptLn2D.y - ptLn2C.y);

	float fDenominator = ((-fDiffLn2_x * fDiffLn1_y) + (fDiffLn1_x * fDiffLn2_y));
	if (fabsf(fDenominator) > MIN_FLOAT)
	{
		float s, t;
		s = ((-fDiffLn1_y * (ptLn1A.x - ptLn2C.x) + fDiffLn1_x * (ptLn1A.y - ptLn2C.y)) / fDenominator);
		t = (( fDiffLn2_x * (ptLn1A.y - ptLn2C.y) - fDiffLn2_y * (ptLn1A.x - ptLn2C.x)) / fDenominator);

		if ((s >= 0.0f) && (s <= 1.0f) &&
			(t >= 0.0f) && (t <= 1.0f))
		{
			// Collision detected!
			*pfX = (ptLn1A.x + (t * fDiffLn1_x));
			*pfY = (ptLn1A.y + (t * fDiffLn1_y));
			return 1;
		}
	}

	// No collision (lines are parallel or co-linear)
	return 0;
}

void InsertionSort(BYTE abyWindow[])
{
	// Insertion sort is efficient for sorting a small list of items
	// Note: This assumes a 3x3 (ie. 9) item array!
	BYTE byTemp;
	int nOuter, nInner;
	for (nOuter = 0; nOuter < 9; nOuter++)
	{
		byTemp = abyWindow[nOuter];
		for (nInner = nOuter-1; (nInner >= 0) && (byTemp < abyWindow[nInner]); nInner--)
			abyWindow[nInner+1] = abyWindow[nInner];

		abyWindow[nInner+1] = byTemp;
	}
}
// End: Functions not exported
