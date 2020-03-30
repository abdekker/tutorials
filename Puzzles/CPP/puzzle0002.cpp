#include "stdafx.h"
#include "puzzlePrototypes.h"

// Write a function that adds two numbers without using any addition, subtraction, increment or
// decrement operator.

// Source: Papyrus coding test

/* Commentary:
Hmm, tricky. Thinking about ways to so it using *, /, %...
Maybe exponentials / logarithms? Should work.
e^(a+b) = e^a * e^b
Therefore log(e^a * e^b) = a + b
*/

int RunPuzzle0002(const int a, const int b)
{
#ifdef _DEBUG
	// Create local variables for debugging
    double expA = exp(a);
    double expB = exp(b);
    double expAB = (expA * expB);
    double logResult = log(expAB);
    int sumResult = static_cast<int>(round(logResult));
    return sumResult;
#else
	return static_cast<int>(round(log(exp(a) * exp(b))));
#endif
   
    // Tested with:
    // a = 3, b = 7
    // a = 5, b = 6
    // a = 22, b = -3
}