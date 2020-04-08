#pragma once
#include "stdafx.h"

// Helper functions to use with the puzzles (generally not related to the puzzles themselves)
void PrintArray(ARRAY input, const char cSeparator = ' ');
void PrintArray(ARRAY input, const int cWidth, const char cFill = ' ', const char cSeparator = ' ');

void PrintMatrix(MATRIX input, const char cSeparator = ' ');
void PrintMatrix(MATRIX input, const int cWidth, const char cFill = ' ', const char cSeparator = ' ');
