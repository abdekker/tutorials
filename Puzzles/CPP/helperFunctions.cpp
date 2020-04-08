#pragma once
#include "helperFunctions.h"
#include <iomanip>		// std::setw, std::setfill

// General helper functions for puzzles
void PrintArray(ARRAY input, const char cSeparator /*= ' '*/)
{
	// Output the array to the console eg. {1,11,21} => 1 11 21
	if (input.size() > 0)
	{
		for (auto a : input)
			std::cout << a << cSeparator;
        
		std::cout << std::endl;
	}
	else
		std::cout << "(empty)\n";
}

void PrintArray(ARRAY input, const int cWidth, const char cFill /*= ' '*/, const char cSeparator /*= ' '*/)
{
	// Array version which formats the output for alignment
	if (input.size() > 0)
	{
		for (auto a : input)
		{
			std::cout << std::setfill(cFill) << std::setw(cWidth);
			std::cout << a << cSeparator;
		}
        
		std::cout << std::endl;
	}
	else
		std::cout << "(empty)\n";
}

void PrintMatrix(MATRIX input, const char cSeparator /*= ' '*/)
{
	// Output the matrix to the console eg. [{1,11,21}, {100,10,0,-10,=100}, {3,4,5}] =>
	// 1 2 19
	// 100 10 0 -10 -100
	// 3 4 5
	if (input.size() > 0)
	{
		for (auto innerArray : input)
		{
			if (innerArray.size() > 0)
			{
				for (auto a : innerArray)
					std::cout << a << cSeparator;

				std::cout << std::endl;
			}
			else
				std::cout << "(row empty)\n";
		}
	}
	else
		std::cout << "(matrix empty)\n";
}

void PrintMatrix(MATRIX input, const int cWidth, const char cFill /*= ' '*/, const char cSeparator /*= ' '*/)
{
	// Matrix version which formats the output for alignment
	if (input.size() > 0)
	{
		for (auto innerArray : input)
		{
			if (innerArray.size() > 0)
			{
				for (auto a : innerArray)
				{
					std::cout << std::setfill(cFill) << std::setw(cWidth);
					std::cout << a << cSeparator;
				}

				std::cout << std::endl;
			}
			else
				std::cout << "(row empty)\n";
		}
	}
	else
		std::cout << "(matrix empty)\n";
}
