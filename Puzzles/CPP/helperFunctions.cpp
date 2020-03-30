#pragma once
#include "stdafx.h"
#include "helperFunctions.h"

// General helper functions

void PrintArray(ARRAY input, const char cSeparator /*= ' '*/)
{
	if (input.size() > 0)
	{
		for (auto a : input)
			std::cout << a << cSeparator;
        
		std::cout << std::endl;
	}
	else
		std::cout << "(empty)\n";
}