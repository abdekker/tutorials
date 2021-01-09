#include "stdafx.h"

/* Given a positive integer n, generate a square matrix filled with elements from 1 to n^2 in
spiral order.

Example
	Input: 3
	Output:	[ 1, 2, 3 ]
			[ 8, 9, 4 ]
			[ 7, 6, 5 ]

Source: https://leetcode.com/problems/spiral-matrix-ii/ */

// Note: An alternative would be to return a MATRIX created here. Our version requires the caller
// to first create the MATRIX and pass a pointer 
	/*MATRIX RunPuzzle0007(const int size)
	{
		MATRIX solution;
		// ...do something
		return solution;
	}*/

// Directions
#define RIGHT	0
#define DOWN	1
#define LEFT	2
#define UP		3

/*int f()
{
	// Demonstrates using a local function to create a dummy array
    static int i = 5;
    return i++;
}*/

void RotateDirection(POINT<int> &pt)
{
	// Rotate direction clockwise
	POINT<int> tmp = pt;
	pt.x = -pt.y;
	pt.y = tmp.x;
}

void RunPuzzle0007(MATRIX* input, const int size)
{
	// Clear the matrix
	input->clear();

	// Generate a starting (or "dummy") matrix. The correct values will be inserted later.
	ARRAY rowDummy(size);

	// Note: The syntax, "ARRAY rowDummy(size)", generates an array of 0s and is the simplest. For a
	// fancier dummy row, uncomment the code block to see a variety of options.
	/*{
		// Set the option you want...
		int option = 4;
		if (option == 1)
		{
			// Option 1: Manually build the row
			// Generates: [0, 1, 2, ...]
			rowDummy.clear();	// Not required if "rowDummy" was declared as "ARRAY rowDummy;"
			for (int pos = 0; pos < size; pos++)
				rowDummy.push_back(pos);
		}
		else if (option == 2)
		{
			// Option 2: Use std::generate and a local function with an internal static variable. Array
			// must have the required number of elements already. If required, uncomment function "f".
			// Generates: [5, 6, 7, ...] (the first time)
			std::generate(rowDummy.begin(), rowDummy.end(), f);
		}
		else if (option == 3)
		{
			// Option 3: Use std::generate and a lambda function. Array must have the required number of
			// elements already.
			// Generates: [10, 11, 12, ...]
			std::generate(rowDummy.begin(), rowDummy.end(), [n = 10] () mutable { return n++; });
		}
		else if (option == 4)
		{
			// Option 4: Use std::fill_n and a constant value. Array must have the required number of
			// elements already.
			// Generates: [-1, -1, -1, ...]
			std::fill_n(rowDummy.begin(), size, -1);
		}
	}*/

	// Put our dummy row into the matrix
	for (int tmp = 0; tmp < size; tmp++)
		input->push_back(rowDummy);

	// Initialise some data
	int nextDirection[4];
	nextDirection[RIGHT] = DOWN;
	nextDirection[DOWN] = LEFT;
	nextDirection[LEFT] = UP;
	nextDirection[UP] = RIGHT;

	POINT<int> directionsInc[4];
	directionsInc[RIGHT] = {1, 0};
	directionsInc[DOWN] = directionsInc[RIGHT]; RotateDirection(directionsInc[DOWN]);
	directionsInc[LEFT] = directionsInc[DOWN]; RotateDirection(directionsInc[LEFT]);
	directionsInc[UP] = directionsInc[LEFT]; RotateDirection(directionsInc[UP]);

	POINT<int> modifierMin[4];
	modifierMin[RIGHT] = {0, 1};
	modifierMin[DOWN] = {0, 0};
	modifierMin[LEFT] = {0, 0};
	modifierMin[UP] = {1, 0};

	POINT<int> modifierMax[4];
	modifierMax[RIGHT] = {0, 0};
	modifierMax[DOWN] = {-1, 0};
	modifierMax[LEFT] = {0, -1};
	modifierMax[UP] = {0, 0};

	// Now generate the spiral!
	int direction = RIGHT;
	POINT<int> posMin = {0, 0};
	POINT<int> posMax = {(size - 1), (size - 1)};
	POINT<int> posInc = directionsInc[direction];
	POINT<int> pos = {0, 0};

	int num = 1;
	int numMax = (size * size);
	do
	{
		input->at(pos.y).at(pos.x) = num;
		num++;
		if (num <= numMax)
		{
			pos += posInc;
			if ((pos < posMin) || (pos > posMax))
			{
				pos -= posInc;
				posMin += modifierMin[direction];
				posMax += modifierMax[direction];
				direction = nextDirection[direction];
				posInc = directionsInc[direction];
				pos += posInc;
			}
		}
	} while (num <= numMax);
}
