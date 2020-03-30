
#include "stdafx.h"
#include <conio.h>

#include "puzzle0001.h"
#include "puzzlePrototypes.h"
#include "helperFunctions.h"

// Puzzles
// * 0001: Print all numbers up to 1000 that have the digit 5 in their number
// * 0002: Add two numbers without using any addition, subtraction, increment or decrement operator
// * 0003: Find the largest and smallest of three numbers without using conditional statements or ternary operators
//		Unsolved!
// * 0004: Print "Hello World" without using a semicolon
// * 0005: Sort an array, removing duplicates except 1s. Place duplicate 1s at the end.
// * 0006: Modify each element of an array by multiplying it by a constant factor
int main()
{
	// C++ code to solve various puzzles, see ../Puzzles/Notes-Puzzles.txt for details

	/*{
		// Puzzle 0001: Print numbers with a "5"
		//ClassPuzzle0001 puzzle;
		//puzzle.RunPuzzle();
	}*/

	/*{
		// Puzzle 0002: Add two numbers, but no addition/increment operators
		cout << "Add two numbers without using addition, subtraction, incremement or decrement operators\n";
		cout << "a=3, b=7, a+b=" << RunPuzzle0002(3, 7) << endl;
		cout << "a=5, b=6, a+b=" << RunPuzzle0002(5, 6) << endl;
		cout << "a=22, b=-3, a+b=" << RunPuzzle0002(22, -3) << endl;
	}*/

	/*{
		// Puzzle 0004: Print "Hello World" without using a semicolon (;)
		cout << "Print \"Hello World\" without using a semicolon (;)\n";
		RunPuzzle0004();
	}*/

	/*{
		// Puzzle 0005: Sort an array, removing duplicates, except place duplicate 1s at the end
		// Example: distinct({3,3,1,1,9}) returns {1,3,9,1}.
		// Note: Using std::vector<int> because they are easy to work with!
		ARRAY in{1,5,6,2,5,1};
		ARRAY out = RunPuzzle0005(in);
		cout << "Input:  "; PrintArray(in);
		cout << "Output: "; PrintArray(out);

		in = {1,5,-7,-8,-7,0,1,1,1,6,2,5,1};
		out = RunPuzzle0005(in);
		cout << "Input:  "; PrintArray(in);
		cout << "Output: "; PrintArray(out);

		in = {2,5,2,2,0,2,9,2};
		out = RunPuzzle0005(in);
		cout << "Input:  "; PrintArray(in);
		cout << "Output: "; PrintArray(out);

		in.clear();
		out = RunPuzzle0005(in);
		cout << "Input:  "; PrintArray(in);
		cout << "Output: "; PrintArray(out);
	}*/

	{
		// Puzzle 0006: Modify each element of an array by multiplying it by a constant factor
		// Example: multiply_array_elements({1,2,4,5,6,0,8}, 4) returns {4,8,16,20,24,0,32}.
		ARRAY test = {1, 2, 4, 5, 6, 0, 8};
		cout << "Before: "; PrintArray(test);
		RunPuzzle0006(&test, 4);
		cout << "After:  "; PrintArray(test);
	}

	// Prompt for exit (holds application on-screen if launched outside the IDE)
	std::cout << "\nFinished...press a key to exit\n";
	(void)_getch();
	return 0;
}