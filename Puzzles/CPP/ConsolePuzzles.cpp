
#include "stdafx.h"
#include "helperFunctions.h"
#include "puzzlePrototypes.h"

#include <conio.h>

// See the following files for details on the puzzles:
// * ..\Notes-Puzzles.txt
// * ..\Notes-Puzzles-Details.txt
int main()
{
	// C++ code to solve various puzzles, see ../Puzzles/Notes-Puzzles.txt for details

	// All puzzles, latest puzzle at the bottom (this section can be collapsed in the IDE)
	{
		// Puzzle 0001
		/*{
			// // Puzzle 0001: Print numbers with a "5"
			ClassPuzzle0001 puzzle;
			puzzle.RunPuzzle();
		}*/

		// Puzzle 0002
		/*{
			// Puzzle 0002: Add two numbers, but without using addition/increment operators
			cout << "Add two numbers without using addition, subtraction, incremement or decrement operators\n";
			cout << "a=3, b=7, a+b=" << RunPuzzle0002(3, 7) << endl;
			cout << "a=5, b=6, a+b=" << RunPuzzle0002(5, 6) << endl;
			cout << "a=22, b=-3, a+b=" << RunPuzzle0002(22, -3) << endl;
		}*/

		// Puzzle 0003 - unsolved

		// Puzzle 0004
		/*{
			// Puzzle 0004: Print "Hello World" without using a semicolon (;)
			cout << "Print \"Hello World\" without using a semicolon (;)\n";
			RunPuzzle0004();
		}*/

		// Puzzle 0005
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

		// Puzzle 0006
		/*{
			// Puzzle 0006: Modify each element of an array by multiplying it by a constant factor
			// Example: multiply_array_elements({1,2,4,5,6,0,8}, 4) returns {4,8,16,20,24,0,32}.
			ARRAY test = {1, 2, 4, 5, 6, 0, 8};
			cout << "Before: "; PrintArray(test, 3);
			RunPuzzle0006(&test, 4);
			cout << "After:  "; PrintArray(test, 3);
		}*/
	}

	// Current (usually latest) puzzle
	{
		// Puzzle 0007
		{
			// Puzzle 0007: Given a positive integer n, generate a square matrix filled with elements from
			// 1 to n^2 in spiral order.
			// Example:
			// Input: 3
			// Output:	[ 1 2 3 ]
			//			[ 8 9 4 ]
			//			[ 7 6 5 ]
			MATRIX testMatrix;
			int widthRequired;
			for (int size=1; size <= 12; size++)
			{
				RunPuzzle0007(&testMatrix, size);
				cout << "Size: " << size << std::endl;
				widthRequired = static_cast<int>(std::ceil(log10(size * size)));
				PrintMatrix(testMatrix, widthRequired);
				cout << "\n\n";
			}
		}
	}

	// Prompt for exit (holds application on-screen if launched outside the IDE)
	std::cout << "\nFinished...press a key to exit\n";
	(void)_getch();
	return 0;
}