#include "stdafx.h"

/* multiply_array_elements() is a function to modify each element of an array by multiplying it with n.

	#include <vector>
	typedef std::vector<int> ARRAY; 

	void multiply_array_elements(ARRAY a, int n)
	{ 
		for (auto el : a) {
			el *= n;
		}
	}

	int main(int argc, char *argv[])
	{ 
		ARRAY test = {1, 2, 4, 5, 6, 0, 8}; 
		multiply_array_elements(test, 4); 
		return 0;
	}
	
a) What are the values of "test" after executing main?
b) What needs to be changed to make the function "multiply_array_elements()" work as expected, and why? */

// Source: Papyrus coding test

/* Commentary:
a
First impressions (not tried in IDE yet).
The code just multiples the elements locally, but does put them back into the array.
Therefore expect there to be no change in test.
Trying now...confirmed

b
To modify the program, I'd want to ensure the vector was passed by reference, probably do it with pointers.
Then each element needs to be modified in-place. We could use the vector::at, or something like the standard
"for_each" method.
*/

// Note: This method would be called "multiply_array_elements" to following the test strictly!
void RunPuzzle0006(ARRAY* input, const int cMultiplier)
{
	// Option 1: Iterate through each element and multiple using "vector::at"
	//for (int el = 0; el < input->size(); el++)
	//	input->at(el) *= cMultiplier;

	// Option 2: Use the standard "for_each" function and a lambda function
	// Note: The lambda must pass values by reference so they can be modified in-place
	std::for_each(input->begin(), input->end(), [&](int &n){ n *= cMultiplier; });
}
