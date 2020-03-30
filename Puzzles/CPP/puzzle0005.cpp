#include "stdafx.h"
#include "puzzlePrototypes.h"

// Implement a method "distinct(...)" that returns an ascendingly sorted array that does not
// contain any repetitions but those of the number '1'. All repetitions of '1' should be the
// last values of the array.
// Example: distinct({3,3,1,1,9}) returns {1,3,9,1}.

// Source: Papyrus coding test

/* Commentary:
Place all repetitions of 1 (after the first) at the end of the array.
First observation is that we may need to write some sort algorithm.
I'm going to "cheat" and use the std::sort function with std::vector<int> because they are easy to use.

Steps will be:
	* Count the number of 1s (duplicates need to be inserted at the end)
	* Sort the array
	* Remove duplicates
	* Insert duplicates of 1 (counted in the first step)
	* Return array
*/

// Note: This method would be called "distinct" to following the test strictly!
ARRAY RunPuzzle0005(ARRAY input)
{
    // Not clear whether we need to preserve the input array or not. Assume for now that we should...
    // Copy the input array
    ARRAY output(input);
    
    // Count the number of ones (we'll need to duplicate these at the end of the array). We could
    // just iterate through the array, but there is a nice library function "count_if" which I'll
	// use. We'll define a lambda function to perform the count.
    int countOnes = count_if(output.begin(), output.end(), [](int i) { return i == 1;} );
    //DBG: count << "ones = " << countOnes << endl;
    
    // Now use the standard library "sort" method. Is this "cheating" or should we write an entire
	// sort algorithm?
    sort(output.begin(), output.end());
    
    // Remove duplicates using "std::unique". Alternatively, we could iterate through the (now
	// sorted!) array, and while the following element is equal to the previous element, remove it
	// using "vector::erase". The "std::unique" method returns the last non-duplicated element.
    auto lastNonDuplicate = unique(output.begin(), output.end());
    
    // Remove duplicate elements (which were placed at the end of the array by "std::unique").
    output.erase(lastNonDuplicate, output.end());
    
    // Finally insert duplicate 1s. Remember that only excess duplicates should be inserted. So if the
    // count is 0 or 1 add nothing, else add (countOnes - 1).
    for (int duplicate = 0; duplicate < (countOnes - 1); duplicate++)
        output.push_back(1);
    
    // Send back the result!
    return output;
}
