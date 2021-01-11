// Standard header file for all puzzle C++ code
// Note: There are a variety of arguments for using "#pragma once" or include guards
// * #pragma once
//		- fewer characters and less error-prone
//		- prevents the inclusion of a file with the same location
//		- passes the onus onto the compiler to guard against multiple inclusion
//	BUT
//		- while widely supported is not standard and some compilers do not support it
//		- causes problems where the build system copies the same file to different locations
//		- complicates the writing of mock class tests

// * include guards
//		- more standardised and therefore more portable
//		- passes the onus onto the developer to guard against multiple inclusion
//	BUT
//		- more characters and error-prone (especially when you copy-paste)

// Some commentators advise using include guards AND #pragma. There is no right or wrong answer,
// though on balance include guards are more portable and allow for greater control.

#ifndef PUZZLES_STD_H
#define PUZZLES_STD_H

// Console I/O
#include <iostream>
#include <stdio.h>

// Strings and timing calculations
#include <chrono>		// Timing calculations
#include <string>		// string.h is an older c header...

// Algorithms and data structures
#include <algorithm>
#include <vector>

// External helper classes and utilities
#include "../../Languages/CPP/Utils/stringHelper.h"
#include "../../Languages/CPP/Utils/dataTypes.h"
#include "../../Languages/CPP/Utils/mathHelper.h"
#include "../../Languages/CPP/Utils/vectorHelper.h"

// Internal helper methods
#include "helperFunctions.h"

#endif //PUZZLES_STD_H
