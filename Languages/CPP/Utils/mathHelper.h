#pragma once

// Header-only helper class for some maths and computer science tricks
using namespace std;
class mathHelper
{
public:
    mathHelper()
    {
        // Constructor
    }

    ~mathHelper()
    {
        // Destructor
    }

    int BitsSet_UL(unsigned long input)
    {
	    input = (input - ((input >> 1) & 0x55555555));
	    input = (input & 0x33333333) + ((input >> 2) & 0x33333333);
	    return ((((input + (input >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24);
    }
};
