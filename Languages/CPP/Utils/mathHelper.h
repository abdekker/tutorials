#pragma once

// Header-only helper class for some maths and computer science tricks
using namespace std;

// General purpose maths class
class mathHelper
{
public:
    // Constructor / destructor
    mathHelper() {}
    ~mathHelper() {}

    // Count bits set
    int CountBitsSet_U1(unsigned int input)
    {
        // Known as the 'parallel' or 'variable-precision SWAR' algorithm. This code was taken
        // from the accepted answer to: https://stackoverflow.com/questions/109023
        input = (input - ((input >> 1) & 0x55555555));
        input = (input & 0x33333333) + ((input >> 2) & 0x33333333);
        return ((((input + (input >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24);
    }

    int CountBitsSet_U2 (unsigned int input)
    {
        // More readable version (which won't be as quick, though)
        int bits = 0;
        while (input > 0) {           // repeat until all bits are zero
            if ((input & 1) == 1)     // check lower bit
                bits++;

            input >>= 1;              // shift bits, removing lower bit
        }
        return bits;
    }
};
