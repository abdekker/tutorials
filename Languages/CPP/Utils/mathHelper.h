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

    bool IsPrime(const int cCandidate)
    {
        // Primality testing
        if (cCandidate < 2)
            return false; // Too small

        if (!(cCandidate & 1) && (cCandidate != 2))
            return false; // Even (and not 2)

        // Test for odd divisors
        int square = 9;
        for (int i=3; square <= cCandidate; square += (i*2 + 1), i+=2)
        {
            if (!(cCandidate % i))
                return false; // Odd and composite
        }

        return true;
    }

    int CountBitsSet_U1(unsigned int input)
    {
         // Count the number of bits set

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
