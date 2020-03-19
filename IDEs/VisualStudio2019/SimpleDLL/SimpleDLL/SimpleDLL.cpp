// MathLibrary.cpp : Defines the exported functions for the DLL.
#include "pch.h" // use stdafx.h in Visual Studio 2017 and earlier
#include <utility>
#include <limits.h>
#include "SimpleDLL.h"

// DLL internal state variables:
static unsigned long long previous64_;  // Previous value, if any
static unsigned long long current64_;   // Current sequence value
static unsigned int previous32_;
static unsigned int current32_;
static unsigned int index_;             // Current sequence position

// Initialize a Fibonacci relation sequence such that F(0) = a, F(1) = b.
// This function must be called before any other function.
void SIMPLEDLL_API fibonacciInit64(
    const unsigned long long a,
    const unsigned long long b)
{
    index_ = 0;
    current64_ = a;
    previous64_ = b;    // see special case when initialized
}

// Produce the next value in the sequence.
// Returns true on success, false on overflow.
bool SIMPLEDLL_API fibonacciNext64()
{
    // Check to see if we'd overflow result or position
    if (((ULLONG_MAX - previous64_) < current64_) ||
        (UINT_MAX == index_))
    {
        return false;
    }

    // Special case when index == 0, just return b value
    if (index_ > 0)
    {
        // Otherwise, calculate next sequence value
        previous64_ += current64_;
    }

    std::swap(current64_, previous64_);
    ++index_;
    return true;
}

// Get the current value in the sequence
unsigned long long SIMPLEDLL_API fibonacciCurrent64()
{
    return current64_;
}

// Get the current index position in the sequence
unsigned int SIMPLEDLL_API fibonacciIndex()
{
    return index_;
}

// 32-bit versions
void SIMPLEDLL_API fibonacciInit32(const unsigned int a, const unsigned int b)
{
    index_ = 0;
    current32_ = a;
    previous32_ = b;
}

bool SIMPLEDLL_API fibonacciNext32()
{
    // Check to see if we'd overflow result or position
    if (((UINT_MAX - previous32_) < current32_) ||
        (UINT_MAX == index_))
    {
        return false;
    }

    if (index_ > 0)
        previous32_ += current32_;

    std::swap(current32_, previous32_);
    ++index_;
    return true;
}

unsigned int SIMPLEDLL_API fibonacciCurrent32() { return current32_; }

// Test functions for Delphi and other external applications
unsigned int SIMPLEDLL_API SimpleReturn()
{
    return 17;
}

int SIMPLEDLL_API SimpleSum(const int a, const int b)
{
    // In Delphi, declare as:
    //      TSimpleSum = function(const a: Integer; const b: Integer) : Cardinal; stdcall;
    return (a + b);
}

float SIMPLEDLL_API SimpleMultiply(const float a, const char b)
{
    // In Delphi, declare as:
    //      TSimpleSum = function(const a: Single; const b: Char) : Single; stdcall;
    return (a * (float)b);
}
