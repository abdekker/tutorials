// SimpleDLL.h - Contains declarations of some math functions
#pragma once

// Calling conventions (examples assume the function "int MyFunc(const int a)")
// * __cdecl
//      - Parameters are pushed right-to-left on the stack
//      - Caller cleans the stack after the function returns
//      - Functions names are undecorated (ie. "MyFunc")
//          - This can be checked with a tool like Dependencies
//      - To use from Delphi:
//          - TMyFunc = function(const a: Integer) : Integer; cdecl;
//          - fMyFunc: TMyFunc;
//          - hDLL := LoadLibrary(PATH-TO-DLL);             // Check (hDLL <> 0)
//          - @fMyFunc := GetProcAddress(hDLL, 'MyFunc');   // Check (Assigned(@fMyFunc))
//          - myInt := fMyFunc(7);

// * __stdcall
//      - As with __cdecl, parameters are pushed right-to-left on the stack
//      - Callee cleans the stack after the function returns
//      - Functions names are decorated (ie. "_MyFunc@4")
//          - Prefix is an underscore
//          - Postfix is the size of parameters (0 for no parameters)
//          - Sometimes a .def file is used to simplify
//      - To use in Delphi:
//          - TMyFunc = function(const a: Integer) : Integer; stdcall;
//          - as with __cdecl except...
//          - @fMyFunc := GetProcAddress(hDLL, '_MyFunc@4');

// * Developers can include an optional .def file with C++ DLL projects with an EXPORTS section
//      - For __cdecl define "MyFunc" as "MyFunc = MyFunc"
//      - For __stdcall define "MyFunc" as "MyFunc = _MyFunc@4"
//      - In theory, the .def makes it easier for external applications to consume methods from
//          this DLL (they only look for "MyFunc" and do not need to know the calling convention)

// Choose the calling convention below ("__cdecl" is the default). This choice does not affect the
// C++ SimpleDllClient application because that application includes this header directly.
#ifdef SIMPLEDLL_EXPORTS
//#define SIMPLEDLL_API __declspec(dllexport) __cdecl
#define SIMPLEDLL_API __declspec(dllexport) __stdcall
#else
//#define SIMPLEDLL_API __declspec(dllimport) __cdecl
#define SIMPLEDLL_API __declspec(dllimport) __stdcall
#endif

// Declare functions exported from this DLL to have C linkage. This allows these functions to be
// used in C-language and Delphi modules.
#ifdef __cplusplus
extern "C" {
#endif

// The Fibonacci recurrence relation describes a sequence F where F(n) is
//      { n = 0, a
//      { n = 1, b
//      { n > 1, F(n-2) + F(n-1)
// for some initial integral values a and b.
// If the sequence is initialized F(0) = 1, F(1) = 1, then this relation produces the well-known
// Fibonacci sequence: 1, 1, 2, 3, 5, 8, 13, 21, 34, ...

// Initialize a Fibonacci relation sequence such that F(0) = a, F(1) = b. This function must be
// called before any other function.
void SIMPLEDLL_API fibonacciInit64(
    const unsigned long long a, const unsigned long long b);

// Produce the next value in the sequence
// Returns true on success and updates current value and index;
// false on overflow, leaves current value and index unchanged.
bool SIMPLEDLL_API fibonacciNext64();

// Get the current value in the sequence
unsigned long long SIMPLEDLL_API fibonacciCurrent64();

// Get the position of the current value in the sequence
unsigned int SIMPLEDLL_API fibonacciIndex();

// 32-bit versions
void SIMPLEDLL_API fibonacciInit32(const unsigned int a, const unsigned int b);
bool SIMPLEDLL_API fibonacciNext32();
unsigned int SIMPLEDLL_API fibonacciCurrent32();
// "fibonacciIndex" is used for the current position in both the 64-bit and 32-bit sequences

// Test functions for external applications (such as x86 applications written in Delphi)
unsigned int SIMPLEDLL_API SimpleReturn();
int SIMPLEDLL_API SimpleSum(const int a, const int b);
float SIMPLEDLL_API SimpleMultiply(const float a, const char b);

#ifdef __cplusplus
}
#endif

