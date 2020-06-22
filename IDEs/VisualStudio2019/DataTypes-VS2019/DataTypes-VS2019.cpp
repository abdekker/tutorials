#include <iostream>
#include <conio.h>
#include "..\..\..\Languages\CPP\Utils\stringHelper.h"

// In VS2003 "#include <limits.h>" was required to get ranges for in-built types. This is implicit
// or automatic in VS2019.

// Compiler trick to see what the largest number is when the preprocessor compiles this code. All
// unsigned values are required to be of type "uintmax_t", and therefore "~0U", "0U-1" and "-1U"
// should all represent the maximum representable number. In future this may change, but when this
// application was written, this is a __int64 (long long) type.
#pragma message("  Testing maximum representable number...")
#if (~0U < 18446744073709551615U)
	#error "  Error: This should be a large positive value, at least ULLONG_MAX >= 2^{64} - 1"
#else
	#pragma message("  ...which should be of type unsigned __int64 (uintmax_t)")
#endif

using namespace std;
int main()
{
    // Welcome message. See the ..\..\VisualStudio2003\DataTypes-VS2003 project which uses
    // "printf". The following line would be: "printf("Data types in Visual Studio 2019\n\n");"
    cout << "Data types in Visual Studio 2019\n\n";

    // Define a string helper (using header-only template) to use "printf" style formatting
    stringHelper formatter;

	// Show information about data types
	cout << "### Integer ###\n";
	cout << "\t\t\tSize (bytes)\tMin\t\t\tMax\n";
	{
		// bool
		bool min = false;
		bool max = true;
		cout << "bool\t\t\t" << sizeof(bool) << "\t\t" << min << "\t\t\t" << max << "\n";
        // Or: cout << formatter.formatString("bool\t\t\t%d\t\t%d\t\t\t%d\n", sizeof(bool), min, max);
	}

    {
		// char, __int8
		char min = CHAR_MIN;
		char max = CHAR_MAX;
		unsigned char maxU = UCHAR_MAX;
        cout << formatter.formatString("char (__int8)\t\t%d\t\t%d\t\t\t%d\n", sizeof(char), min, max);
        cout << formatter.formatString("unsigned char\t\t%d\t\t0\t\t\t%u\n", sizeof(unsigned char), maxU);
	}

    {
		// wchar_t, __wchar_t (wide char: should be the same as __int16)
		wchar_t max = _UI16_MAX;
		cout << formatter.formatString("wchar_t\t\t\t%d\t\t0\t\t\t%u\n", sizeof(wchar_t), max);
	}

    {
		// short, short int, __int16
		short min = SHRT_MIN;
		short max = SHRT_MAX;
		unsigned short maxU = USHRT_MAX;
		cout << formatter.formatString("short (__int16)\t\t%d\t\t%d\t\t\t%d\n", sizeof(short), min, max);
		cout << formatter.formatString("unsigned short\t\t%d\t\t0\t\t\t%u\n", sizeof(unsigned short), maxU);
	}

	{
		// int, __int32
		int min = INT_MIN;
		int max = INT_MAX;
		unsigned int maxU = UINT_MAX;
		cout << formatter.formatString("int (__int32)\t\t%d\t\t%d\t\t%d\n", sizeof(int), min, max);
		cout << formatter.formatString("unsigned int\t\t%d\t\t%u\t\t\t%u\n", sizeof(unsigned int), 0, maxU);
	}

	{
		// long, __int32 (same as int)
		long min = LONG_MIN;
		long max = LONG_MAX;
		unsigned long maxU = ULONG_MAX;
		cout << formatter.formatString("long (__int32)\t\t%d\t\t%ld\t\t%ld\n", sizeof(long), min, max);
		cout << formatter.formatString("unsigned long\t\t%d\t\t0\t\t\t%lu\n", sizeof(unsigned long), maxU);
	}

	{
		// long long, __int64

		// Note: VS2003 did not format __int64 variables correctly according to the C99 standard,
        // and used the Microsoft-specific "%I64" format specifier instead. VS2019 uses the
        // compliant "%ll" format specifier.

		long long min = LLONG_MIN;
		long long max = LLONG_MAX;
		unsigned long long maxU = ULLONG_MAX;
        cout << formatter.formatString("long long (__int64)\t%d\t\t%lld\t%lld\n", sizeof(long long), min, max);
		cout << formatter.formatString("unsigned long long\t%d\t\t0\t\t\t%llu\n", sizeof(unsigned long long), maxU);
	}
	cout << "\n";

    cout << "### Floating point ###\n";
	cout << "\t\tSize (bytes)\tZero\t\t\tMin\t\t\tMax\n";
	{
		// float
		float zero = FLT_MIN;
		float min = -FLT_MAX;
		float max = FLT_MAX;
		cout << formatter.formatString("float\t\t%d\t\t%.10e\t%.10e\t%.10e\n", sizeof(float), zero, min, max);
	}

	{
		// double, long double
		double zero = DBL_MIN;
		double min = -DBL_MAX;
		double max = DBL_MAX;
		cout << formatter.formatString("double\t\t%d\t\t%.10e\t%.10e\t%.10e\n", sizeof(double), zero, min, max);
	}
	cout << "\n";

	// Prompt for exit
	cout << "Finished...press a key to exit\n";
	_getch();
}
