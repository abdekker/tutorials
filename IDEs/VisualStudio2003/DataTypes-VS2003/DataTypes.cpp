#include <windows.h>
#include <conio.h>
#include <stdio.h>
#include <limits.h>		// Defines ranges of types
#include <float.h>		// Platform-specific constants for floating point values

int main(int argc, char *argv[])
{
	// Welcome message
	printf("Data types in Visual Studio 2003\n\n");

	// Show information about data types
	printf("### Integers ###\n");
	printf("\t\t\tSize (bytes)\tMin\t\t\tMax\n");
	{
		// bool
		bool min = false;
		bool max = true;
		printf("bool\t\t\t%d\t\t%d\t\t\t%d\n", sizeof(bool), min, max);
	}

	{
		// char, __int8
		char min = CHAR_MIN;
		char max = CHAR_MAX;
		unsigned char maxU = UCHAR_MAX;
		printf("char (__int8)\t\t%d\t\t%d\t\t\t%d\n", sizeof(char), min, max);
		printf("unsigned char\t\t%d\t\t0\t\t\t%u\n", sizeof(unsigned char), maxU);
	}

	{
		// wchar_t, __wchar_t (wide char: should be the same as __int16)
		wchar_t max = _UI16_MAX;
		printf("wchar_t\t\t\t%d\t\t0\t\t\t%u\n", sizeof(wchar_t), max);
	}

	{
		// short, short int, __int16
		short min = SHRT_MIN;
		short max = SHRT_MAX;
		unsigned short maxU = USHRT_MAX;
		printf("short (__int16)\t\t%d\t\t%d\t\t\t%d\n", sizeof(short), min, max);
		printf("unsigned short\t\t%d\t\t0\t\t\t%u\n", sizeof(unsigned short), maxU);
	}

	{
		// int, __int32
		int min = INT_MIN;
		int max = INT_MAX;
		unsigned int maxU = UINT_MAX;
		printf("int (__int32)\t\t%d\t\t%d\t\t%d\n", sizeof(int), min, max);
		printf("unsigned int\t\t%d\t\t%u\t\t\t%u\n", sizeof(unsigned int), 0, maxU);
	}

	{
		// long, __int32 (same as int)
		long min = LONG_MIN;
		long max = LONG_MAX;
		unsigned long maxU = ULONG_MAX;
		printf("long (__int32)\t\t%d\t\t%ld\t\t%ld\n", sizeof(long), min, max);
		printf("unsigned long\t\t%d\t\t0\t\t\t%lu\n", sizeof(unsigned long), maxU);
	}

	{
		// long long, __int64

		// Note: VS2003 does not format __int64 variables correctly according to the C99 standard.
		// Use the Microsoft-specific "%I64" format specifier. For later versions or to make this
		// code compatible with other compilers (eg. on Linux) use "%ll".

		// <inttypes.h> is defined from C++11 and defines two macros, PRIu32 and PRIu64. These can
		// be used like this: printf("Minimum number is: %" PRIu64 "!\n", min);
		long long min = LLONG_MIN;
		long long max = LLONG_MAX;
		unsigned long long maxU = ULLONG_MAX;
		printf("long long (__int64)\t%d\t\t%I64d\t%I64d\n", sizeof(long long), min, max);
		printf("unsigned long long\t%d\t\t0\t\t\t%I64u\n", sizeof(unsigned long long), maxU);
	}
	printf("\n");

	printf("### Floating point ###\n");
	printf("\t\tSize (bytes)\tZero\t\t\tMin\t\t\tMax\n");
	{
		// float
		float zero = FLT_MIN;
		float min = -FLT_MAX;
		float max = FLT_MAX;
		printf("float\t\t%d\t\t%.10e\t%.10e\t%.10e\n", sizeof(float), zero, min, max);
	}

	{
		// double, long double
		double zero = DBL_MIN;
		double min = -DBL_MAX;
		double max = DBL_MAX;
		printf("double\t\t%d\t\t%.10e\t%.10e\t%.10e\n", sizeof(double), zero, min, max);
	}
	printf("\n");

	// Prompt for exit
	printf("Finished...press a key to exit\n");
	_getch();
}
