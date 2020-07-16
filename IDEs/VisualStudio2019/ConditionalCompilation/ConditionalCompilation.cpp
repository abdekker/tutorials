#include <iostream>
#include <conio.h>

// Helper function
void UpdateRow(const std::string cszSymbol, const bool cbDefined, const std::string szExtra)
{
	std::cout << "  " << cszSymbol << "...";
	if (cbDefined)
		std::cout << "defined!";
	else
		std::cout << "NOT defined";

	if (!szExtra.empty())
		std::cout << szExtra;

	std::cout << std::endl;
}

void Demonstrate_if()
{
	// Define a simple symbol and use #if / #ifdef
	// Note: Preprocessor commands must be the first non-whitespace on a line. Consequently, it is
	// typical for #if/#else/#endif to cover several lines.
	bool bIsDefined;

	std::cout << "=== #if and #ifdef: Define a symbol ===\n";
#define SYMBOL1

	bIsDefined =
#if defined(SYMBOL1)
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL1", bIsDefined, " [ #if defined(X) ]");

	bIsDefined =
#if !defined(SYMBOL1)
		false;
#else
		true;
#endif
	UpdateRow("SYMBOL1", bIsDefined, " [ #if !defined(X) ]");

	bIsDefined =
#ifdef SYMBOL1
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL1", bIsDefined, " [ #ifdef X ]");

	bIsDefined =
#ifndef SYMBOL1
		false;
#else
		true;
#endif
	UpdateRow("SYMBOL1", bIsDefined, " [ #ifndef X ]");

	// Undefine the symbol and repeat
	std::cout << "\nUndefine the symbol\n";
#if defined(SYMBOL1)
    #undef SYMBOL1
#endif

	bIsDefined =
#if defined(SYMBOL1)
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL1", bIsDefined, "");

    // Other differences between #ifdef and #if:
    // * #ifdef checks whether a macro by that name has been defined
    // * #if evaluates the expression and checks for a true value
    // To make use of this distinction, assign a value to the symbol
#if defined(SYMBOL1)
	#undef SYMBOL1
#endif
#if defined(SYMBOL2)
	#undef SYMBOL2
#endif

#define SYMBOL1 1
#define SYMBOL2 0

    std::cout << "\nUsing values, for example #define SYMBOL X, shows another difference between #ifdef and #if\n";
    std::cout << "SYMBOL1 is \"1\" and SYMBOL2 is \"0\"\n";
	bIsDefined =
#ifdef SYMBOL1
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL1", bIsDefined, "    [ #ifdef X ]");

    bIsDefined =
#ifdef SYMBOL2
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL2", bIsDefined, "    [ #ifdef X ]");

    bIsDefined =
#if SYMBOL1
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL1", bIsDefined, "    [ #if X ]");

    	bIsDefined =
#if SYMBOL2
		true;
#else
		false;
#endif
	UpdateRow("SYMBOL2", bIsDefined, " [ #if X ]");
}

void Demonstrate_if_logic()
{
	// Demonstrate some simple logic with #if
	std::cout << "\n=== #if: Logical operators ===\n";
	std::string szSymbolName;
	bool bIsDefined;

	// None defined
	std::cout << "Neither symbol defined\n";
#if defined(SYMBOL1)
	#undef SYMBOL1
#endif
#if defined(SYMBOL2)
	#undef SYMBOL2
#endif

	szSymbolName = "SYMBOL1 or SYMBOL2";
	bIsDefined =
#if defined(SYMBOL1) || defined(SYMBOL2)
		true;
#else
		false;
#endif
	UpdateRow(szSymbolName, bIsDefined, "");

	szSymbolName = "SYMBOL1 and SYMBOL2";
	bIsDefined =
#if defined(SYMBOL1) && defined(SYMBOL2)
		true;
#else
		false;
#endif
	UpdateRow(szSymbolName, bIsDefined, "");

	// One symbol defined
	std::cout << "\nOnly one symbol defined\n";
#define SYMBOL1

	szSymbolName = "SYMBOL1 or SYMBOL2";
	bIsDefined =
#if defined(SYMBOL1) || defined(SYMBOL2)
		true;
#else
		false;
#endif
	UpdateRow(szSymbolName, bIsDefined, "");

	szSymbolName = "SYMBOL1 and SYMBOL2";
	bIsDefined =
#if defined(SYMBOL1) && defined(SYMBOL2)
		true;
#else
		false;
#endif
	UpdateRow(szSymbolName, bIsDefined, "");

	// Both symbols defined
	std::cout << "\nBoth symbols defined\n";
#define SYMBOL2

	szSymbolName = "SYMBOL1 or SYMBOL2";
	bIsDefined =
#if defined(SYMBOL1) || defined(SYMBOL2)
		true;
#else
		false;
#endif
	UpdateRow(szSymbolName, bIsDefined, "");

	szSymbolName = "SYMBOL1 and SYMBOL2";
	bIsDefined =
#if defined(SYMBOL1) && defined(SYMBOL2)
		true;
#else
		false;
#endif
	UpdateRow(szSymbolName, bIsDefined, "");
}

void Demonstrate_elif()
{
	// Use of #elif and nested preprocessor commands
	std::cout << "\n=== #elif: Use _MSC_VER (" << _MSC_VER << ") to demonstrate nested commands ===\n";
	// Note: _MSC_VER encodes the compiler version. Here's an (incomplete) list:
	// * Visual Studio 6.0					1200
	// * Visual Studio .NET 2003 (7.1)		1310
	// * Visual Studio 2005 (8.0)			1400
	// * Visual Studio 2008 (9.0)			1500
	// * Visual Studio 2013 (12.0)			1800
	// * Visual Studio 2015 (14.0)			1900
	// * Visual Studio 2017 version 15.3	1911
	// * Visual Studio 2017 version 15.9	1916
	// * Visual Studio 2019 version 16.1	1921
	// * Visual Studio 2019 version 16.4	1924
	// * Visual Studio 2019 version 16.6	1926
#if (_MSC_VER <= 1310)
	std::cout << "  Visual Studio 2003 or earlier\n";
#elif (_MSC_VER <= 1900)
	std::cout << "  Visual Studio 2015 or earlier\n";
#else
	#if (_MSC_VER < 1920)
		std::cout << "  Visual Studio 2017 or earlier\n";
	#else
		std::cout << "  Visual Studio 2019 or later\n";
	#endif
#endif
}

void Demonstrate_pragma()
{
	// The #pragma directive is powerful, but specific to Microsoft (and therefore not portable)
	std::cout << "\n=== #pragma: Use #pragma to output a message during compilation ===\n";
	std::cout << "Useful to see what is happening (say with #if). Alternatively use #error, though\n";
	std::cout << "this will terminate the compilation.\n";
#pragma message("  Time: " __TIME__)
#pragma message("  Compiling: " __FILE__)
#pragma message("  Last modified on: " __TIMESTAMP__)
    std::cout << "Note: #pragma output only appears during compilation and not when running the application!\n";

	// Uncomment the following section to output a compiler message and termine compilation
/*#if defined(RANDOM_SYMBOL)
	// Do something
#else
	#error "RANDOM_SYMBOL is not defined! Panic now...";
#endif*/
}

void Demonstrate_predefinedMacros()
{
	// Standard pre-defined macros (specified by the ISO C99 and ISO C++17 standards)
	// See: https://docs.microsoft.com/en-us/cpp/preprocessor/predefined-macros
	std::cout << "\n=== Standard compiler pre-defined macros ===\n";
	std::cout << "  __cplusplus = " << __cplusplus << std::endl;
	std::cout << "  __TIME__ = " << __TIME__ << std::endl;
	std::cout << "  __DATE__ = " << __DATE__ << std::endl;
	std::cout << "  __FILE__ = " << __FILE__ << std::endl;
	std::cout << "  __LINE__ = " << __LINE__ << std::endl;
#if defined(__STDC__)
	std::cout << "  __STDC__ = " << __STDC__ << std::endl;
#else
	std::cout << "  __STDC__ is not defined" << std::endl;
#endif
	std::cout << "  __STDCPP_THREADS__ = " << __STDCPP_THREADS__ << std::endl;

	// Microsoft-specific predefined macros (selection)
	std::cout << "\n=== Microsoft-specific compiler pre-defined macros ===\n";
#if defined(__CLR_VER)
	std::cout << "  __CLR_VER = " << __CLR_VER << std::endl;
#else
	std::cout << "  __CLR_VER is not defined (requires the /clr compiler option)" << std::endl;
#endif

	std::cout << "  __COUNTER__ = " << __COUNTER__ << std::endl;
	std::cout << "  __COUNTER__ = " << __COUNTER__ << " (increments each time it is referenced)" << std::endl;
#if defined(_DEBUG)
	std::cout << "  _DEBUG is defined\n";
#elif defined(NDEBUG)
	std::cout << "  NDEBUG is defined\n";
#else
	std::cout << "  Neither _DEBUG nor NDEBUG is defined\n";
#endif

#if defined(_DLL)
	std::cout << "  _DLL = " << _DLL << std::endl;
#else
	std::cout << "  _DLL is not defined\n";
#endif

	std::cout << "  __FUNCDNAME__ = " << __FUNCDNAME__ << std::endl;
	std::cout << "  __FUNCSIG__ = " << __FUNCSIG__ << std::endl;
#if defined(_M_IX86)
	std::cout << "  _M_IX86 = " << _M_IX86 << std::endl;
#else
	std::cout << "  _M_IX86 is not defined\n";
#endif

	std::cout << "  _MSC_BUILD = " << _MSC_BUILD << std::endl;
	std::cout << "  _MSC_FULL_VER = " << _MSC_FULL_VER << std::endl;
	std::cout << "  _MSC_VER = " << _MSC_VER << std::endl;
#if defined(__cplusplus) && defined(_MSVC_LANG)
	std::cout << "  _MSVC_LANG = " << _MSVC_LANG << std::endl;
#endif

#if defined(_OPENMP)
	std::cout << "  _OPENMP = " << _OPENMP << std::endl;
#else
	std::cout << "  _OPENMP is not defined\n";
#endif

	std::cout << "  __TIMESTAMP__ = " << __TIMESTAMP__ << std::endl;

#if defined(_WIN32)
	std::cout << "  _WIN32 = " << _WIN32 << std::endl;
#else
	std::cout << "  _WIN32 is not defined\n";
#endif

#if defined(_WIN64)
	std::cout << "  _WIN64 = " << _WIN64 << std::endl;
#else
	std::cout << "  _WIN64 is not defined\n";
#endif
}

void Demonstrate_line()
{
	// Use #line to modify __LINE__ and __FILE__ (pre-defined macros)
	std::cout << "\n=== #line: Change __LINE__ and __FILE__ ===\n";
#define ASSERT(cond) if (!(cond))\
	{ printf( "Assertion error!\n  Line: %d\n  File: %s\n", \
		__LINE__, __FILE__ ); }
	ASSERT(false);
#line 999 "spider.man"
	ASSERT(false);
}

int main()
{
	// Start with a friendly message
	std::cout << "Demonstrates the preprocessor and conditional compilation in Visual Studio 2019\n\n";
	Demonstrate_if();
	Demonstrate_if_logic();
	Demonstrate_elif();
	Demonstrate_pragma();
	Demonstrate_predefinedMacros();
	Demonstrate_line();

	 // Prompt for exit
    std::cout << "\nFinished...press a key to exit\n";
    (void) _getch();
}
