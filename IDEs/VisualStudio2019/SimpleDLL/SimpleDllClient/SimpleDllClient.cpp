// SimpleDllClient.cpp : Client app for a simple C++ DLL. This file contains the 'main' function.
// Program execution begins and ends there.
#include "windows.h"
#include <iostream>
#include <conio.h>
#include "SimpleDLL.h"
#include "SimpleDllWithExports.h"

// DLL names that this test application will consume
#define SIMPLE_DLL					"SimpleDLL.dll"
#define SIMPLE_DLL_WITH_EXPLORTS	"SimpleDllWithExports.dll"
#define SIMPLE_DLL_MFC				"SimpleDllMFC.dll"

#ifdef _DEBUG
	// Uncomment this line to pop up a message in DEBUG mode (generated by some of the DLLs)
	//#define DBG_SHOW_POPUP_MSGS_CLIENT
#endif

// Function definitions (for SimpleDll.dll)
typedef unsigned int (CALLBACK* LPFNDLLFUNC_SimpleReturn)();
typedef int (CALLBACK* LPFNDLLFUNC_SimpleSum)(const int, const int);
typedef float (CALLBACK* LPFNDLLFUNC_SimpleMultiply)(const float, const char);

// Function definitions (for SimpleDllWithExports.dll)
typedef CSimpleDllWithExports* (*LPFNDLLFUNC_CreateClass)();
typedef CSimpleDllWithExports* (*LPFNDLLFUNC_CreateClassMsg)(bool);
typedef void (*LPFNDLLFUNC_DestroyClass)(CSimpleDllWithExports*);

// Function definitions (for SimpleDllMFC.dll)
// Just one function prototype
//		unsigned int __declspec(dllexport) __stdcall Function();
// Do re-use "LPFNDLLFUNC_SimpleReturn"
typedef unsigned int (CALLBACK* LPFNDLLFUNC_SimpleReturnMFC)();

// Local function prototypes
void SimpleDLL_Test_Static();
void SimpleDLL_Test_Manual();
void SimpleDLL_Fibonacci();

void SimpleDllWithExports_Static();
void SimpleDllWithExports_Manual();

void SimpleDllMFC_Manual();

int main()
{
	// Start with a friendly message
	std::cout << "Hello from a simple DLL client (written in VS 2019)!\n\n";

	std::cout << "This application will consume functions from C++ DLLs:\n";
	std::cout << "  1) Written using the \"Dynamic-Link Library (DLL)\" wizard\n";
	std::cout << "\tContains simple \"extern \"C\"\" functions\n";
	std::cout << "  2) Written using the \"Dynamic-Link Library with exports (DLL)\" wizard\n";
	std::cout << "\tContains data, functions and a C++ class\n\n";

	std::cout << "Note: DLLs consumed should be in the path or folder of the consuming application\n\n";

	// ### SimpleDLL ###
	{
		std::cout << "### SimpleDLL ###\n";
		SimpleDLL_Test_Static();
		SimpleDLL_Test_Manual();
		SimpleDLL_Fibonacci();
	}

    // ### SimpleDllWithExports ###
	{
		std::cout << "### SimpleDllWithExports ###\n";
		SimpleDllWithExports_Static();
		SimpleDllWithExports_Manual();
	}

	// ### SimpleDllMFC ###
	{
		std::cout << "### SimpleDllMFC ###\n";
		SimpleDllMFC_Manual();
	}

    // Prompt for exit
    std::cout << "\nFinished...press a key to exit\n";
    (void) _getch();
}

void SimpleDLL_Test_Static()
{
	// SimpleDLL.dll::Test functions (prototypes defined in "SimpleDLL.h")
	std::cout << SIMPLE_DLL << ", test functions: Defined from header\n";
	std::cout << "  SimpleReturn()\t\t= " << SimpleReturn() << "\n";
	std::cout << "  SimpleSum(4, -11)\t\t= " << SimpleSum(4, -11) << "\n";
	std::cout << "  SimpleMultiply(3.7f, 12)\t= " << SimpleMultiply(3.7f, 12) << "\n\n";
}

void SimpleDLL_Test_Manual()
{
	// SimpleDLL.dll::Test functions (function pointers extracted with LoadLibrary / GetProcAddress)
	std::cout << SIMPLE_DLL << ", test functions: Extracted with LoadLibrary and GetProcAddress\n";

	// Note: By default, VS creates DLL projects with the UNICODE symbol defined. This means
	// "LoadLibrary" is an alias for "LoadLibraryW". Use either of these:
	//		* LoadLibraryA(SIMPLE_DLL);		// Using the "#define SIMPLE_DLL "DLL_NAME""
	//		* LoadLibrary(L"DLL_NAME");
	HINSTANCE hDLL = LoadLibraryA(SIMPLE_DLL);
	if (NULL != hDLL)
	{
		// Extract functions
		// Note: These prototypes assume the calling convention in SimpleDLL.dll is "__stdcall". If this
		// is changed to "__decl", or a definition (.def) file is used, function names would change to
		// "SimpleReturn", "SimpleSum", and "SimpleMultiply".
		LPFNDLLFUNC_SimpleReturn lpFn1 = (LPFNDLLFUNC_SimpleReturn)GetProcAddress(hDLL, "_SimpleReturn@0");
		LPFNDLLFUNC_SimpleSum lpFn2 = (LPFNDLLFUNC_SimpleSum)GetProcAddress(hDLL, "_SimpleSum@8");
		LPFNDLLFUNC_SimpleMultiply lpFn3 = (LPFNDLLFUNC_SimpleMultiply)GetProcAddress(hDLL, "_SimpleMultiply@8");
		if ((NULL != lpFn1) &&
			(NULL != lpFn2) &&
			(NULL != lpFn3))
		{
			// Call the functions
			std::cout << "  SimpleReturn()\t\t= " << lpFn1() << "\n";
			std::cout << "  SimpleSum(4, -11)\t\t= " << lpFn2(4, -11) << "\n";
			std::cout << "  SimpleMultiply(3.7f, 12)\t= " << lpFn3(3.7f, 12) << "\n\n";
		}
		else
			std::cout << "  Error: Unable to extract function pointers\n\n";

		// Unload library
		FreeLibrary(hDLL);
	}
	else
		std::cout << "  Error: Unable to load " << SIMPLE_DLL << "\n\n";
}

void SimpleDLL_Fibonacci()
{
	// Initialize a 64-bit Fibonacci relation sequence (prototypes defined in "SimpleDLL.h")
	// Note: There is no manual version of this function (using LoadLibrary / GetProcAddress)
	std::cout << "Fibonacci sequence (64-bit)\n";
	fibonacciInit64(1, 1);

	// Write out the sequence values until overflow
	do {
		std::cout << fibonacciIndex() << ": " << fibonacciCurrent64() << std::endl;
	} while (fibonacciNext64());

	// Report count of values written before overflow
	std::cout << fibonacciIndex() + 1 <<
		" Fibonacci sequence values fit in an unsigned 64-bit integer.\n\n";

	// Repeat with 32-bit
	std::cout << "Fibonacci sequence (32-bit)\n";
	fibonacciInit32(1, 1);
	do {
		std::cout << fibonacciIndex() << ": " << fibonacciCurrent32() << std::endl;
	} while (fibonacciNext32());

	std::cout << fibonacciIndex() + 1 <<
		" Fibonacci sequence values fit in an unsigned 32-bit integer.\n\n";
}

void SimpleDllWithExports_Static()
{
	// SimpleDllWithExports.dll::Test functions (prototypes defined in "SimpleDllWithExports.h")
	std::cout << SIMPLE_DLL_WITH_EXPLORTS << ", test functions: Defined from header\n";
	{
		std::cout << "  nSimpleReturn_Data\t\t= " << nSimpleReturn_Data << "\n";
		std::cout << "  SimpleReturn_NonClass()\t= " << SimpleReturn_NonClass() << "\n\n";
	}

	{
		// Use the exported class
		// Note,1: VS 2019 Intellisense does not find "SimpleHello_Class" which has been declared inline
		// Note,2: If this DLL is built in debug mode, a console message is displayed when the class is
		//		constructed and then destroyed 
		std::cout << "  exportedClass (no parameter)\n";
		CSimpleDllWithExports exportedClass;
		std::cout << "    SimpleHello_Class()\t\t\t= " << exportedClass.SimpleHello_Class() << "\n";
		std::cout << "    SimpleReturn_Class()\t\t= " << exportedClass.SimpleReturn_Class() << "\n";
		std::cout << "    SimpleMultiply_Class(3.7f, 12)\t= " <<
			exportedClass.SimpleMultiply_Class(3.7f, 12) << "\n";
	}
	std::cout << std::endl;

#ifdef DBG_SHOW_POPUP_MSGS_CLIENT
	{
		// Use the exported class which takes a parameter to display a popup message
		std::cout << "  exportedClass (with parameter)\n";
		CSimpleDllWithExports exportedClass(true);
		std::cout << "    SimpleHello_Class()\t\t\t= " << exportedClass.SimpleHello_Class() << "\n";
	}
	std::cout << std::endl;
#endif
}

void SimpleDllWithExports_Manual()
{
	// SimpleDllWithExports.dll::Test functions (function pointers extracted with LoadLibrary / GetProcAddress)
	std::cout << SIMPLE_DLL_WITH_EXPLORTS << ", test functions: Extracted with LoadLibrary and GetProcAddress\n";

	HINSTANCE hDLL = LoadLibraryA(SIMPLE_DLL_WITH_EXPLORTS);
	if (NULL != hDLL)
	{
		// Extract functions to create a new class
		LPFNDLLFUNC_CreateClass lpFnConstructor = (LPFNDLLFUNC_CreateClass)GetProcAddress(hDLL, "CreateClass");
		LPFNDLLFUNC_DestroyClass lpFnDestructor = (LPFNDLLFUNC_DestroyClass)GetProcAddress(hDLL, "DestroyClass");
		if ((NULL != lpFnConstructor) &&
			(NULL != lpFnDestructor))
		{
			// Use the exported class
			// Note: It is generally a bad idea to export C++ classes...
			std::cout << "  exportedClass (no parameter)\n";
			CSimpleDllWithExports* pExportedClass = lpFnConstructor();
			std::cout << "    SimpleHello_Class()\t\t\t= " << pExportedClass->SimpleHello_Class() << "\n";
			std::cout << "    SimpleReturn_Class()\t\t= " << pExportedClass->SimpleReturn_Class() << "\n";
			std::cout << "    SimpleMultiply_Class(3.7f, 12)\t= " << pExportedClass->SimpleMultiply_Class(3.7f, 12) << "\n";
			lpFnDestructor(pExportedClass);
		}
		else
			std::cout << "  Unable to extract function pointers\n";

#ifdef DBG_SHOW_POPUP_MSGS_CLIENT
		std::cout << std::endl;

		// Now ceate a class which pops up a message (in DEBUG mode only)
		LPFNDLLFUNC_CreateClassMsg lpFnConstructorMsg = (LPFNDLLFUNC_CreateClassMsg)GetProcAddress(hDLL, "CreateClassMsg");
		if ((NULL != lpFnConstructorMsg) &&
			(NULL != lpFnDestructor))
		{
			// Use the exported class
			// Note: It is generally a bad idea to export C++ classes...
			std::cout << "  exportedClass (with parameter)\n";
			CSimpleDllWithExports* pExportedClass = lpFnConstructorMsg(true);
			std::cout << "    SimpleHello_Class()\t\t\t= " << pExportedClass->SimpleHello_Class() << "\n";
			lpFnDestructor(pExportedClass);
		}
		else
			std::cout << "  Unable to extract function pointers\n";
#endif

		// Unload library
		FreeLibrary(hDLL);
	}
	std::cout << std::endl;
}

void SimpleDllMFC_Manual()
{
	// SimpleDllMFC.dll::Test functions (function pointers extracted with LoadLibrary / GetProcAddress)
	// Note: There is no static version of this method
	std::cout << SIMPLE_DLL_MFC << ", test functions: Extracted with LoadLibrary and GetProcAddress\n";

	HINSTANCE hDLL = LoadLibraryA(SIMPLE_DLL_MFC);
	if (NULL != hDLL)
	{
		// Extract functions
		LPFNDLLFUNC_SimpleReturnMFC lpFn1 = (LPFNDLLFUNC_SimpleReturnMFC)GetProcAddress(hDLL, "SimpleReturn1_OutsideClass");
		LPFNDLLFUNC_SimpleReturnMFC lpFn2 = (LPFNDLLFUNC_SimpleReturnMFC)GetProcAddress(hDLL, "SimpleReturn2_OutsideClass");
		if ((NULL != lpFn1) &&
			(NULL != lpFn2))
		{
			// Use the extracted functions
			std::cout << "    SimpleReturn1_OutsideClass()\t= " << lpFn1() << "\n";
			std::cout << "    SimpleReturn2_OutsideClass()\t= " << lpFn2() << "\n";
		}
		else
			std::cout << "  Unable to extract function pointers\n";

		// Unload library
		FreeLibrary(hDLL);
	}
	else
		std::cout << "  Error: Unable to load " << SIMPLE_DLL_MFC << "\n\n";
}

