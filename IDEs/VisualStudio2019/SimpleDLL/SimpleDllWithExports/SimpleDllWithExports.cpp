// SimpleDllWithExports.cpp : Defines the exported functions for the DLL
#include "pch.h"
#include "framework.h"
#include "SimpleDllWithExports.h"

// To output debug messages...
#ifdef _DEBUG
	#include <iostream>
#endif

// Example: Exported class (note that "SimpleHello_Class" has been declared inline)
CSimpleDllWithExports::CSimpleDllWithExports()
{
	// Constructor
	m_nData = 3;
	m_bDisplayMsg = false;

#ifdef _DEBUG
		// Note: Prints out a message only in _DEBUG mode and if the client is a console app
		std::cout << "      !! CSimpleDllWithExports constructor !!\n";
#endif
	return;
}

CSimpleDllWithExports:: CSimpleDllWithExports(bool bDisplayMsg)
{
	// Constructor (with parameter)
	m_nData = 7;
	m_bDisplayMsg = bDisplayMsg;

#ifdef _DEBUG
	// Note: Prints out a message only in _DEBUG mode and if the client is a console app
	std::cout << "      !! CSimpleDllWithExports constructor !!\n";

	if (m_bDisplayMsg)
	{
		TCHAR szMsg[50];
		wsprintf(&szMsg[0], L"CSimpleDllWithExports constructor");
		MessageBox(NULL, (LPCTSTR)szMsg, L"Developer Test", MB_OK | MB_ICONINFORMATION);
	}
#endif
	return;
}

#ifdef _DEBUG
CSimpleDllWithExports::~CSimpleDllWithExports(void)
{
	std::cout << "      !! CSimpleDllWithExports destructor !!\n";
	if (m_bDisplayMsg)
	{
		TCHAR szMsg[50];
		wsprintf(&szMsg[0], L"CSimpleDllWithExports destructor");
		MessageBox(NULL, (LPCTSTR)szMsg, L"Developer Test", MB_OK | MB_ICONINFORMATION);
	}
}
#endif

int CSimpleDllWithExports::SimpleReturn_Class(void)
{
	return m_nData;
}

float CSimpleDllWithExports::SimpleMultiply_Class(const float a, const char b)
{
	return (a * (float)b);
}

// Example: Export to create CSimpleDllWithExports objects
CSimpleDllWithExports* CreateClass()
{
	return new CSimpleDllWithExports();
}

CSimpleDllWithExports* CreateClassMsg(bool bDisplayMsg)
{
	return new CSimpleDllWithExports(bDisplayMsg);
}

// Example: Export to destroy CSimpleDllWithExports objects
void DestroyClass(CSimpleDllWithExports* pC)
{
	delete pC;
	pC = NULL;
};

// Example: Exported data
int SIMPLEDLLWITHEXPORTS_CLASS_API nSimpleReturn_Data = 19;

// Example: Exported function
int SIMPLEDLLWITHEXPORTS_API SimpleReturn_NonClass(void)
{
	return 23;
}
