// SimpleDllMFC.h : main header file for the SimpleDllMFC DLL
#pragma once

#ifndef __AFXWIN_H__
	#error "include 'pch.h' before including this file for PCH"
#endif

#include "resource.h" // main symbols

// See the SimpleDLL project for notes about calling conventions
#ifdef SIMPLEDLLMFC_EXPORTS
#define SIMPLEDLLMFC_CLASS_API __declspec(dllexport)
//#define SIMPLEDLLMFC_API __declspec(dllexport) __cdecl
#define SIMPLEDLLMFC_API __declspec(dllexport) __stdcall
#else
#define SIMPLEDLLMFC_CLASS_API __declspec(dllimport)
//#define SIMPLEDLLMFC_API __declspec(dllimport) __cdecl
#define SIMPLEDLLMFC_API __declspec(dllimport) __stdcall
#endif

#ifdef __cplusplus
extern "C" {
#endif

class CSimpleDllMFCApp : public CWinApp
{
public:
    CSimpleDllMFCApp();
#ifdef _DEBUG
    ~CSimpleDllMFCApp();
#endif

// Overrides
public:
    virtual BOOL InitInstance();
    DECLARE_MESSAGE_MAP()

public:
	unsigned int SimpleReturn_InsideClass();
};

// Function prototypes outside the class
unsigned int SIMPLEDLLMFC_API SimpleReturn1_OutsideClass();
unsigned int SIMPLEDLLMFC_API SimpleReturn2_OutsideClass();

#ifdef __cplusplus
}
#endif
