#pragma once
#include <iostream>

// See the SimpleDLL project for notes about calling conventions
#ifdef SIMPLEDLLWITHEXPORTS_EXPORTS
#define SIMPLEDLLWITHEXPORTS_CLASS_API __declspec(dllexport)
//#define SIMPLEDLLWITHEXPORTS_API __declspec(dllexport) __cdecl
#define SIMPLEDLLWITHEXPORTS_API __declspec(dllexport) __stdcall
#else
#define SIMPLEDLLWITHEXPORTS_CLASS_API __declspec(dllimport)
//#define SIMPLEDLLWITHEXPORTS_API __declspec(dllimport) __cdecl
#define SIMPLEDLLWITHEXPORTS_API __declspec(dllimport) __stdcall
#endif

// Declare functions exported from this DLL to have C linkage. This allows these functions to be
// used in C-language and Delphi modules. Note that this does not affect exported classes
#ifdef __cplusplus
extern "C" {
#endif

// This class is exported from the dll
class SIMPLEDLLWITHEXPORTS_CLASS_API CSimpleDllWithExports
{
    public:
	    CSimpleDllWithExports(void);

        inline std::string SimpleHello_Class(void) { return "Hello from CSimpleDllWithExports"; }
        int SimpleReturn_Class(void);
        float SimpleMultiply_Class(const float a, const char b);

    private:
        int m_nData;
};

// Exported data and a simple function
extern int SIMPLEDLLWITHEXPORTS_API nSimpleReturn_Data;
int SIMPLEDLLWITHEXPORTS_API SimpleReturn_NonClass(void);

#ifdef __cplusplus
}
#endif
