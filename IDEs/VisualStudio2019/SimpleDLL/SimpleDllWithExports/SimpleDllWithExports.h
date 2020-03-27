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

// One technique described here:
// https://social.msdn.microsoft.com/Forums/vstudio/en-US/9b39ab21-91ac-488c-9001-13de51566497/calling-a-c-class-constructor-from-a-dll
// is to define an interface with pure virtual functions only:
//	struct InterfaceWithExports
//	{
//		virtual std::string SimpleHello_Class(void) = 0;
//		virtual int SimpleReturn_Class(void) = 0;
//		virtual float SimpleMultiply_Class(const float a, const char b) = 0;
//	};

// The class derives from this interface and you export this interface to clients. When tested,
// this resulted in link errors in the client application.

// This class is exported from the dll
class SIMPLEDLLWITHEXPORTS_CLASS_API CSimpleDllWithExports
{
	public:
		CSimpleDllWithExports(void);
		CSimpleDllWithExports(bool bDisplayMsg);
#ifdef _DEBUG
		~CSimpleDllWithExports(void);
#endif

		inline std::string SimpleHello_Class(void) { return "Hello from CSimpleDllWithExports"; }
		int SimpleReturn_Class(void);
		float SimpleMultiply_Class(const float a, const char b);

	private:
		int m_nData;
		bool m_bDisplayMsg;
};

// Declare functions exported from this DLL to have C linkage. This allows these functions to be
// used in C-language and Delphi modules. Note that this does not affect exported classes.
#ifdef __cplusplus
extern "C" {
#endif

// Exported methods to allow use of "CSimpleDllWithExports" objects
// Note: Used with LoadLibrary and GetProcAddress. This is apparently the way COM works, see the
// link above and the "InterfaceWithExports" structure.
SIMPLEDLLWITHEXPORTS_CLASS_API CSimpleDllWithExports* CreateClass();
SIMPLEDLLWITHEXPORTS_CLASS_API CSimpleDllWithExports* CreateClassMsg(bool bDisplayMsg);
void SIMPLEDLLWITHEXPORTS_CLASS_API DestroyClass(CSimpleDllWithExports* pC);

// Exported data and a simple function
// Note: Using a calling convention modifier (eg. "__stdcall") on exported data is ignored by the
// compiler and results in warning C4229
extern int SIMPLEDLLWITHEXPORTS_CLASS_API nSimpleReturn_Data;
int SIMPLEDLLWITHEXPORTS_API SimpleReturn_NonClass(void);

#ifdef __cplusplus
}
#endif
