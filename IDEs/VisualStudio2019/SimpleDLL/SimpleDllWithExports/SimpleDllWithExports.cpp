// SimpleDllWithExports.cpp : Defines the exported functions for the DLL
#include "pch.h"
#include "framework.h"
#include "SimpleDllWithExports.h"

// Example: Exported variable
int SIMPLEDLLWITHEXPORTS_API nSimpleReturn_Data = 19;

// Example: Exported function
int SIMPLEDLLWITHEXPORTS_API SimpleReturn_NonClass(void)
{
    return 23;
}

// Example: Exported class (note that "SimpleHello_Class" has been declared inline)
CSimpleDllWithExports::CSimpleDllWithExports()
{
    m_nData = 3;
    return;
}

int CSimpleDllWithExports::SimpleReturn_Class(void)
{
    return m_nData;
}

float CSimpleDllWithExports::SimpleMultiply_Class(const float a, const char b)
{
    return (a * (float)b);
}
