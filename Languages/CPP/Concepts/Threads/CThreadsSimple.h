#pragma once
#include <windows.h>
#include <vector>
 
enum class ThreadsSimple
{
    Example1 = 0,		// CreateThread
    Example2,			// _beginthread (simple example)
    Example3,			// _beginthread (example from MSDN)
		// See: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/beginthread-beginthreadex?view=vs-2019)
	Example4,			// _beginthreadex (example from MSDN)
	ExamplesEnd
};

// Class for demonstrating threads in C++
class CThreadsSimple
{
public:
	// Constructor / Destructor
    CThreadsSimple();
    ~CThreadsSimple();

	// Thread examples
	void StartThread(const ThreadsSimple ex);
	void StopThread(const ThreadsSimple ex);

private:
	// Member data
	std::vector<ThreadsSimple> m_threads;
	std::vector<uint32_t> m_threadRunning;
	uint32_t m_threadCounter;

	// Thread handles
	HANDLE	m_hThreadEx1;
	HANDLE	m_hThreadEx2;
	HANDLE	m_hThreadEx3_CheckKey;
	HANDLE	m_hThreadEx4;

	// Methods
	void Ex1_Run();
	void Ex2_Run();
	void Ex3_Run();
	void Ex4_Run();

	// Static methods (used for thread functions)
	static DWORD Ex1_ThreadFunc(LPVOID lParam);
};

