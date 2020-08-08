#include "CThreads.h"

#include <conio.h>		// _getch
#include <iostream>
#include <process.h>	// _beginthread, _endthread

// Macros
// GetRandom returns a random integer between min and max
#define GetRandom( min, max ) ((rand() % (int)(((max) + 1) - (min))) + (min))

// GetGlyph returns a printable ASCII character value
#define GetGlyph( val ) ((char)((val + 32) % 93 + 33))

// Global flags

// Example 1
static bool		g_bTerminateEx1 = false;
static bool		g_bFinishedEx1 = false;

// Example 2
static bool		g_bTerminateEx2 = false;
static bool		g_bFinishedEx2 = false;

// Example 3
bool g_bEx3Repeat = true;
HANDLE g_hStdOut;					// Handle for console window
CONSOLE_SCREEN_BUFFER_INFO g_csbi;	// Console information structure

// Forward declare thread functions
// Note: _beginthread requires thread functions be declared with "__cdecl" calling convention.
// This can be set explicitly eg. "void __cdecl func(void* );"
void Ex2_ThreadFunc(void* pData);
void Ex3_ThreadFunc_CheckKey(void*);
void Ex3_ThreadFunc_Bounce(void*);
unsigned int __stdcall Ex4_ThreadFunc(void* pData);

// Constructor / Destructor
CThreads::CThreads()
{
	std::cout << "  [CThreads constructor]\n";

	// Add a list of possible threads to a vector (so we can easily iterate over them)
	m_threads.push_back(ThreadExample::Example1);
	m_threads.push_back(ThreadExample::Example2);
	m_threads.push_back(ThreadExample::Example3);
	m_threads.push_back(ThreadExample::Example4);
	for (size_t thread = 0; thread < m_threads.size(); thread++)
		m_threadRunning.push_back(0);

	// Thread handles
	m_hThreadEx1 = nullptr;
	m_hThreadEx2 = nullptr;
	m_hThreadEx3_CheckKey = nullptr;
	m_hThreadEx4 = nullptr;
}

CThreads::~CThreads()
{
	std::cout << "  [CThreads destructor]\n";

	// Stop all threads
	std::cout << "  [CThreads destructor - stop all threads]\n";
	for (auto ex : m_threads)
	{
		StopThread(ex);
	}
}

// Thread examples
void CThreads::StartThread(const ThreadExample ex)
{
	// Start one of the threads
	if (m_threadRunning[(int)ex] > 0) {
		// Thread already running...
		std::cout << "  [Thread ex" << ((int)ex + 1) << " is already running]\n";
		return;
	}

	switch (ex)
	{
	case ThreadExample::Example1:
		Ex1_Run();
		break;
	case ThreadExample::Example2:
		Ex2_Run();
		break;
	case ThreadExample::Example3:
		Ex3_Run();
		break;
	case ThreadExample::Example4:
		Ex4_Run();
		break;
	}
}

void CThreads::StopThread(const ThreadExample ex)
{
	// Stop one of the threads
	if (m_threadRunning[(int)ex] == 0) {
		// Thread not running...
		std::cout << "  [Thread ex" << ((int)ex + 1) << " is not running]\n";
		return;
	}

	std::cout << "...terminating thread example " << ((int)ex + 1) << "\n";
	DWORD dwTimer;
	switch (ex)
    {
	case ThreadExample::Example1:
		// Terminate thread example 1
		g_bTerminateEx1 = true;
		if (!g_bFinishedEx1) {
			dwTimer = GetTickCount();
			while (!g_bFinishedEx1) {
				if ((GetTickCount() - dwTimer) > 1000)
					break;

				Sleep(20);
			}

			// Has the thread finished?
			if (!g_bFinishedEx1) {
				// Thread has timed out, so force kill it
				MessageBeep(-1);
				TerminateThread(m_hThreadEx1, 0);
				g_bFinishedEx1 = true;
			}
		}
		CloseHandle(m_hThreadEx1);
		m_threadRunning[(int)ThreadExample::Example1]--;
		std::cout << "...example 1 terminated, counter is " << m_threadCounter << "\n";
		break;

	case ThreadExample::Example2:
		// Terminate thread example 2
		g_bTerminateEx1 = true;
		if (!g_bFinishedEx1) {
			TerminateThread(m_hThreadEx2, 0);
			g_bFinishedEx2 = true;
		}
		CloseHandle(m_hThreadEx1);
		m_threadRunning[(int)ThreadExample::Example2]--;
		std::cout << "...example 2 terminated, counter is " << m_threadCounter << "\n";
		break;

	case ThreadExample::Example3:
	case ThreadExample::Example4:
		// Threads are stopped separately
		break;
	}
}

// Private methods
void CThreads::Ex1_Run()
{
	std::cout << "  Inside example 1: Using CreateThread\n";
	g_bTerminateEx1 = false;
	g_bFinishedEx1 = false;
	m_threadCounter = 0;
	DWORD dwThreadID = 0;
	if (NULL == (m_hThreadEx1 =
					CreateThread(
						(LPSECURITY_ATTRIBUTES) NULL,
						0,
						(LPTHREAD_START_ROUTINE) Ex1_ThreadFunc,
						&m_threadCounter,
						0, &dwThreadID)))
	{
		std::cout << "    Failed to create thread...\n";
	}
	else {
		// Set thread priority lower then normal
		SetThreadPriority(m_hThreadEx1, THREAD_PRIORITY_BELOW_NORMAL);
		m_threadRunning[(int)ThreadExample::Example1]++;
		std::cout << "    Successfully created thread with ID " << dwThreadID << "!\n";
	}
}

void CThreads::Ex2_Run()
{
	std::cout << "  Inside example 2: Using _beginthread\n";
	g_bTerminateEx2 = false;
	g_bFinishedEx2 = false;
	m_threadCounter = 0;
	m_hThreadEx2 = (HANDLE)_beginthread(&Ex2_ThreadFunc, 0, &m_threadCounter);
	if (m_hThreadEx2 == nullptr) {
		std::cout << "    Failed to create thread...\n";
	}
	else {
		// Set thread priority higher then normal
		SetThreadPriority(m_hThreadEx2, THREAD_PRIORITY_ABOVE_NORMAL);
		m_threadRunning[(int)ThreadExample::Example2]++;
		std::cout << "    Successfully created thread!\n";
	}
}

void CThreads::Ex3_Run()
{
	std::cout << "  Inside example 3: Using _beginthread (complex example)\n";

	// Get display screen's text row and column information.
	g_hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleScreenBufferInfo(g_hStdOut, &g_csbi);

	// Launch a separate thread to check for terminating keystroke
	m_hThreadEx3_CheckKey = (HANDLE)_beginthread(Ex3_ThreadFunc_CheckKey, 0, NULL);
	if (m_hThreadEx3_CheckKey == nullptr)
		std::cout << "    Failed to create thread...\n";
	else {
		m_threadRunning[(int)ThreadExample::Example3]++;
		std::cout << "    Successfully created thread!\n";
	}

	// Loop until user terminates threads (by pressing a key) or 1000 threads are created
	int threadNum = 0;
    int *pThreadNum = &threadNum;
    while (g_bEx3Repeat && threadNum < 1000)
    {
        // Launch another character thread
        _beginthread(Ex3_ThreadFunc_Bounce, 0, (void *) pThreadNum);

        // Increment the thread parameter
        pThreadNum++;

        // Wait one second between loops
        Sleep(1000L);
    }
}

void CThreads::Ex4_Run()
{
	std::cout << "  Inside example 4: Using _beginthreadex\n";
	uint32_t uThreadID; 
	m_threadCounter = 0;
	m_hThreadEx4 = (HANDLE)_beginthreadex(NULL, 0, &Ex4_ThreadFunc, &m_threadCounter, 0, &uThreadID);

    // Wait until second thread terminates. If you comment out the line below, the counter will
	// not be correct because the thread has not terminated, and counter most likely has not been
	// incremented to 1000000 yet.
    WaitForSingleObject(m_hThreadEx4, INFINITE);
	std::cout << "  Counter should be 1000000; it is-> " << m_threadCounter << "\n";

    // Destroy the thread object
    CloseHandle(m_hThreadEx4);
}

// Thread functions
DWORD CThreads::Ex1_ThreadFunc(LPVOID lParam)
{
	// Example 1 thread function
	uint32_t& localCounter = *((uint32_t*)lParam);
	while (!g_bTerminateEx1)
	{
		// Increment the local counter
		++localCounter;
		if (localCounter == 0xFFFFFFFF)
			localCounter = 0;

		if ((localCounter % 1000) == 0)
			std::cout << "\tEx1 (ID " << GetCurrentThreadId() << ") tick, counter is " << localCounter << "\n";

		// Minimum sleep...comment out to let the thread to run as fast as possible
		Sleep(1);
	}

	g_bFinishedEx1 = true;
	return 0;
}

void Ex2_ThreadFunc(void* pData)
{
	// Example 2 thread function
	uint32_t& localCounter = *((uint32_t*)pData);
	while (!g_bTerminateEx2)
	{
		// Increment the local counter
		++localCounter;
		if (localCounter == 0xFFFFFFFF)
			localCounter = 0;

		if ((localCounter % 1000) == 0)
			std::cout << "\tEx2 (ID " << GetCurrentThreadId() << ") tick, counter is " << localCounter << "\n";

		// Minimum sleep...comment out to let the thread to run as fast as possible
		Sleep(1);
	}

	g_bFinishedEx2 = true;
}

void Ex3_ThreadFunc_CheckKey(void*)
{
	// Thread to wait for a keystroke, then clear the global repeat flag
	std::cout << "  Example 3 thread to check for keypress (ID " << _threadid << ")\n";
	(void) _getch();
	g_bEx3Repeat = false;
	_endthread();
}

void Ex3_ThreadFunc_Bounce(void* pData)
{
	// Thread to create and control a colored letter that bounces around the screen
	// Parameters: pData is the value to create the character from
	char       blankcell = 0x20;
	CHAR_INFO  ci;
	COORD      oldcoord, cellsize, origin;
	DWORD      result;
	SMALL_RECT region;

	cellsize.X = cellsize.Y = 1;
	origin.X = origin.Y = 0;

	// Generate location, letter and color attribute from thread argument
	// Note: Set a minimum X co-ordinate otherwise we stomp over all the debugging text!
	SHORT minX = max(g_csbi.srWindow.Left, (g_csbi.srWindow.Right - 70));
	srand(_threadid);
	oldcoord.X = region.Left = region.Right = GetRandom(minX, g_csbi.srWindow.Right - 1);
	oldcoord.Y = region.Top = region.Bottom = GetRandom(g_csbi.srWindow.Top, g_csbi.srWindow.Bottom - 1);
	//ci.Char.AsciiChar = GetGlyph(*((int *)pData));
	ci.Char.AsciiChar = GetRandom('0', 'z');
	ci.Attributes = GetRandom(1, 15);

	while (g_bEx3Repeat)
	{
		// Pause between loops
		Sleep(100L);

		// Blank out our old position on the screen, and draw new letter
		WriteConsoleOutputCharacterA(g_hStdOut, &blankcell, 1, oldcoord, &result);
		WriteConsoleOutputA(g_hStdOut, &ci, cellsize, origin, &region);

		// Increment the coordinate for next placement of the block
		oldcoord.X = region.Left;
		oldcoord.Y = region.Top;
		region.Left = region.Right += GetRandom(-1, 1);
		region.Top = region.Bottom += GetRandom(-1, 1);

		// Correct placement (and beep) if about to go off the screen
		if (region.Left < minX)
			region.Left = region.Right = (minX + 1);
		else if (region.Right >= g_csbi.srWindow.Right)
			region.Left = region.Right = (g_csbi.srWindow.Right - 2);
		else if (region.Top < g_csbi.srWindow.Top)
			region.Top = region.Bottom = (g_csbi.srWindow.Top + 1);
		else if (region.Bottom >= g_csbi.srWindow.Bottom)
			region.Top = region.Bottom = (g_csbi.srWindow.Bottom - 2);

		// If not at a screen border, continue, otherwise beep
		else
			continue;
		Beep((ci.Char.AsciiChar - 'A') * 100, 175);
	}

	// Terminate thread
	_endthread();
}

unsigned int __stdcall Ex4_ThreadFunc(void* pData)
{
	uint32_t& localCounter = *((uint32_t*)pData);
	while (localCounter < 1000000)
		localCounter++;

	_endthreadex(0);
	return 0;
}
