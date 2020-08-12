#include "CThreadsSimple.h"

#include <conio.h>		// _getch
#include <iostream>
#include <process.h>	// _beginthread, _endthread

// Macros
// GetRandom returns a random integer between min and max
#define GetRandom( min, max ) ((rand() % (int)(((max) + 1) - (min))) + (min))

// GetGlyph returns a printable ASCII character value
#define GetGlyph( val ) ((char)((val + 32) % 93 + 33))

// Global flags
CRITICAL_SECTION m_hCritSect;		// Synchronisation object

// Example 1
static bool		g_bTerminateEx1 = false;
static bool		g_bFinishedEx1 = false;

// Example 2
static bool		g_bTerminateEx2 = false;
static bool		g_bFinishedEx2 = false;

// Example 3
#define MAX_BOUNCE_THREADS 200
bool g_bEx3Repeat = true;
HANDLE g_hStdOut;					// Handle for console window
CONSOLE_SCREEN_BUFFER_INFO g_csbi;	// Console information structure

// Example 4
#define COUNTER_TARGET	1000000

// Forward declare thread functions
// Note: _beginthread requires thread functions be declared with "__cdecl" calling convention.
// This can be set explicitly eg. "void __cdecl func(void* );"
void Ex2_ThreadFunc(void* pData);
void Ex3_ThreadFunc_CheckKey(void*);
void Ex3_ThreadFunc_Bounce(void*);
unsigned int __stdcall Ex4_ThreadFunc(void* pData);

// Constructor / Destructor
CThreadsSimple::CThreadsSimple()
{
	std::cout << "  [CThreadsSimple constructor]\n";

	// Add a list of possible threads to a vector (so we can easily iterate over them)
	for (ThreadsSimple ex = ThreadsSimple::Example1;
			ex != ThreadsSimple::ExamplesEnd;
			ex = (ThreadsSimple)((int)ex+1))
	{
		m_threads.push_back(ex);
		m_threadRunning.push_back(0);
	}

	// Thread handles
	m_hThreadEx1 = nullptr;
	m_hThreadEx2 = nullptr;
	m_hThreadEx3_CheckKey = nullptr;
	m_hThreadEx4 = nullptr;

	// Synchronization object
	InitializeCriticalSection(&m_hCritSect);
}

CThreadsSimple::~CThreadsSimple()
{
	std::cout << "  [CThreadsSimple destructor]\n";

	// Stop all threads
	std::cout << "  [CThreadsSimple destructor - stop all threads]\n";
	for (auto ex : m_threads) {
		StopThread(ex);
	}

	// Alternatively: Use an iterator
	//for (std::vector<ThreadsSimple>::const_iterator it = m_threads.begin(); it != m_threads.end(); ++it) {
	//	StopThread(*it);
	//}

	// Clean up synchronization object
	DeleteCriticalSection(&m_hCritSect);
}

// Thread examples
void CThreadsSimple::StartThread(const ThreadsSimple ex)
{
	// Start one of the threads
	if (m_threadRunning[(int)ex] > 0) {
		// Thread example already started...
		std::cout << "  [Thread ex" << ((int)ex + 1) << " is already running]\n";
		return;
	}

	switch (ex)
	{
	case ThreadsSimple::Example1:
		Ex1_Run();
		break;
	case ThreadsSimple::Example2:
		Ex2_Run();
		break;
	case ThreadsSimple::Example3:
		Ex3_Run();
		break;
	case ThreadsSimple::Example4:
		Ex4_Run();
		break;
	}
}

void CThreadsSimple::StopThread(const ThreadsSimple ex)
{
	// Stop one of the threads
	if (m_threadRunning[(int)ex] == 0) {
		// Thread example not started...
		std::cout << "  [Thread ex" << ((int)ex + 1) << " is not running]\n";
		return;
	}

	std::cout << "...terminating thread example " << ((int)ex + 1) << "\n";
	DWORD dwTimer;
	switch (ex)
    {
	case ThreadsSimple::Example1:
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
		m_threadRunning[(int)ThreadsSimple::Example1]--;
		std::cout << "...example 1 terminated, counter is " << m_threadCounter << "\n";
		break;

	case ThreadsSimple::Example2:
		// Terminate thread example 2
		g_bTerminateEx2 = true;
		if (!g_bFinishedEx2) {
			TerminateThread(m_hThreadEx2, 0);
			g_bFinishedEx2 = true;
		}
		CloseHandle(m_hThreadEx2);
		m_threadRunning[(int)ThreadsSimple::Example2]--;
		std::cout << "...example 2 terminated, counter is " << m_threadCounter << "\n";
		break;

	case ThreadsSimple::Example3:
	case ThreadsSimple::Example4:
		// Threads are stopped separately
		break;
	}
}

// Private methods
void CThreadsSimple::Ex1_Run()
{
	std::cout << "  Using CreateThread\n";
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
		m_threadRunning[(int)ThreadsSimple::Example1]++;
		std::cout << "    Successfully created thread with ID " << dwThreadID << "!\n";
	}
}

void CThreadsSimple::Ex2_Run()
{
	std::cout << "  Using _beginthread\n";
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
		m_threadRunning[(int)ThreadsSimple::Example2]++;
		std::cout << "    Successfully created thread!\n";
	}
}

void CThreadsSimple::Ex3_Run()
{
	std::cout << "  Using _beginthread (complex example)\n";

	// Get display screen's text row and column information.
	g_hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
	GetConsoleScreenBufferInfo(g_hStdOut, &g_csbi);

	// Launch a separate thread to check for terminating keystroke
	m_hThreadEx3_CheckKey = (HANDLE)_beginthread(Ex3_ThreadFunc_CheckKey, 0, NULL);
	if (m_hThreadEx3_CheckKey == nullptr)
		std::cout << "    Failed to create thread...\n";
	else
		std::cout << "    Successfully created thread!\n";

	// Loop until user terminates (by pressing a key) or the maximum number of threads are created
	int threadNum = 0;
	while (g_bEx3Repeat && threadNum < MAX_BOUNCE_THREADS)
	{
        // Launch another character thread
        _beginthread(Ex3_ThreadFunc_Bounce, 0, (void *) &m_threadRunning[(int)ThreadsSimple::Example3]);

        // Increment the thread parameter
        threadNum++;
		std::cout << "...bounce thread " << m_threadRunning[(int)ThreadsSimple::Example3] << " started\n";
		if (threadNum < MAX_BOUNCE_THREADS)
			Sleep(1000L);
		else
			std::cout << "  (maximum number of bounce threads created!)\n";
	}

	std::cout << "  (waiting for bounce threads to terminate)\n";
	while ((g_bEx3Repeat) || (m_threadRunning[(int)ThreadsSimple::Example3] > 0)) {
		Sleep(20);
	}

	std::cout << "  (bounce threads all terminated!)\n";
}

void CThreadsSimple::Ex4_Run()
{
	std::cout << "  Using _beginthreadex\n";
	uint32_t uThreadID; 
	m_threadCounter = 0;
	m_hThreadEx4 = (HANDLE)_beginthreadex(NULL, 0, &Ex4_ThreadFunc, &m_threadCounter, 0, &uThreadID);

    // Wait until second thread terminates. If you comment out the line below, the counter will
	// not be correct because the thread has not terminated, and counter most likely has not been
	// incremented to rach the target yet.
    WaitForSingleObject(m_hThreadEx4, INFINITE);
	std::cout << "  Counter should be " << COUNTER_TARGET << "; it is-> " << m_threadCounter << "\n";

    // Destroy the thread object
    CloseHandle(m_hThreadEx4);
}

// Thread functions
DWORD CThreadsSimple::Ex1_ThreadFunc(LPVOID lParam)
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
	// Example 3 thread function
	// Thread to wait for a keystroke, then clear the global repeat flag
	std::cout << "  (example 3 thread, ID " << _threadid << ", press any key to stop thread)\n";
	(void) _getch();
	std::cout << "    (key pressed...stopping threads!)\n";
	g_bEx3Repeat = false;
	_endthread();
}

void Ex3_ThreadFunc_Bounce(void* pData)
{
	// Example 3 thread function

	// Thread to create and control a colored letter that bounces around the screen
	// Parameters: pData is the address of variable which tells how many threads are running
	uint32_t& threadsRunning = *((uint32_t*)pData);
	EnterCriticalSection(&m_hCritSect);
	threadsRunning++;
	LeaveCriticalSection(&m_hCritSect);

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
	EnterCriticalSection(&m_hCritSect);
	--threadsRunning;
	LeaveCriticalSection(&m_hCritSect);
	_endthread();
}

unsigned int __stdcall Ex4_ThreadFunc(void* pData)
{
	// Example 4 thread function
	uint32_t& localCounter = *((uint32_t*)pData);
	while (localCounter < COUNTER_TARGET)
		localCounter++;

	_endthreadex(0);
	return 0;
}
