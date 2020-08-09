#include "CThreadsSTL.h"
#include "..\..\Utils\stringHelper.h"

#include <chrono>		// std::chrono_literals, std::this_thread::sleep_for
#include <conio.h>		// _getch
#include <iostream>
#include <thread>		// std::thread

// Glocal flags

// Example 3
std::map<std::string, std::string> CThreadsSTL::m_ex3Pages;
std::mutex CThreadsSTL::m_ex3Mutex;

// Example 4
class CEx4Wrapper {
public:
	void operator()() {
		for (int i=0; i >= -10; i--)
			std::cout << "  wrapper: i = " << i << endl;
	};
};

// Constructor / Destructor
CThreadsSTL::CThreadsSTL()
{
	std::cout << "  [CThreadsSTL constructor]\n";

	// Add a list of possible threads to a vector (so we can easily iterate over them)
	for (ThreadsSTL ex = ThreadsSTL::Example1;
			ex != ThreadsSTL::ExamplesEnd;
			ex = (ThreadsSTL)((int)ex+1))
	{
		m_threads.push_back(ex);
		m_threadRunning.push_back(0);
	}
}

CThreadsSTL::~CThreadsSTL()
{
	std::cout << "  [CThreadsSTL destructor]\n";

	// Stop all threads
	std::cout << "  [CThreadsSTL destructor - stop all threads]\n";
	for (auto ex : m_threads)
	{
		StopThread(ex);
	}
}

void CThreadsSTL::StartThread(const ThreadsSTL ex)
{
	// Start one of the threads
	if (m_threadRunning[(int)ex] > 0) {
		// Thread already running...
		std::cout << "  [Thread ex" << ((int)ex + 1) << " is already running]\n";
		return;
	}

	switch (ex)
	{
	case ThreadsSTL::Example1:
		Ex1_Run();
		break;
	case ThreadsSTL::Example2:
		Ex2_Run();
		break;
	case ThreadsSTL::Example3:
		Ex3_Run();
		break;
	case ThreadsSTL::Example4:
		Ex4_Run();
		break;
	}
}

void CThreadsSTL::StopThread(const ThreadsSTL ex)
{
	// Stop one of the threads
	if (m_threadRunning[(int)ex] == 0) {
		// Thread not running...
		std::cout << "  [Thread ex" << ((int)ex + 1) << " is not running]\n";
		return;
	}

	std::cout << "...terminating thread example " << ((int)ex + 1) << "\n";
	switch (ex)
    {
	case ThreadsSTL::Example1:
	case ThreadsSTL::Example2:
	case ThreadsSTL::Example3:
	case ThreadsSTL::Example4:
		break;
	}
}

// Private methods
void CThreadsSTL::Ex1_Run()
{
	// Example 1: std::thread with std::join
	std::cout << "  Creating thread with std::thread, then use thread::join\n";
	std::thread tEx1(Ex1_Ex2_ThreadFunc);

	// Note: Threads should be joined only once (main thread waits for the child thread to complete).
	// If the thread is not joined (or detached) before completing its work, the application exits.
	tEx1.join();
	std::cout << "...finished with thread (ex1)\n";
}

void CThreadsSTL::Ex2_Run()
{
	// Example 2: std::thread with std::detach
	using namespace std::chrono_literals;
	std::cout << "  Creating thread with std::thread, then use thread::detach\n";
	std::thread tEx2(Ex1_Ex2_ThreadFunc);

	// Note: Threads should be detached only once (child thread runs independently)
	tEx2.detach();
	std::cout << "...thread detached (it will now run completely independently of the main thread)\n";
	std::this_thread::sleep_for(2s);

	// If you Check it is joinable (which
	if (tEx2.joinable())
		tEx2.join();
	else
		std::cout << "  [thread is independent and cannot be joined]\n";

	std::cout << "...finished with thread (ex2)\n";
}

void CThreadsSTL::Ex3_Run()
{
	// Example 31: std::thread with std::mutex
	std::cout << "  Creating threads with std::thread, then use thread::mutex to protect resources\n";
	m_ex3Pages.clear();
	std::thread tEx3a(Ex3_ThreadFunc, "http://foo");
	std::thread tEx3b(Ex3_ThreadFunc, "http://bar");
	tEx3a.join();
	tEx3b.join();
 
	// Safe to access the map without lock now, as the threads are joined
	for (const auto &pair : m_ex3Pages) {
		std::cout << "  " << pair.first << " => " << pair.second << '\n';
	}

	std::cout << "...finished with threads (ex3)\n";
}

void CThreadsSTL::Ex4_Run()
{
	// Example 4: Using simple wrapper for std::thread
	std::cout << "  Creating wrapper for thread function\n";
	CEx4Wrapper wrapper;
	std::thread tEx4(wrapper);

	// Count up from 0 (while the child counts down from 0). This will create a mess in the console
	// output! Wrap the block in a try-catch block to ensure that the child thread completes.
	try {
		for (int i=0; i <= 10; i++)
			std::cout << "  main thread: i = " << i << endl;
	} catch (...) {
		tEx4.join();
		throw;
	}

	if (tEx4.joinable())
		tEx4.join();

	std::cout << "...finished with threads (ex4)\n";
}

// Thread functions
void CThreadsSTL::Ex1_Ex2_ThreadFunc()
{
	// Example 1/2 thread function
	std::cout << "  Simple example 1/2 thread function!\n";
}

void CThreadsSTL::Ex3_ThreadFunc(const std::string &url)
{
	// Simulate a long page fetch
	std::cout << "  (sleep thread for a few seconds to simulate a long fetch)\n";
	std::this_thread::sleep_for(std::chrono::seconds(2));

	// Create the text for page
	stringHelper sHelper;
	std::thread::id this_id = std::this_thread::get_id();
	std::string result = sHelper.formatString("fake content from thread 0x%8.8d", this_id); 

	// Lock mutex and update our map
	m_ex3Mutex.lock();
	m_ex3Pages[url] = result;
	m_ex3Mutex.unlock();

	// Alternative syntax for mutex lock is:
	//std::lock_guard<std::mutex> guard(m_ex3Mutex);
    //m_ex3Pages[url] = result;
}
