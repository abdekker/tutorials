#include "CThreadsSTL.h"
#include "..\..\Utils\stringHelper.h"

#include <algorithm>	// std::accumulate
#include <chrono>		// std::chrono_literals, std::this_thread::sleep_for
#include <conio.h>		// _getch
#include <future>		// std::async
#include <iostream>
#include <numeric>
#include <thread>		// std::thread

using namespace std;

// Global flags

// Example 1
class CEx1DisplayThread
{
public:
	void operator()() {
		cout << "    (Inside display thread)\n";
	}
};

// Example 3
map<string, string> CThreadsSTL::m_ex3Pages;
mutex CThreadsSTL::m_ex3Mutex;

// Example 4
class CEx4aWrapper {
public:
	void operator()() {
		for (int i=0; i >= -10; i--)
			cout << "  wrapper: i = " << i << endl;
	};
};

class CEx4bWrapper {
public:
	void operator()(string &msg) {
		cout << "  thread says: " << msg << endl;
		msg = "modified by thread!";
	};
};

// Constructor / Destructor
CThreadsSTL::CThreadsSTL()
{
	cout << "  [CThreadsSTL constructor]\n";

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
	cout << "  [CThreadsSTL destructor]\n";

	// Stop all threads
	cout << "  [CThreadsSTL destructor - stop all threads]\n";
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
		cout << "  [Thread ex" << ((int)ex + 1) << " is already running]\n";
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
	case ThreadsSTL::Example4a:
		Ex4a_Run();
		break;
	case ThreadsSTL::Example4b:
		Ex4b_Run();
		break;
	}
}

void CThreadsSTL::StopThread(const ThreadsSTL ex)
{
	// Stop one of the threads
	if (m_threadRunning[(int)ex] == 0) {
		// Thread not running...
		cout << "  [Thread ex" << ((int)ex + 1) << " is not running]\n";
		return;
	}

	cout << "...terminating thread example " << ((int)ex + 1) << "\n";
	switch (ex)
    {
	case ThreadsSTL::Example1:
	case ThreadsSTL::Example2:
	case ThreadsSTL::Example3:
	case ThreadsSTL::Example4a:
	case ThreadsSTL::Example4b:
		break;
	}
}

// Private methods
void CThreadsSTL::Ex1_Run()
{
	// Example 1: thread with join
	cout << "  Creating thread with thread, then use thread::join\n";
	thread tEx1(Ex1_Ex2_ThreadFunc);

	// Note: Threads should be joined only once (main thread waits for the child thread to complete).
	// If the thread is not joined (or detached) before completing its work, the application exits.
	tEx1.join();
	cout << "...finished with thread (ex1)\n";
}

void CThreadsSTL::Ex2_Run()
{
	// Example 2: thread with detach
	using namespace chrono_literals;
	cout << "  Creating thread with thread, then use thread::detach\n";
	thread tEx2(Ex1_Ex2_ThreadFunc);

	// Note: Threads should be detached only once (child thread runs independently)
	tEx2.detach();
	cout << "...thread detached (it will now run completely independently of the main thread)\n";
	this_thread::sleep_for(2s);

	// If you Check it is joinable (which
	if (tEx2.joinable())
		tEx2.join();
	else
		cout << "  [thread is independent and cannot be joined]\n";

	cout << "...finished with thread (ex2)\n";
}

void CThreadsSTL::Ex3_Run()
{
	// Example 31: thread with mutex
	cout << "  Creating threads with thread, then use thread::mutex to protect resources\n";
	m_ex3Pages.clear();
	thread tEx3a(Ex3_ThreadFunc, "http://foo");
	thread tEx3b(Ex3_ThreadFunc, "http://bar");
	tEx3a.join();
	tEx3b.join();
 
	// Safe to access the map without lock now, as the threads are joined
	for (const auto &pair : m_ex3Pages) {
		cout << "  " << pair.first << " => " << pair.second << '\n';
	}

	cout << "...finished with threads (ex3)\n";
}

void CThreadsSTL::Ex4a_Run()
{
	// Example 4a: Using simple wrapper for thread
	cout << "  Creating wrapper for thread function\n";
	CEx4aWrapper wrapper;
	thread tEx4a(wrapper);

	// Count up from 0 (while the child counts down from 0). This creates a mess in the console
	// output! Wrap the block in a try-catch block to ensure that the child thread completes.
	try {
		for (int i=0; i <= 10; i++)
			cout << "  main thread: i = " << i << endl;
	} catch (...) {
		tEx4a.join();
		throw;
	}

	if (tEx4a.joinable())
		tEx4a.join();

	cout << "...finished with threads (ex4a)\n";
}

void CThreadsSTL::Ex4b_Run()
{
	/*// Example 4b: Using simple wrapper for thread, this time passing a parameter
	cout << "  Creating wrapper for thread function\n";
	string msg = "hello from main";
	CEx4aWrapper wrapper;
	thread tEx4b(wrapper, msg);

	//thread tEx4b((CEx4bWrapper()), msg);
	tEx4b.join();
	cout << "  main says: " << msg << endl;
	cout << "  (Note: String was declared pass by reference, but was actually passed by value)\n";
	cout << "...finished with threads (ex4b)\n";*/
}

// Thread functions
void CThreadsSTL::Ex1_Ex2_ThreadFunc()
{
	// Example 1/2 thread function
	cout << "  Simple example 1/2 thread function!\n";
}

void CThreadsSTL::Ex3_ThreadFunc(const string &url)
{
	// Simulate a long page fetch
	cout << "  (sleep thread for a few seconds to simulate a long fetch)\n";
	this_thread::sleep_for(chrono::seconds(2));

	// Create the text for page
	stringHelper sHelper;
	thread::id this_id = this_thread::get_id();
	string result = sHelper.formatString("fake content from thread 0x%8.8d", this_id); 

	// Lock mutex and update our map
	m_ex3Mutex.lock();
	m_ex3Pages[url] = result;
	m_ex3Mutex.unlock();

	// Alternative syntax for mutex lock is:
	//lock_guard<mutex> guard(m_ex3Mutex);
    //m_ex3Pages[url] = result;
}
