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

// Example 5
mutex g_ex5Mutex;
struct CEx5 {
	void foo(int i, const string& str) {
		lock_guard<mutex> lk(g_ex5Mutex);
		cout << "    (" << str << ' ' << i << ")\n";
	}

	void bar(const string& str) {
		lock_guard<mutex> lk(g_ex5Mutex);
		cout << "    (" << str << ")\n";
	}

	int operator()(int i) {
		lock_guard<mutex> lk(g_ex5Mutex);
		cout << "    (in: " << i << ")\n";
		return (i + 10);
	}
};

// Example 6
mutex g_e6Mutex;
condition_variable g_ex6CV;
string g_ex6Data = "";
bool g_ex6Ready = false;
bool g_ex6Processed = false;

template <typename RandomIt>
int Ex5_ParallelSum(RandomIt beg, RandomIt end)
{
	auto len = (end - beg);
	if (len < 1000)
		return accumulate(beg, end, 0);		// For small arrays, use std::accumulate
 
	// Chop the array in two, and recursively create a new thread for the 2nd half
	RandomIt mid = (beg + len / 2);
	auto handle = async(launch::async, Ex5_ParallelSum<RandomIt>, mid, end);
	int sum = Ex5_ParallelSum(beg, mid);
	return (sum + handle.get());
}

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

	// Thread member data
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
	case ThreadsSTL::Example5:
		Ex5_Run();
		break;
	case ThreadsSTL::Example6:
		Ex6_Run();
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
	cout << "  Creating thread with std::thread from function pointer, then use std::thread::join\n";

	// Create a thread with a function pointer
	thread tEx1a(Ex1_Ex2_ThreadFunc);

	// Create a thread with a function object
	thread tEx1b( (CEx1DisplayThread() ));

	// Create a thread with lambda function
	thread tEx1c( [] {
		cout << "    (Inside lambda function)\n";
	});

	// Note: Threads should be joined only once (main thread waits for the child thread to complete).
	// If the thread is not joined (or detached) before completing its work, the application exits.
	tEx1a.join();
	tEx1b.join();
	tEx1c.join();
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

void CThreadsSTL::Ex5_Run()
{
	// Example 5: Using std::async
	cout << "  Using std::async\n";
	vector<int> v(57450, 1);
	cout << "    (vector has " << v.size() << " elements and the sum is " << Ex5_ParallelSum(v.begin(), v.end()) << ")\n";
 
	// Use a separate structure and call methods in the structure asynchronously
	CEx5 ex5;

	// Calls (&ex5)->foo(int, string) with default policy:
	//		=> may print "string int" concurrently or defer execution
	auto ex5a = async(&CEx5::foo, &ex5, 42, "default launch");

	// Calls ex5.bar(string) with deferred policy:
	//		=> prints "string" when a2.get() or a2.wait() is called
	auto ex5b = async(launch::deferred, &CEx5::bar, ex5, "deferred launch");

	// Calls CEx5()(int) with async policy:
	//		=> prints "int + 10" concurrently
	auto ex5c = async(launch::async, CEx5(), 43);
	ex5b.wait();									// prints "deferred launch"
	cout << "    (out: " << ex5c.get() << ")\n";	// prints "53"
	cout << "...finished with thread (ex5)\n";
}	// if a1 is not done at this point, destructor of a1 prints "default launch 42" here

void CThreadsSTL::Ex6_Run()
{
	// Example 6: Using std::condition_variable
	// Adapted from: https://en.cppreference.com/w/cpp/thread/condition_variable

	cout << "  Using std::condition_variable\n";
	thread tWorker(Ex6_ThreadFunc);
	g_ex6Data = "Example data";
	cout << "    (In caller, data = " << g_ex6Data << ")\n";

	// Send data to the worker thread
	{
		lock_guard<mutex> lk(g_e6Mutex);
		g_ex6Ready = true;
		cout << "    (Caller signals thread that data is ready for processing)\n";
	}
	g_ex6CV.notify_one();
 
	// Wait for the worker
	{
		unique_lock<mutex> lk(g_e6Mutex);
		g_ex6CV.wait(lk, []{ return g_ex6Processed; });
	}
	cout << "    (Back in caller, data = " << g_ex6Data << ")\n";
	tWorker.join();
	cout << "...finished with thread (ex6)\n";
}

// Thread functions
void CThreadsSTL::Ex1_Ex2_ThreadFunc()
{
	// Example 1/2 thread function
	cout << "    (Simple example thread function)\n";
}

void CThreadsSTL::Ex3_ThreadFunc(const string &url)
{
	// Simulate a long page fetch
	cout << "    (sleep thread for a few seconds to simulate a long fetch)\n";
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

void CThreadsSTL::Ex6_ThreadFunc()
{
	// Wait until the calling method sends data
	unique_lock<mutex> lk(g_e6Mutex);
	Ex6_LockHelper(lk, "      (1 ");

	g_ex6CV.wait(lk, []{ return g_ex6Ready; });
	Ex6_LockHelper(lk, "      (2 ");
 
	// After the wait, we own the lock
	cout << "    (Worker thread is processing data)\n";
	g_ex6Data += " added by worker thread";
 
	// Send data back to calling method
	g_ex6Processed = true;
	cout << "    (Worker thread signals data processing completed)\n";
 
	// Manual unlocking is done before notifying, to avoid waking up the waiting thread only to
	// block again
	Ex6_LockHelper(lk, "      (3 ");
	lk.unlock();
	Ex6_LockHelper(lk, "      (4 ");

	g_ex6CV.notify_one();
	Ex6_LockHelper(lk, "      (5 ");
}

void CThreadsSTL::Ex6_LockHelper(unique_lock<mutex> &lk, const string prefix)
{
	// Helper function to show the state of the mutex lock
	string owns = (lk.owns_lock()) ? "TRUE" : "FALSE";
	cout << prefix << " Worker thread, lock is owned? " << owns << ")\n";
}

