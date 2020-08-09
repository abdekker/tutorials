#pragma once
#include <map>
#include <mutex>
#include <vector>

enum class ThreadsSTL
{
    Example1 = 0,	// std::thread (simple example with std::thread::join)
	Example2,		// std::thread (simple example with std::thread::detach)
	Example3,		// using std::mutex to protect a resource
	Example4,		// using class to wrap the thread function
	ExamplesEnd
};

class CThreadsSTL
{
	public:
	// Constructor / Destructor
    CThreadsSTL();
    ~CThreadsSTL();

	// Thread examples
	void StartThread(const ThreadsSTL ex);
	void StopThread(const ThreadsSTL ex);

private:
	// Member data
	std::vector<ThreadsSTL> m_threads;
	std::vector<uint8_t> m_threadRunning;

	// Thread member data
	static std::map<std::string, std::string> m_ex3Pages;
	static std::mutex m_ex3Mutex;

	// Methods
	void Ex1_Run();
	void Ex2_Run();
	void Ex3_Run();
	void Ex4_Run();

	// Static methods (used for thread functions)
	static void Ex1_Ex2_ThreadFunc();
	static void Ex3_ThreadFunc(const std::string &url);
};
