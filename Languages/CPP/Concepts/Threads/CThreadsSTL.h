#pragma once
#include <map>
#include <mutex>
#include <vector>

enum class ThreadsSTL
{
    Example1 = 0,	// std::thread with std::thread::join
	Example2,		// std::thread with std::thread::detach
	Example3,		// using std::mutex to protect a resource
	Example4a,		// using class to wrap the thread function
	Example4b,		// passing a parameter to the wrapper
	Example5,		// std::async
	Example6,		// std::condition_variable
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
	static std::map<std::string, std::string> m_ex3Pages;	// Example 3
	static std::mutex m_ex3Mutex;

	// Methods
	void Ex1_Run();
	void Ex2_Run();
	void Ex3_Run();
	void Ex4a_Run();
	void Ex4b_Run();
	void Ex5_Run();
	void Ex6_Run();

	// Static methods (used for thread functions)
	static void Ex1_Ex2_ThreadFunc();
	static void Ex3_ThreadFunc(const std::string &url);
	static void Ex6_ThreadFunc();
	static void Ex6_LockHelper(std::unique_lock<std::mutex> &lk, const std::string prefix);

};
