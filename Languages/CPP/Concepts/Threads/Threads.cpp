#include <conio.h>
#include <iostream>

#include "CThreadsSimple.h"
#include "CThreadsSTL.h"

using namespace std;

void RunSimpleThreads()
{
	// Simple threads (Microsoft specific)
	cout << "\n\n### Simple threads (specific to Windows) ###\n";
	CThreadsSimple threadsSimple;

	// Example 1
	bool bRunExample = false;
	if (bRunExample) {
		// Example 1: CreateThread (Windows-only)
		cout << "\n### Example 1 (CreateThread, specific to Windows) ###\n";
		threadsSimple.StartThread(ThreadsSimple::Example1);
		cout << "...attempt to start the thread again\n";
		threadsSimple.StartThread(ThreadsSimple::Example1);

		// Rest a little (to give the thread some time)
		cout << "  (press any key to terminate the thread)\n";
		(void) _getch();
		threadsSimple.StopThread(ThreadsSimple::Example1);
		cout << "...attempt to stop the thread again\n";
		threadsSimple.StopThread(ThreadsSimple::Example1);
		cout << "###\n\n";
	}

	// Example 2
	bRunExample = false;
	if (bRunExample) {
		// Example 2: _beginthread (Windows-only)
		cout << "\n### Example 2 (_beginthread, specific to Windows) ###\n";
		threadsSimple.StartThread(ThreadsSimple::Example2);
		cout << "  (press any key to terminate the thread)\n";
		(void) _getch();
		threadsSimple.StopThread(ThreadsSimple::Example2);
		cout << "###\n\n";
	}

	// Example 3
	bRunExample = false;
	if (bRunExample) {
		// Example 3: _beginthread (Windows-only)
		cout << "\n### Example 3 (_beginthread, complex multi-threaded example) ###\n";
		cout << "  (press any key to terminate the threads in this example)\n";
		threadsSimple.StartThread(ThreadsSimple::Example3);
		cout << "###\n\n";
	}

	// Example 4
	bRunExample = false;
	if (bRunExample) {
		// Example 4: _beginthreadex (Windows-only)
		cout << "\n### Example 4 (_beginthreadex, specific to Windows) ###\n";
		cout << "  (press any key to terminate the threads in this example)\n";
		threadsSimple.StartThread(ThreadsSimple::Example4);
		cout << "###\n\n";
	}
	cout << "### (simple threads end)\n\n";
}

void RunThreadsSTL()
{
	// STL threads (cross-platform)_
	cout << "\n\n### STL threads (cross-platform) ###\n";
	CThreadsSTL threadsSTL;

	// Example 1
	bool bRunExample = false;
	if (bRunExample) {
		// Example 1: std::thread (with thread::join)
		cout << "\n### Example 1 (std::thread and std::thread::join) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example1);
		cout << "###\n\n";
	}

	// Example 2
	bRunExample = false;
	if (bRunExample) {
		// Example 2: std::thread (with thread::detach)
		cout << "\n### Example 2 (std::thread and std::thread::detach) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example2);
		cout << "###\n\n";
	}

	// Example 3
	bRunExample = false;
	if (bRunExample) {
		// Example 3: std::thread (with std::thread::mutex)
		cout << "\n### Example 3 (std::thread and std::thread::mutex) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example3);
		cout << "###\n\n";
	}

	// Example 4
	bRunExample = false;
	if (bRunExample) {
		// Example 4a: std::thread with wrapper
		cout << "\n### Example 4a (std::thread with wrapper) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example4a);
		cout << "###\n\n";

		// Example 4b: std::thread with wrapper, this time passing a parameter
		cout << "\n### Example 4b (std::thread with wrapper and parameter) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example4b);
		cout << "###\n\n";
	}

	// Example 5
	bRunExample = true;
	if (bRunExample) {
		// Example 5: std::async and std::future
		cout << "\n### Example 5 (std::thread and std::future) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example5);
		cout << "###\n\n";
	}

	// Example 6
	bRunExample = true;
	if (bRunExample) {
		// Example 6: std::thread and std::condition_variable
		cout << "\n### Example 6 (std::thread and std::condition_variable) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example6);
		cout << "###\n\n";
	}

	cout << "### (STL threads end)\n\n";
}

int main()
{
	// Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);

	// Sample application to show some concepts of dealing with threads in C++
    cout << "### C++ threads...###\n";
	RunSimpleThreads();
	RunThreadsSTL();

	// All done!
	cout << "\nFinished...";
	return 0;
}
