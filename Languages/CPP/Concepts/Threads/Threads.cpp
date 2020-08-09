#include <conio.h>
#include <iostream>

#include "CThreadsSimple.h"
#include "CThreadsSTL.h"

void RunSimpleThreads()
{
	// Simple threads (Microsoft specific)
	std::cout << "\n\n### Simple threads (specific to Windows) ###\n";
	CThreadsSimple threadsSimple;

	// Example 1
	bool bRunExample = false;
	if (bRunExample) {
		// Example 1: CreateThread (Windows-only)
		std::cout << "\n### Example 1 (CreateThread, specific to Windows) ###\n";
		threadsSimple.StartThread(ThreadsSimple::Example1);
		std::cout << "...attempt to start the thread again\n";
		threadsSimple.StartThread(ThreadsSimple::Example1);

		// Rest a little (to give the thread some time)
		std::cout << "  (press any key to terminate the thread)\n";
		(void) _getch();
		threadsSimple.StopThread(ThreadsSimple::Example1);
		std::cout << "...attempt to stop the thread again\n";
		threadsSimple.StopThread(ThreadsSimple::Example1);
		std::cout << "###\n\n";
	}

	// Example 2
	bRunExample = false;
	if (bRunExample) {
		// Example 2: _beginthread (Windows-only)
		std::cout << "\n### Example 2 (_beginthread, specific to Windows) ###\n";
		threadsSimple.StartThread(ThreadsSimple::Example2);
		std::cout << "  (press any key to terminate the thread)\n";
		(void) _getch();
		threadsSimple.StopThread(ThreadsSimple::Example2);
		std::cout << "###\n\n";
	}

	// Example 3
	bRunExample = false;
	if (bRunExample) {
		// Example 3: _beginthread (Windows-only)
		std::cout << "\n### Example 3 (_beginthread, complex multi-threaded example) ###\n";
		std::cout << "  (press any key to terminate the threads in this example)\n";
		threadsSimple.StartThread(ThreadsSimple::Example3);
		std::cout << "###\n\n";
	}

	// Example 4
	bRunExample = false;
	if (bRunExample) {
		// Example 4: _beginthreadex (Windows-only)
		std::cout << "\n### Example 4 (_beginthreadex, specific to Windows) ###\n";
		std::cout << "  (press any key to terminate the threads in this example)\n";
		threadsSimple.StartThread(ThreadsSimple::Example4);
		std::cout << "###\n\n";
	}
	std::cout << "### (simple threads end)\n\n";
}

void RunThreadsSTL()
{
	// STL threads (cross-platform)_
	std::cout << "\n\n### STL threads (cross-platform) ###\n";
	CThreadsSTL threadsSTL;

	// Example 1
	bool bRunExample = false;
	if (bRunExample) {
		// Example 1: std::thread (with thread::join)
		std::cout << "\n### Example 1 (std::thread and thread::join) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example1);
		std::cout << "###\n\n";
	}

	// Example 2
	bRunExample = false;
	if (bRunExample) {
		// Example 2: std::thread (with thread::detach)
		std::cout << "\n### Example 2 (std::thread and thread::detach) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example2);
		std::cout << "###\n\n";
	}

	// Example 3
	bRunExample = false;
	if (bRunExample) {
		// Example 3: std::thread (with thread::mutex)
		std::cout << "\n### Example 3 (std::thread and thread::mutex) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example3);
		std::cout << "###\n\n";
	}

	// Example 4
	bRunExample = true;
	if (bRunExample) {
		// Example 4: std::thread with wrapper
		std::cout << "\n### Example 4 (std::thread with wrapper) ###\n";
		threadsSTL.StartThread(ThreadsSTL::Example4);
		std::cout << "###\n\n";
	}

	std::cout << "### (STL threads end)\n\n";
}

int main()
{
	// Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);

	// Sample application to show some concepts of dealing with threads in C++
    std::cout << "### C++ threads...###\n";
	RunSimpleThreads();
	RunThreadsSTL();

	// All done!
	std::cout << "\nFinished...";
	return 0;
}
