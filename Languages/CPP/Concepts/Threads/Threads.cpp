#include <conio.h>
#include <iostream>
#include "CThreads.h"

using namespace std;
int main()
{
	// Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);

	// Sample application to show some concepts of dealing with threads in C++
    std::cout << "C++ threads...\n\n";
	CThreads threads;
	bool bRunExample = true;

	// Example 1
	bRunExample = false;
	if (bRunExample) {
		// Example 1: CreateThread (Windows-only)
		std::cout << "### Example 1 (CreateThread, specific to Windows) ###\n";
		threads.StartThread(ThreadExample::Example1);
		std::cout << "...attempt to start the thread again\n";
		threads.StartThread(ThreadExample::Example1);

		// Rest a little (to give the thread some time)
		std::cout << "  (press any key to terminate the thread)\n";
		(void) _getch();
		threads.StopThread(ThreadExample::Example1);
		std::cout << "...attempt to stop the thread again\n";
		threads.StopThread(ThreadExample::Example1);
		std::cout << "###\n\n";
	}

	// Example 2
	bRunExample = false;
	if (bRunExample) {
		// Example 2: _beginthread (Windows-only)
		std::cout << "### Example 2 (_beginthread, specific to Windows) ###\n";
		threads.StartThread(ThreadExample::Example2);
		std::cout << "  (press any key to terminate the thread)\n";
		(void) _getch();
		threads.StopThread(ThreadExample::Example2);
		std::cout << "###\n\n";
	}

	// Example 3
	bRunExample = false;
	if (bRunExample) {
		// Example 3: _beginthread (Windows-only)
		std::cout << "### Example 3 (_beginthread, complex multi-threaded example) ###\n";
		std::cout << "  (press any key to terminate the threads in this example)\n";
		threads.StartThread(ThreadExample::Example3);
		std::cout << "###\n\n";
	}

	// Example 4
	bRunExample = true;
	if (bRunExample) {
		// Example 4: _beginthreadex (Windows-only)
		std::cout << "### Example 4 (_beginthreadex, specific to Windows) ###\n";
		std::cout << "  (press any key to terminate the threads in this example)\n";
		threads.StartThread(ThreadExample::Example4);
		std::cout << "###\n\n";
	}

	// All done!
	std::cout << "\nFinished...";
	return 0;
}
