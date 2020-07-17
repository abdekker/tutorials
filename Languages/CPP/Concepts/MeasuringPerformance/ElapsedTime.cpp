#include <iostream>
#include <chrono>

// Measure elapsed time in Visual Studio Code
int main()
{
    // Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);
    std::cout << "Measuring performance using std::chrono\n";

    // Measure some time-consuming operation
    std::chrono::system_clock::time_point tmp;
    auto start1 = std::chrono::system_clock::now();

    // Some computation here...
    for (int loop=0; loop < 100000000; loop++)
        tmp = std::chrono::system_clock::now();

    // Show results
    std::chrono::duration<double> elapsed1 = (std::chrono::system_clock::now() - start1);
    std::cout << "Elapsed time (std::chrono):\t" << elapsed1.count() << "s\n";

    // Now use the time.h::clock method which returns the ms since the process started
    std::cout << "\nMeasuring performance using time.h::clock()\n";

    // Measure some time-consuming operation
    clock_t start2 = clock();

    // Some computation here...
    for (int loop=0; loop < 100000000; loop++)
        tmp = std::chrono::system_clock::now();

    // Show results
    clock_t elapsed2 = (clock() - start2);
    std::cout << "Elapsed time (time.h::clock()):\t" << elapsed2 << "ms\n";

    // Exit
    std::cout << "\nAll done!\n";
    return 0;
}
