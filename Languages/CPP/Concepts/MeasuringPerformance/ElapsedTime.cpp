#include <iostream>
#include <chrono>

// Measure elapsed time in Visual Studio Code
int main()
{
    // Send output to console
    setvbuf(stdout, NULL, _IONBF, 0);
    std::cout << "Measuring performance using std::chrono\n\n";

    // Testing commiting individual lines, Visual Studio Code 1

    // Measure some time-consuming operation
    // blah1
    std::chrono::system_clock::time_point tmp;
    auto startTime = std::chrono::system_clock::now();

    // Some computation here...
    for (int loop=0; loop < 100000000; loop++)
        tmp = std::chrono::system_clock::now();

    // Testing commiting individual lines, Visual Studio Code 2

    // Show results
    std::chrono::duration<double> elapsedSeconds = (std::chrono::system_clock::now() - startTime);
    std::cout << "Elapsed time: " << elapsedSeconds.count() << "s\n";
    std::cout << "All done!\n";
    return 0;
}
