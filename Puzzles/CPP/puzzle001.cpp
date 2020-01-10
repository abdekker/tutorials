#include "puzzle001.h"

const int TARGET = 1000;

using namespace std;
using std::vector;
int main()
{
    // See ../Puzzles/Notes-Puzzles.txt for details

    cout << "Print all numbers up to " << TARGET << " that have the digit 5 in their number";
    ClassPuzzle001 puzzle;

    cout << "\n\nAttempt 1 (use integer division 1)\n";
    puzzle.Attempt1();
    cout << "\n\nAttempt 2 (use integer division 2)\n";
    puzzle.Attempt2();
    cout << "\n\nAttempt 3 (convert to char *)\n";
    puzzle.Attempt3();
    cout << "\n\nAttempt 4 (convert to std::string)\n";
    puzzle.Attempt4();

    // Now repeat using a method that saves to a vector
    vector<int> vecNumbers;
    int pos;

    cout << "\n\nAttempt 1 again (no print)\n";
    puzzle.Attempt1NoPrint(vecNumbers);
    for (pos=0; pos < vecNumbers.size(); pos++)
        cout << vecNumbers[pos] << " ";

    cout << "\n\nAttempt 2 again (no print)\n";
    puzzle.Attempt2NoPrint(vecNumbers);
    for (pos=0; pos < vecNumbers.size(); pos++)
        cout << vecNumbers[pos] << " ";

    cout << "\n\nAttempt 3 again (no print)\n";
    puzzle.Attempt3NoPrint(vecNumbers);
    for (pos=0; pos < vecNumbers.size(); pos++)
        cout << vecNumbers[pos] << " ";

    cout << "\n\nAttempt 4 again (no print)\n";
    puzzle.Attempt4NoPrint(vecNumbers);
    for (pos=0; pos < vecNumbers.size(); pos++)
        cout << vecNumbers[pos] << " ";

    // Compare the performance of each method
    puzzle.PerformanceTests();

    cout << "\nAll done!\n";
}

void ClassPuzzle001::Attempt1()
{
    // Attempt 1: Make use of integer division (crude version that fails if TARGET > 1000)
    int num, tmp;
    for (num=1; num < TARGET; num++)
    {
        if (((num % 5) == 0) && ((num % 10) != 0))
            cout << num << " ";
        else
        {
            tmp = (num / 10);
            if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                cout << num << " ";
            else
            {
                tmp = (num / 100);
                if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                    cout << num << " ";
            }
        }
    }
}

void ClassPuzzle001::Attempt1NoPrint(vector<int>& vec)
{
    // Attempt 1: Same version but saves values to a vector (the caller should print them)
    vec.clear();
    int num, tmp;
    for (num=1; num < TARGET; num++)
    {
        if (((num % 5) == 0) && ((num % 10) != 0))
            vec.push_back(num);
        else
        {
            tmp = (num / 10);
            if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                vec.push_back(num);
            else
            {
                tmp = (num / 100);
                if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                    vec.push_back(num);
            }
        }
    }
}

void ClassPuzzle001::Attempt2()
{
    // Attempt 2: Make use of integer division (sophisticated version that works for all values of TARGET)
    int num, tmp;
    for (num=1; num < TARGET; num++)
    {
        if (((num % 5) == 0) && ((num % 10) != 0))
            cout << num << " ";
        else
        {
            tmp = num;
            do
            {
                tmp = (tmp / 10);
                if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                {
                    cout << num << " ";
                    tmp = 0;
                }
            } while (tmp > 0);
        }
    }
}

void ClassPuzzle001::Attempt2NoPrint(vector<int>& vec)
{
    // Attempt 2: Same version but saves values to a vector
    vec.clear();
    int num, tmp;
    for (num=1; num < TARGET; num++)
    {
        if (((num % 5) == 0) && ((num % 10) != 0))
            vec.push_back(num);
        else
        {
            tmp = num;
            do
            {
                tmp = (tmp / 10);
                if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                {
                    vec.push_back(num);
                    tmp = 0;
                }
            } while (tmp > 0);
        }
    }
}

void ClassPuzzle001::Attempt3()
{
    // Attempt 3: Convert number to char *
    char numAsChar[10];
    for (int num=1; num < TARGET; num++)
    {
        snprintf(numAsChar, 10, "%d", num);
        if (strchr(numAsChar, '5') != NULL)
            cout << num << " ";
    }
}

void ClassPuzzle001::Attempt3NoPrint(vector<int>& vec)
{
    // Attempt 3: Same version but saves values to a vector
    vec.clear();
    char numAsChar[10];
    for (int num=1; num < TARGET; num++)
    {
        snprintf(numAsChar, 10, "%d", num);
        if (strchr(numAsChar, '5') != NULL)
            vec.push_back(num);
    }
}

void ClassPuzzle001::Attempt4()
{
    // Attempt 4: Convert number to std::string
    string numAsString;
    for (int num=1; num < TARGET; num++)
    {
        numAsString = to_string(num);
        if (numAsString.find("5") != string::npos)
            cout << num << " ";
    }
}

void ClassPuzzle001::Attempt4NoPrint(vector<int>& vec)
{
    // Attempt 4: Same version but saves values to a vector
    vec.clear();
    string numAsString;
    for (int num=1; num < TARGET; num++)
    {
        numAsString = to_string(num);
        if (numAsString.find("5") != string::npos)
            vec.push_back(num);
    }
}

void ClassPuzzle001::PerformanceTests()
{
    // Test the performance of the various attempts
    cout << "\n\nShow performance of each attempt (no printing)\n";
    vector<int> vecNumbers;
    stringHelper strings;
    const int iterations = 20000;
    int loop;

    // Attempt 1
    auto timerStart = chrono::high_resolution_clock::now();
    for (loop=0; loop < iterations; loop++)
        Attempt1NoPrint(vecNumbers);

    auto timerEnd = chrono::high_resolution_clock::now();
    long timerDuration = chrono::duration_cast<chrono::nanoseconds>(timerEnd - timerStart).count();
    double totalTime = static_cast<double>(timerDuration) / static_cast<double>(1000000000L);    // Convert to s
    double averagePerAttempt =
        (static_cast<double>(timerDuration) / static_cast<double>(iterations));
    averagePerAttempt /= static_cast<double>(1000000L);   // Concert to ms
    cout << strings.formatString("  Attempt 1: Total %.3f s, Avg %.3f ms (%d)\n",
        totalTime, averagePerAttempt, iterations);

    // Attempt 2
    timerStart = chrono::high_resolution_clock::now();
    for (loop=0; loop < iterations; loop++)
        Attempt2NoPrint(vecNumbers);

    timerEnd = chrono::high_resolution_clock::now();
    timerDuration = chrono::duration_cast<chrono::nanoseconds>(timerEnd - timerStart).count();
    totalTime = static_cast<double>(timerDuration) / static_cast<double>(1000000000L);
    averagePerAttempt = (static_cast<double>(timerDuration) / static_cast<double>(iterations));
    averagePerAttempt /= static_cast<double>(1000000L);
    cout << strings.formatString("  Attempt 2: Total %.3f s, Avg %.3f ms (%d)\n",
        totalTime, averagePerAttempt, iterations);

    // Attempt 3
    timerStart = chrono::high_resolution_clock::now();
    for (loop=0; loop < iterations; loop++)
        Attempt3NoPrint(vecNumbers);

    timerEnd = chrono::high_resolution_clock::now();
    timerDuration = chrono::duration_cast<chrono::nanoseconds>(timerEnd - timerStart).count();
    totalTime = static_cast<double>(timerDuration) / static_cast<double>(1000000000L);
    averagePerAttempt = (static_cast<double>(timerDuration) / static_cast<double>(iterations));
    averagePerAttempt /= static_cast<double>(1000000L);
    cout << strings.formatString("  Attempt 3: Total %.3f s, Avg %.3f ms (%d)\n",
        totalTime, averagePerAttempt, iterations);

    // Attempt 4
    timerStart = chrono::high_resolution_clock::now();
    for (loop=0; loop < iterations; loop++)
        Attempt4NoPrint(vecNumbers);

    timerEnd = chrono::high_resolution_clock::now();
    timerDuration = chrono::duration_cast<chrono::nanoseconds>(timerEnd - timerStart).count();
    totalTime = static_cast<double>(timerDuration) / static_cast<double>(1000000000L);
    averagePerAttempt = (static_cast<double>(timerDuration) / static_cast<double>(iterations));
    averagePerAttempt /= static_cast<double>(1000000L);
    cout << strings.formatString("  Attempt 4: Total %.3f s, Avg %.3f ms (%d)\n",
        totalTime, averagePerAttempt, iterations);
}
