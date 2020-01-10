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
    puzzle.DoAttempt(AttemptNumber::ATTEMPT_1, TARGET, true);
    cout << "\n\nAttempt 2 (use integer division 2)\n";
    puzzle.DoAttempt(AttemptNumber::ATTEMPT_2, TARGET, true);
    cout << "\n\nAttempt 3 (use integer division 3)\n";
    puzzle.DoAttempt(AttemptNumber::ATTEMPT_3, TARGET, true);
    cout << "\n\nAttempt 4 (convert to char *)\n";
    puzzle.DoAttempt(AttemptNumber::ATTEMPT_4, TARGET, true);
    cout << "\n\nAttempt 5 (convert to std::string)\n";
    puzzle.DoAttempt(AttemptNumber::ATTEMPT_5, TARGET, true);

    // Compare the performance of each method
    puzzle.PerformanceTests();
    cout << "\nAll done!\n";
}

void ClassPuzzle001::DoAttempt(const AttemptNumber attempt, const int target, const bool print)
{
    // Attempt the problem...
    vector<int> vecResults;
    int num, tmp;
    switch(attempt)
    {
        case ATTEMPT_1:
            // Attempt 1: Make use of integer division (crude version that fails if TARGET > 1000)
            for (num=1; num < TARGET; num++)
            {
                if (((num % 5) == 0) && ((num % 10) != 0))
                    vecResults.push_back(num);
                else
                {
                    tmp = (num / 10);
                    if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                        vecResults.push_back(num);
                    else
                    {
                        tmp = (num / 100);
                        if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                            vecResults.push_back(num);
                    }
                }
            }
            break;

        case ATTEMPT_2:
            // Attempt 2: Same version but saves values to a vector
            for (num=1; num < TARGET; num++)
            {
                if (((num % 5) == 0) && ((num % 10) != 0))
                    vecResults.push_back(num);
                else
                {
                    tmp = num;
                    do
                    {
                        tmp = (tmp / 10);
                        if (((tmp % 5) == 0) && ((tmp % 10) != 0))
                        {
                            vecResults.push_back(num);
                            tmp = 0;
                        }
                    } while (tmp > 0);
                }
            }
            break;

        case ATTEMPT_3:
            // Attempt 3: Using integer division but just looking for 5s
            for (num=1; num < TARGET; num++)
            {
                if ((num % 10) == 5)
                    vecResults.push_back(num);
                else
                {
                    tmp = (num / 10);
                    while (tmp > 0)
                    {
                        if ((tmp % 10) == 5)
                        {
                            vecResults.push_back(num);
                            break;
                        }

                        tmp /= 10;
                    }
                }
            }
            break;

        case ATTEMPT_4:
            // Attempt 3: Same version but saves values to a vector
            {
                char numAsChar[10];
                for (num=1; num < TARGET; num++)
                {
                    snprintf(numAsChar, 10, "%d", num);
                    if (strchr(numAsChar, '5') != NULL)
                        vecResults.push_back(num);
                }
            }
            break;

        case ATTEMPT_5:
            // Attempt 4: Same version but saves values to a vector
            {
                string numAsString;
                for (num=1; num < TARGET; num++)
                {
                    numAsString = to_string(num);
                    if (numAsString.find("5") != string::npos)
                        vecResults.push_back(num);
                }
            }
            break;


        default:
            cout << "\nUnknown attempt...\n";
             break;
    }

    // Show the results!
    if (print) {
        for (int pos=0; pos < vecResults.size(); pos++)
            cout << vecResults[pos] << " ";
    }
}

void ClassPuzzle001::PerformanceTests()
{
    // Test the performance of the various attempts
    cout << "\n\nShow performance of each attempt (no printing)\n";
    vector<int> vecNumbers;
    stringHelper strings;
    const int iterations = (20000000 / TARGET);
    int loop;

    double totalTimes[AttemptNumber::ATTEMPT_LAST];
    double averagePerAttempts[AttemptNumber::ATTEMPT_LAST];
    for (AttemptNumber attempt = AttemptNumber::ATTEMPT_1;
            attempt != AttemptNumber::ATTEMPT_LAST;
            attempt = static_cast<AttemptNumber>(static_cast<int>(attempt) + 1)) {
        auto timerStart = chrono::high_resolution_clock::now();
        for (loop=0; loop < iterations; loop++)
            DoAttempt(attempt, TARGET, false);

        auto timerEnd = chrono::high_resolution_clock::now();
        long timerDuration = chrono::duration_cast<chrono::nanoseconds>(timerEnd - timerStart).count();
        totalTimes[attempt] = static_cast<double>(timerDuration) / static_cast<double>(1000000000L);    // Convert to s
        averagePerAttempts[attempt] =
            (static_cast<double>(timerDuration) / static_cast<double>(iterations));
        averagePerAttempts[attempt] /= static_cast<double>(1000000L);   // Concert to ms
        cout << strings.formatString("  Attempt %d: Total %.3f s, Avg %.3f ms (%d)\n",
            attempt, totalTimes[attempt],  averagePerAttempts[attempt], iterations);
    }
}
