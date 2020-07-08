#include "stdafx.h"
#include "puzzle0001.h"

// Print all numbers up to 1000 that have the digit 5 in their number

// Source: Teraki coding test
const int TARGET = 1000;

// For performance timing, change this value. Use a larger value on faster hardware.
//const int TIMING_TOTAL_ITERATIONs = 20000000;		// Faster HW
const int TIMING_TOTAL_ITERATIONs = 5000000;		// Slower HW
const int TIMING_ITERATIONS = (TIMING_TOTAL_ITERATIONs / TARGET);

using namespace std;
void ClassPuzzle0001::RunPuzzle()
{
	// This class was a standalone console application and this function prototype used to be:
	//		int main()
	cout << "Print all numbers up to " << TARGET << " that have the digit 5 in their number";

	cout << "\n\nAttempt 1 (use integer division 1)\n";
	DoAttempt(AttemptNumber::ATTEMPT_1, TARGET, true);
	cout << "\n\nAttempt 2 (use integer division 2)\n";
	DoAttempt(AttemptNumber::ATTEMPT_2, TARGET, true);
	cout << "\n\nAttempt 3 (use integer division 3)\n";
	DoAttempt(AttemptNumber::ATTEMPT_3, TARGET, true);
	cout << "\n\nAttempt 4 (convert to char *)\n";
	DoAttempt(AttemptNumber::ATTEMPT_4, TARGET, true);
	cout << "\n\nAttempt 5 (convert to std::string)\n";
	DoAttempt(AttemptNumber::ATTEMPT_5, TARGET, true);

	// Compare the performance of each method
	PerformanceTests();
	cout << "\nAll done!\n";
}

// Start: Private methods
void ClassPuzzle0001::DoAttempt(const AttemptNumber attempt, const int target, const bool print)
{
	// Attempt the problem...
	vector<int> vecResults;
	int num, tmp;
	switch (attempt)
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
			// Attempt 2: Same as attempt 1 but works for general TARGET
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
			// Attempt 4: Convert numbers to char*
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
			// Attempt 5: Convert numbers to std::string (rather slow!)
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
		for (size_t pos=0; pos < vecResults.size(); pos++)
			cout << vecResults[pos] << " ";
	}
}

void ClassPuzzle0001::PerformanceTests()
{
	// Test the performance of the various attempts
	cout << "\n\nPerformance of each attempt (no printing) [" << TIMING_ITERATIONS << " iterations]\n";
	vector<int> vecNumbers;
	stringHelper strings;
	int loop;

	double totalTimes[AttemptNumber::ATTEMPT_LAST];
	double averagePerAttempts[AttemptNumber::ATTEMPT_LAST];
	for (AttemptNumber attempt = AttemptNumber::ATTEMPT_1;
			attempt != AttemptNumber::ATTEMPT_LAST;
			attempt = static_cast<AttemptNumber>(static_cast<int>(attempt) + 1)) {
		auto timerStart = chrono::high_resolution_clock::now();
		for (loop=0; loop < TIMING_ITERATIONS; loop++)
			DoAttempt(attempt, TARGET, false);

		auto timerEnd = chrono::high_resolution_clock::now();
		long long timerDuration = chrono::duration_cast<chrono::nanoseconds>(timerEnd - timerStart).count();
		// Note: Be careful of the type declaration! On {gcc, Linux, x64} systems, "long" is of
		// type "__int64". But on {MSVC, Windows, xAny}, it is of type "__int32". Use "auto" or
		// define the type as "long long".

		totalTimes[attempt] = static_cast<double>(timerDuration) / static_cast<double>(1000000000L);    // Convert to s
		averagePerAttempts[attempt] =
			(static_cast<double>(timerDuration) / static_cast<double>(TIMING_ITERATIONS));
		averagePerAttempts[attempt] /= static_cast<double>(1000000L);   // Concert to ms
		cout << strings.formatString("  Attempt %d: Total %.3f s, Avg %.3f ms\n",
			attempt, totalTimes[attempt],  averagePerAttempts[attempt]);
	}
}
// End: Private methods
