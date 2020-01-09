#include "puzzle001.h"

const int TARGET = 1000;

using namespace std;
int main()
{
    // See ../Puzzles/Notes-Puzzles.txt for details

    cout << "Print all numbers up to " << TARGET << " that have the digit 5 in their number";
    ClassPuzzle001 puzzle;  
    puzzle.Attempt1();
    puzzle.Attempt2();
    puzzle.Attempt3();
    puzzle.Attempt4();

    cout << "\n\nAll done!\n";
}

void ClassPuzzle001::Attempt1()
{
    // Attempt 1: Make use of integer division (crude version that fails if TARGET > 1000)
    cout << "\n\nAttempt 1 (use integer division 1)\n";
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

void ClassPuzzle001::Attempt2()
{
    // Attempt 2: Make use of integer division (sophisticated version that works for all values of TARGET)
    cout << "\n\nAttempt 2 (use integer division 2)\n";
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

void ClassPuzzle001::Attempt3()
{
    // Attempt 3: Convert number to char *
    cout << "\n\nAttempt 3 (convert to char *)\n";
    char numAsChar[10];
    for (int num=1; num < TARGET; num++)
    {
        snprintf(numAsChar, 10, "%d", num);
        if (strchr(numAsChar, '5') != NULL)
            cout << num << " ";
    }
}

void ClassPuzzle001::Attempt4()
{
    // Attempt 4: Convert number to std::string
    cout << "\n\nAttempt 4 (convert to std::string)\n";
    string numAsString;
    for (int num=1; num < TARGET; num++)
    {
        numAsString = to_string(num);
        if (numAsString.find("5") != string::npos)
            cout << num << " ";
    }
}
