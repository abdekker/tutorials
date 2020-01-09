#include <iostream>

using namespace std;
int main()
{
    // See ../Puzzles/Notes-Puzzles.txt for details

    cout << "Print all numbers up to 1000 that have the digit 5 in their number";
    const int target = 1000;

    cout << "\n\nAttempt 1 (making use of integer division 1)\n";
    int num, tmp;
    for (num=1; num < target; num++)
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

    cout << "\n\nAttempt 2 (making use of integer division 2)\n";
    for (num=1; num < target; num++)
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

    cout << "\n\nAll done!\n";
}
