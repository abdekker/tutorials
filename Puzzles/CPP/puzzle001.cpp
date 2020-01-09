#include <iostream>

using namespace std;
int main()
{
    // See ../Puzzles/Notes-Puzzles.txt for details

    // Print all numbers up to 1000 that have the digit 5 in their number

    cout << "Attempt 1\n";
    bool bPrintNum = false;
    int tmp = 0;
    for (int num=1; num < 1000; num++)
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

    cout << "\n\nAll done!\n";
}
