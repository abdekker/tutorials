#include <iostream>

#include "..\..\Utils\dataTypes.h"
#include "..\..\Utils\mathHelper.h"
#include "..\..\Utils\stringHelper.h"
#include "..\..\Utils\systemHelper.h"

using namespace std;
int main()
{
    // Ensure the output buffer is flushed on each insertion operation
    systemHelper sysHelper;
    sysHelper.SetFlushOutputBuffer();

    cout << "### Collected maths and computer science tricks ###\n";
    mathHelper mHelper;
    stringHelper sHelper;

    {
        cout << "\nCount the number of bits set in a bitmask\n";
        ARRAY_U bitmask{
            0x00000000,     // 0
            0x00000001,     // 1
            0x01000071,     // 5
            0xFFFFFFFF      // 32
            };
        for (auto in : bitmask)
        {
            cout << sHelper.formatString("  Algorithm 1: 0x%08x (%d bits are set)", in, mHelper.CountBitsSet_U1(in)) << endl;
            cout << sHelper.formatString("  Algorithm 2: 0x%08x (%d bits are set)", in, mHelper.CountBitsSet_U2(in)) << endl;
        }
    }

    cout << "\nAll done!\n";
}
