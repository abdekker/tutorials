#include <iostream>
#include <vector>

#include "..\..\Utils\dataTypes.h"
#include "..\..\Utils\mathHelper.h"
#include "..\..\Utils\stringHelper.h"
#include "..\..\Utils\systemHelper.h"

using namespace std;
mathHelper m_mHelper;
stringHelper m_sHelper;

void CountBits()
{
    cout << "# Count the number of bits set in a bitmask #\n";
    ARRAY_U bitmask
    {
        0x00000000,     // 0
        0x00000001,     // 1
        0x01000071,     // 5
        0xFFFFFFFF      // 32
    };
    for (auto in : bitmask)
    {
        cout << m_sHelper.FormatString("  Algorithm 1: 0x%08x (%d bits are set)", in, m_mHelper.CountBitsSet_U1(in)) << endl;
        cout << m_sHelper.FormatString("  Algorithm 2: 0x%08x (%d bits are set)", in, m_mHelper.CountBitsSet_U2(in)) << endl;
    }
    cout << "#\n\n";
}

void FloatToInt()
{
    cout << "Representing a floating point number as an integer\n";
    cout << "(Uses IEEE 754 representation and assumption that floating and integer number sizes are the same)\n";
    cout << "(The IEEE 754 specification should ensure that if (a > b) then (int(a) > int(b))\n";
    cout << m_sHelper.FormatString("  Size of float, int:\t\t%d, %d\n  Size of double, __int64:\t%d, %d\n\n",
        sizeof(float),
        sizeof(int),
        sizeof(double),
        sizeof(__int64));

    cout << "(float => int)\n";
    vector<float> arrayFloats
    {
        0.0f,
        -0.1f,
        0.0000001f,
        0.99999991f,
        0.99999992f,
        1.0f,
        1.0000001f,
        FLT_MIN,
        -FLT_MAX,
        FLT_MAX
    };

    int index = 0;
    int int32Convert;
    for (auto in : arrayFloats)
    {
        int32Convert = *(int*)&in;
        if (index++ <= 7)
            cout << m_sHelper.FormatString("  %-30.8f: %d\n", in, int32Convert);
        else
            cout << m_sHelper.FormatString("  %-30.8e: %d\n", in, int32Convert);
    }
    cout << endl;

    cout << "(double => __int64)\n";
    vector<double> arrayDoubles
    {
        0.0,
        -0.1,
        0.0000000000000001,
        0.9999999999999998,
        0.9999999999999999,
        1.0,
        1.0000000000000002,
        DBL_MIN,
        -DBL_MAX,
        DBL_MAX
    };

     index = 0;
    __int64 int64Convert;
    for (auto in : arrayDoubles)
    {
        int64Convert = *(__int64*)&in;
        if (index++ <= 7)
            cout << m_sHelper.FormatString("  %-30.16f: %lld\n", in, int64Convert);
        else
            cout << m_sHelper.FormatString("  %-30.16e: %lld\n", in, int64Convert);
    }
    cout << endl;
}

int main()
{
    // Ensure the output buffer is flushed on each insertion operation
    systemHelper sysHelper;
    sysHelper.SetFlushOutputBuffer();

    cout << "### Collected maths and computer science tricks ###\n\n";
    CountBits();
    FloatToInt();

    cout << "\nAll done!\n";
}
