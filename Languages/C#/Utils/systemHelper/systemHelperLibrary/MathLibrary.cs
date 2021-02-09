using System;

namespace systemHelperLibrary
{
    public static class MathLibrary
    {
        public static T Clamp<T>(this T val, T min, T max) where T : IComparable<T>
        {
            // Similar to std::clamp. Note that .NET 2.0 Core has the System.Math.Clamp method which may
            // make this obsolete in future. To use: byte myVal = Clamp<byte>(anotherVal, 1, 100);
            if (val.CompareTo(min) < 0)
                return min;
            else if (val.CompareTo(max) > 0)
                return max;
            else
                return val;
        }
    }
}
