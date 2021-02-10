using System;
using System.Collections.Generic;
using System.Threading;

namespace systemHelperLibrary
{
    public static class StringLibrary
    {
        public static bool StartsWithUpper(this string s)
        {
            // "Help" returns true, and "help" returns false.
            if (string.IsNullOrWhiteSpace(s))
                return false;

            char ch = s[0];
            return char.IsUpper(ch);
        }

        public static string GetRandomString(byte length, bool lettersOnly, bool removeIllegal, int seed = 0)
        {
            // Generate a randomised string
            string sRandom = string.Empty;
            length = MathLibrary.Clamp<byte>(length, 1, 200);

            // Assign allowed characters to a temporary array
            byte start, end, character;
            List<char> allowed = new List<char>();
            List<char> forbidden = new List<char>();
            if (lettersOnly)
            {
                // Lowercase letters only
                start = 97;     // a
                end = 122;      // z
            }
            else
            {
                // Any printable ASCII character
                start = 33;     // !
                end = 126;      // ~

                // Remove illegal characters? Use this if the random string will be used to create a file/folder in
                // the file system. Forbidden printable ASCII characters are:
                // * Windows:   \ / : * ? " < > |
                // * MacOS:     :
                // * Linux:     /
                if (removeIllegal)
                {
                    // Use the Windows superset because it covers MacOS and Linux too
                    forbidden.Add('\\');
                    forbidden.Add('/');
                    forbidden.Add(':');
                    forbidden.Add('*');
                    forbidden.Add('?');
                    forbidden.Add('"');
                    forbidden.Add('<');
                    forbidden.Add('>');
                    forbidden.Add('|');
                }
            }

            if (forbidden.Count > 0)
            {
                char toAdd;
                for (character = start; character <= end; character++)
                {
                    toAdd = (char)character;
                    if (!forbidden.Contains(toAdd))
                        allowed.Add((char)character);
                }
            }
            else
            {
                for (character = start; character <= end; character++)
                    allowed.Add((char)character);
            }

            // Create a random number generator (with optional seed for performance and test)
            Random rnd;
            if (seed == 0)
            {
                Thread.Sleep(1);    // Ensure that calls made in close succession have different seeds
                rnd = new Random();
            }
            else
                rnd = new Random(seed);

            // Build randomised string
            for (character = 0; character < length; character++)
                sRandom += allowed[rnd.Next(int.MaxValue) % allowed.Count];

            return sRandom;
        }

        public static string FormatFloat<T>(T value, int precision)
        {
            // Generic method to format a floating point number
            // Note: String interpolation replaces double braces "{{" with a single brace
            return string.Format($"{{0:F{precision}}}", value);

            // Alternative:
            //  string formatString = string.Concat("{0:F", precision, "}");
            //  return string.Format(formatString, value);
        }
    }
}
