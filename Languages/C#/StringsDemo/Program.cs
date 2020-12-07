using System;
//using System.Collections.Generic;
using System.Globalization;

namespace StringsDemo
{
    class Program
    {
        private static void WriteConsole(string value)
        {
            Console.WriteLine(value);
        }

        private static string FormatFloat<T>(T value, int precision)
        {
            // Generic method to format a floating point number
            // Note: String interpolation replaces double braces "{{" with a single brace
            return string.Format($"{{0:F{precision}}}", value);

            // Alternative:
            /*string formatString = string.Concat("{0:F", precision, "}");
            return string.Format(formatString, value);*/
        }

        private static void OutputInfo_Basic()
        {
            // Basic information on formatting (see each section for more detailed examples)
            Console.WriteLine("# Basics #");
            WriteConsole("  * Use {0} for the 1st parameter, {1} for the 2nd, and so on");
            WriteConsole("    Note: Use \"$\" for string literals. The following are equivalent.");
            WriteConsole("          Console.WriteLine(\"My integer is {0} and I like it\", myInt);");
            WriteConsole("          Console.WriteLine($\"My integer is {myInt} and I like it\");");
            Console.WriteLine();

            Console.WriteLine("  * Integers");
            WriteConsole("    - {0:000} adds leading zeroes to pad the number to a fixed length (eg. 3)");
            WriteConsole("    - {0:D3} achieves the same thing");
            WriteConsole("    - {0,3} adds leading spaces to pad the number to a fixed length");
            WriteConsole("    - {0,3:00} adds leading zeroes, then spaces to pad the number to a fixed length");
            WriteConsole("    - {0:X2} displays the number in hexadecimal");
            Console.WriteLine();

            WriteConsole("  * Floats...TODO");
            Console.Write(Environment.NewLine);
        }

        private static void OutputInfo_String()
        {
            // Formatting strings
            // Note: You can also use the "string.Format(...)" methods
            Console.WriteLine("# Strings #");
            string s1 = "AAA";
            string s2 = "zzz";
            Console.WriteLine("(A = {0}, B = {1})", s1, s2);
            Console.WriteLine("  Add together = {0}\t\t[using the \"+\" operator]", (s1 + s2));
            Console.WriteLine("  Add together = {0}\t\t[using string::Concat]", string.Concat(s1, s2));
            Console.Write(Environment.NewLine);
        }

        private static void OutputInfo_Integer()
        {
            // Formatting integers
            Console.WriteLine("# Integers #");
            int n1 = 0;
            int n2 = -13;
            int n3 = 12345;
            Console.WriteLine("  A = {0}, B = {1}, C = {2}\t\t\t[raw]", n1, n2, n3);
            Console.WriteLine("  A = {0:00000000}, B = {1:00000000}, C = {2:00000000}\t[leading zeroes using \"0:0...\"]", n1, n2, n3);
            Console.WriteLine("  A = {0:D8}, B = {1:D8}, C = {2:D8}\t[leading zeroes using \"0:D*\"]", n1, n2, n3);
            Console.WriteLine("  A = {0,8}, B = {1,8}, C = {2,8}\t[leading spaces using \"0,*\"]", n1, n2, n3);
            Console.WriteLine("  A = {0,-8}, B = {1,-8}, C = {2,-8}\t[trailing spaces using \"0,-*\"]", n1, n2, n3);
            Console.WriteLine("  A = {0,8:0000}, B = {1,8:0000}, C = {2,8:0000}\t[leading zeroes and spaces using \"0,*:0...\"]", n1, n2, n3);
            Console.WriteLine("  A = {0:N0}, B = {1:N0}, C = {2:N0}\t\t\t[thousand separator using \"0:N0\"]", n1, n2, n3);
            Console.WriteLine("  A = {0:#,0}, B = {1:#,0}, C = {2:#,0}\t\t\t[thousand separator using \"0:#,0\"]", n1, n2, n3);
            Console.Write(Environment.NewLine);

            Console.WriteLine("(displaying numbers in hex using 0:X*)");
            byte u1 = 10;
            uint u2 = uint.MaxValue;
            Console.WriteLine("  {0:D2} = 0x{1:X2}", u1, u1);
            Console.WriteLine("  {0:D8} = 0x{1:X8}", u2, u2);
            Console.Write(Environment.NewLine);
        }

        private static void OutputInfo_Float()
        {
            // Formatting floating point numbers (of type "float")
            Console.WriteLine("# Floats #");
            float f1 = 0.0f;
            float f2 = -3.14159265359f;
            float f3 = 123456.789f;
            Console.WriteLine("  A = {0}, B = {1}, C = {2}\t\t[raw]", f1, f2, f3);
            Console.WriteLine("  A = {0:0.###}, B = {1:0.###}, C = {2:0.###}\t\t[max decimal places using \"0:0.#...\"]", f1, f2, f3);
            Console.WriteLine("  A = {0:0.000}, B = {1:0.000}, C = {2:0.000}\t\t[fixed decimal places using \"0:0.0...\"]", f1, f2, f3);
            Console.WriteLine("  A = {0:N3}, B = {1:N3}, C = {2:N3}\t[thousand separator and fixed dps using \"0:N*\"]", f1, f2, f3);
            Console.Write(Environment.NewLine);

            Console.WriteLine("(using a method to display a variable number of decimal places)");
            for (int places = 0; places <= 5; places++)
                Console.WriteLine("  decimals = {0}, output = {1}", places, FormatFloat(f2, places));
            Console.Write(Environment.NewLine);

            Console.WriteLine("(using different formatting for positive and negative numbers, and zero)");
            string formatter = "{0:0.000;(0.000);zero}";    // The string "zero" can be anything
            Console.WriteLine("  A = {0}, B = {1}, C = {2}", 
                string.Format(formatter, f1),
                string.Format(formatter, f2),
                string.Format(formatter, f3));
            Console.Write(Environment.NewLine);

            Console.WriteLine("(pre-defined formats for floating point numbers)");
            Console.WriteLine("(generally two decimal places; add a number after the format symbol for different dps)");
            f1 = 0.1532f;
            Console.WriteLine("  A = {0:0.0000}\t\t\t[original]", f1);
            Console.WriteLine("  A = {0:C}\t\t\t[using 0:C]", f1);
            Console.WriteLine("  A = {0:E}\t\t[using 0:E]", f1);
            Console.WriteLine("  A = {0:F}\t\t\t[using 0:F]", f1);
            Console.WriteLine("  A = {0:G}\t\t\t[using 0:G]", f1);
            Console.WriteLine("  A = {0:N}\t\t\t[using 0:N]", f1);
            Console.WriteLine("  A = {0:P}\t\t\t[using 0:P]", f1);
            Console.Write(Environment.NewLine);
        }

        private static void OutputInfo_Double()
        {
            // Formatting floating point numbers (of type "double")
            // Note: Similar to float, so this section is simpler
            Console.WriteLine("# Doubles # (similar to floats)");
            double d = 123.0987654321;
            Console.WriteLine("  A = {0} [raw]", d);
            Console.Write(Environment.NewLine);

            Console.WriteLine("(using a method to display a variable number of decimal places)");
            for (int places = 0; places <= 10; places++)
                Console.WriteLine("  decimals = {0,2}, output = {1}", places, FormatFloat(d, places));

            Console.Write(Environment.NewLine);
        }

        static void Main(string[] args)
        {
            // Demonstrating some aspects of string formatting in C#
            Console.WriteLine("=== Strings and string formatting in C# ===");
            Console.Write(Environment.NewLine);
            OutputInfo_Basic();
            OutputInfo_String();
            OutputInfo_Integer();
            OutputInfo_Float();
            OutputInfo_Double();
            Console.ReadKey(false);
        }
    }
}
