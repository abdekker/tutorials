using System;
using System.Globalization;
using System.Linq;
using System.Text;

using systemHelperLibrary;

namespace StringsDemo
{
    class Program
    {
        #region Constants
        // Date/Time separation for strings in ISO 8601 format (yyyy-MM-ddTHH:mm:ss)
        public static char[] cDateTimeSeparator = { 'T' };
        public const string szDateTimeFormat_ISO8601 = "yyyy-MM-ddTHH:mm:ss";
        #endregion // Constants

        #region Enumerations
        private enum Weekdays
        {
            Monday = 0,
            Tuesday,
            Wednesday,
            Thursday,
            Friday,
            Saturday,
            Sunday
        };

        [Flags]
        private enum EnumWithFlags
        {
            First = 1,
            Second = 2,
            Third = 4,
            Fourth = 8
        };

        private enum EnumWithNegativeNumbers
        {
            First = -2,
            Second,
            Third,
            Fourth
        };
        #endregion // Enumerations

        #region Main test methods
        private static void Info_Basic()
        {
            // Basic information on formatting (see each section for more detailed examples)
            Console.WriteLine("# Basics #");
            Console.WriteLine("  * Use {0} for the 1st parameter, {1} for the 2nd, and so on");
            Console.WriteLine("    Note: Use \"$\" for string literals. The following are equivalent.");
            Console.WriteLine("          Console.WriteLine(\"My integer is {0} and I like it\", myInt);");
            Console.WriteLine("          Console.WriteLine($\"My integer is {myInt} and I like it\");");
            Console.WriteLine();

            Console.WriteLine("  * Integers");
            Console.WriteLine("    - {0:000} adds leading zeroes to pad the number to a fixed length (eg. 3)");
            Console.WriteLine("    - {0:D3} achieves the same thing");
            Console.WriteLine("    - {0,3} adds leading spaces to pad the number to a fixed length");
            Console.WriteLine("    - {0,3:00} adds leading zeroes, then spaces to pad the number to a fixed length");
            Console.WriteLine("    - {0:X2} displays the number in hexadecimal");
            Console.WriteLine();

            Console.WriteLine("  * Floats...TODO");
            Console.WriteLine();
        }

        private static void Info_String()
        {
            // Formatting strings
            // Note: You can also use the "string.Format(...)" methods
            Console.WriteLine("# Strings #");
            string s1 = "AAA";
            string s2 = "zzz";
            Console.WriteLine("(A = {0}, B = {1})", s1, s2);
            Console.WriteLine("  Add together = {0}\t\t[using the \"+\" operator]", (s1 + s2));
            Console.WriteLine("  Add together = {0}\t\t[using string::Concat]", string.Concat(s1, s2));
            Console.WriteLine();

            Console.WriteLine("(string padding)");
            Console.WriteLine("  Left padding with {{0,N}}\t!{0,8}!", s1);
            Console.WriteLine("  Right padding with {{0,-N}}\t!{0,-8}!", s1);
            Console.WriteLine();

            // Note: In the loop below we should check for outbreaks (where "16-name.Length" is negative)
            Console.WriteLine("(output strings in a loop with padding)");
            Console.WriteLine("  (using String.PadLeft)");
            string [] names = { "A", "Bob", "Charles" };
            foreach (string name in names)
                Console.WriteLine("    {0}{1}", name, name.PadLeft(16-name.Length, ' '));

            Console.WriteLine("  (using string interpolation using the \"$\" character)");
            foreach (string name in names)
                 Console.WriteLine($"    {{0}}{{0,{16-name.Length}}}", name);

            Console.WriteLine();

            string s3 = "AAA";
            string s4 = "aaa";
            Console.WriteLine("(string comparisons)");
            Console.WriteLine("(A = {0}, B = {1}, C = {2}, D = {3})", s1, s2, s3, s4);
            Console.WriteLine("  (A == B):\t{0}\t[using operator==]", (s1 == s2));
            Console.WriteLine("  (A == C):\t{0}", (s1 == s3));
            Console.WriteLine("  (A == D):\t{0}", (s1 == s4));
            Console.WriteLine();
            Console.WriteLine("  (A == B):\t{0}\t[using string::Compare (case-sensitive)]", (string.Compare(s1, s2) == 0));
            Console.WriteLine("  (A == C):\t{0}", (string.Compare(s1, s3) == 0));
            Console.WriteLine("  (A == D):\t{0}", (string.Compare(s1, s4) == 0));
            Console.WriteLine();
            Console.WriteLine("  (A == B):\t{0}\t[using string::Compare (ignore case)]", (string.Compare(s1, s2, true) == 0));
            Console.WriteLine("  (A == C):\t{0}", (string.Compare(s1, s3, true) == 0));
            Console.WriteLine("  (A == D):\t{0}", (string.Compare(s1, s4, true) == 0));
            Console.WriteLine();
            Console.WriteLine("  (A == B):\t{0}\t[using string::Equals (case-sensitive)]", s1.Equals(s2));
            Console.WriteLine("  (A == C):\t{0}", s1.Equals(s3));
            Console.WriteLine("  (A == D):\t{0}", s1.Equals(s4));
            Console.WriteLine();
            Console.WriteLine("  (A == B):\t{0}\t[using string::Equals (ignore case)]", s1.Equals(s2, StringComparison.OrdinalIgnoreCase));
            Console.WriteLine("  (A == C):\t{0}", s1.Equals(s3, StringComparison.OrdinalIgnoreCase));
            Console.WriteLine("  (A == D):\t{0}", s1.Equals(s4, StringComparison.OrdinalIgnoreCase));
            Console.WriteLine();

            char c1 = 'a';
            int repeat = 10;
            Console.WriteLine("(repeating character '{0}' {1} times)", c1, repeat);
            Console.WriteLine("  {0} [new String(char, int)]", new String(c1, repeat));
            Console.WriteLine("  {0} [string.Empty.PadLeft(int, char)]", string.Empty.PadLeft(repeat, c1));
            Console.WriteLine("  {0} [System.Linq.Enumerable.Repeat(T, int)]", string.Concat(Enumerable.Repeat(c1, repeat)));
            Console.WriteLine("  {0} [new System.Text.StringBuilder(int).Append(char, int)]", new StringBuilder(repeat).Append(c1, repeat));
            Console.WriteLine();

            string s5 = "xy";
            repeat = 5;
            Console.WriteLine("(repeating string \"{0}\" {1} times)", s5, repeat);
            Console.WriteLine("  {0} [new String(char, int).Replace(string, string)]", new String('+', repeat).Replace("+", s5));
            Console.WriteLine("  {0} [new System.Text.StringBuilder(int).Insert(...)]", new StringBuilder(s5.Length * repeat).Insert(0, s5, repeat).ToString());
            Console.WriteLine("  {0} [string.Concat(System.Linq.Enumerable.Repeat(string, int))]", string.Concat(Enumerable.Repeat(s5, repeat)));
            Console.WriteLine("  {0} [new String(System.Linq.Enumerable.Range(...)) (super weird)]", new String(Enumerable.Range(0, repeat).SelectMany(x => s5).ToArray()));
            Console.WriteLine();
        }

        private static void Info_Boolean()
        {
            // Formatting boolean values
            Console.WriteLine("# Booleans #");
            bool bFalse = false;
            bool bTrue = true;
            int nonZero = 1;
            Console.WriteLine("(A = {0}, B = {1}, C = {2})", bFalse, bTrue, nonZero);
            Console.WriteLine("  (A):\t\t{0}", bFalse);
            Console.WriteLine("  (B):\t\t{0}", bTrue);
            Console.WriteLine("  (C == 0):\t{0}", (nonZero == 0));
            Console.WriteLine("  (C != 0):\t{0}", (nonZero != 0));
            Console.WriteLine();
        }

        private static void Info_Integer()
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
            Console.WriteLine();

            Console.WriteLine("(displaying numbers in hex using 0:X*)");
            byte u1 = 10;
            uint u2 = uint.MaxValue;
            Console.WriteLine("  {0:D2} = 0x{1:X2}", u1, u1);
            Console.WriteLine("  {0:D8} = 0x{1:X8}", u2, u2);
            Console.WriteLine();
        }

        private static void Info_Float()
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
            Console.WriteLine();

            Console.WriteLine("(using a method to display a variable number of decimal places)");
            for (int places = 0; places <= 5; places++)
                Console.WriteLine("  decimals = {0}, output = {1}", places, StringLibrary.FormatFloat(f2, places));
            Console.WriteLine();

            Console.WriteLine("(using different formatting for positive and negative numbers, and zero)");
            string formatter = "{0:0.000;(0.000);zero}";    // The string "zero" can be anything
            Console.WriteLine("  A = {0}, B = {1}, C = {2}",
                string.Format(formatter, f1),
                string.Format(formatter, f2),
                string.Format(formatter, f3));
            Console.WriteLine();

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
            Console.WriteLine();

            Console.WriteLine("(list of basic methods to format a floating point number)");
            Console.WriteLine("  A = {0:0.0000}\t\t\t[using \"{{0:0.0...}}\"]", f1);
            Console.WriteLine("  A = {0}\t\t\t[custom function using \"{{0:F*}}\" internally]", StringLibrary.FormatFloat(f1, 4));
            Console.WriteLine("  A = {0}\t\t\t[using System.Single::ToString(format)]", f1.ToString("0.0000"));
            Console.WriteLine();
        }

        private static void Info_Double()
        {
            // Formatting floating point numbers (of type "double")
            // Note: Similar to float, so this section is simpler
            Console.WriteLine("# Doubles # (similar to floats)");
            double d = 123.0987654321;
            Console.WriteLine("  A = {0} [raw]", d);
            Console.WriteLine();

            Console.WriteLine("(using a method to display a variable number of decimal places)");
            for (int places = 0; places <= 10; places++)
                Console.WriteLine("  decimals = {0,2}, output = {1}", places, StringLibrary.FormatFloat(d, places));

            Console.WriteLine();
        }

        private static void Info_DateTime()
        {
            // Formatting System.Date variables
            // Note: Date/Time variables are specific to culture/locale. Change locale like this:
            //      Thread.CurrentThread.CurrentCulture = new CultureInfo("fr-FR");
            Console.WriteLine("# Date/Time #");
            DateTime dtA = DateTime.Now;
            DateTime dtB = new DateTime(2008, 1, 20, 12, 34, 56);   // Constructed dates
            DateTime dtC;
            DateTime.TryParse("2008-01-20T12:34:56", out dtC);
            Console.WriteLine("  A (now) = {0}", dtA);
            Console.WriteLine("  B (from numbers) = {0}, C (from string) = {0}", dtB, dtC);
            Console.WriteLine("  Is B == C? {0}", ( dtB.CompareTo(dtC) == 0));
            Console.WriteLine("  ISO8601 (B) = {0}", dtB.ToString(szDateTimeFormat_ISO8601));
            Console.WriteLine();

            Console.WriteLine("(locale information)");
            Console.WriteLine("  current locale:\t\t{0}", CultureInfo.CurrentCulture.Name);
            Console.WriteLine("  short date pattern:\t\t{0} ({1})",
                CultureInfo.CurrentCulture.DateTimeFormat.ShortDatePattern,
                dtA.ToShortDateString());
            Console.WriteLine("  short time pattern:\t\t{0} ({1})",
                CultureInfo.CurrentCulture.DateTimeFormat.ShortTimePattern,
                dtA.ToShortTimeString());
            Console.WriteLine("  long date pattern:\t\t{0} ({1})",
                CultureInfo.CurrentCulture.DateTimeFormat.LongDatePattern,
                dtA.ToLongDateString());
            Console.WriteLine("  long time pattern:\t\t{0} ({1})",
                CultureInfo.CurrentCulture.DateTimeFormat.LongTimePattern,
                dtA.ToLongTimeString());
            Console.WriteLine("  full date pattern:\t\t{0} ({1})",
               CultureInfo.CurrentCulture.DateTimeFormat.FullDateTimePattern,
               dtA.ToString());
            Console.WriteLine("  time separator:\t\t{0}", CultureInfo.CurrentCulture.DateTimeFormat.TimeSeparator);
            Console.WriteLine("  date separator:\t\t{0}", CultureInfo.CurrentCulture.DateTimeFormat.DateSeparator);
            Console.WriteLine("  1st day of working week:\t{0}", CultureInfo.CurrentCulture.DateTimeFormat.FirstDayOfWeek);
            Console.WriteLine("  name of 1st day of week:\t{0}", CultureInfo.CurrentCulture.DateTimeFormat.DayNames[0]);
            Console.WriteLine("  name of 1st month of year:\t{0}", CultureInfo.CurrentCulture.DateTimeFormat.MonthNames[0]);
            Console.WriteLine("  rule for 1st week of year:\t{0}", CultureInfo.CurrentCulture.DateTimeFormat.CalendarWeekRule.ToString());
            Console.WriteLine();
        }

        private static void Info_Enum()
        {
            // Displaying enumerated values
            Console.WriteLine("# Enumerations #");
            Console.WriteLine("(Weekdays contains {0} values)", Enum.GetNames(typeof(Weekdays)).Length);
            foreach (var day in Enum.GetValues(typeof(Weekdays)))
            {
                Console.WriteLine("  {0,2}  {1}", (int)day, day);
            }
            Console.WriteLine();

            Console.WriteLine("(integer -> enum using Enum.IsDefined)");
            int [] dayNums = { 2, 3, -1, 8 };
            foreach (int dayNum in dayNums)
            {
                if (Enum.IsDefined(typeof(Weekdays), dayNum))
                {
                    Weekdays day = (Weekdays)dayNum;
                    Console.WriteLine("  {0,-10} converts to {1}", dayNum, day);
                }
                else
                    Console.WriteLine("  {0,-10} is not an underlying value of Weekday", dayNum);
            }
            Console.WriteLine();

            Console.WriteLine("(integer -> enum using naming hack)");
            foreach (int dayNum in dayNums)
            {
                if (TypesLibrary.IsValidEnumValue((Weekdays)dayNum))
                {
                    Weekdays day = (Weekdays)dayNum;
                    Console.WriteLine("  {0,-10} converts to {1}", dayNum, day);
                }
                else
                    Console.WriteLine("  {0,-10} is not an underlying value of Weekday", dayNum);
            }
            Console.WriteLine();

            Console.WriteLine("(string -> enum)");
            string[] dayStrings = { "2", "Friday", "8", "Blue" };
            string tmp;
            foreach (string dayString in dayStrings)
            {
                tmp = string.Format("'{0}'", dayString);
                Weekdays day;
                if (Enum.TryParse(dayString, true, out day))
                {
                    if (Enum.IsDefined(typeof(Weekdays), day) | day.ToString().Contains(","))
                        Console.WriteLine("  {0,-10} converts to {1}", tmp, day.ToString());
                    else
                        Console.WriteLine("  {0,-10} is not an underlying value of Weekdays", tmp);
                }
                else
                    Console.WriteLine("  {0,-10} is not a member of Weekdays", tmp);
            }
            Console.WriteLine();

            Console.WriteLine("(enum with FlagsAttribute)");
            for (int val = 0; val <= 16; val++)
                Console.WriteLine( "{0,3} - {1:G}", val, (EnumWithFlags)val);

            Console.WriteLine();
        }
        #endregion // Main test methods

        static void Main(string[] args)
        {
            // Demonstrating some aspects of string formatting in C#
            Console.WriteLine("=== Strings and string formatting in C# ===");
            Console.WriteLine();    // Or "Console.Write(Environment.NewLine)"
            Info_Basic();
            Info_String();
            Info_Boolean();
            Info_Integer();
            Info_Float();
            Info_Double();
            Info_DateTime();
            Info_Enum();
            Console.WriteLine("All done...press any key to continue");
            Console.ReadKey(false);
        }
    }
}
