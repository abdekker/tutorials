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
