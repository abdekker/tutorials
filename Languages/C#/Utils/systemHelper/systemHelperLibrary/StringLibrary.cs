namespace systemHelperLibrary
{
    public static class StringLibrary
    {
        public static bool StartsWithUpper(this string str)
        {
            // "Help" returns true, and "help" returns false.
            if (string.IsNullOrWhiteSpace(str))
                return false;

            char ch = str[0];
            return char.IsUpper(ch);
        }
    }
}
