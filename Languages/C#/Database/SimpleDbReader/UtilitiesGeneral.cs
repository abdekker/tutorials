namespace SimpleDbReader
{
    public class UtilitiesGeneral
    {
        // General utility class
        public static string FormatException(string className, string methodName, string exMessage)
        {
            // When an exception occurs, format the message for the console
            if ((string.IsNullOrEmpty(className) || string.IsNullOrEmpty(methodName)))
                return string.Format("Exception: {1}", exMessage);
            else
                return string.Format("Ex: {0}::{1}: {2}", className, methodName, exMessage);
        }
    }
}
