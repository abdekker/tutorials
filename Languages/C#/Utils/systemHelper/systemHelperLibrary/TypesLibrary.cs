using System;
using System.Runtime.InteropServices;

namespace systemHelperLibrary
{
    public static class TypesLibrary
    {
        // Note: In C#, all types can be cast to "object"
        public static int GetObjectSize(object obj)
        {
            // Return the size of the object type (eg. 1 for "System.Byte")
            int objSize = -1;
            try
            {
                // This fails for types such as "System.String"...
                objSize = Marshal.SizeOf(obj);
            }
            catch { }
            return objSize;
        }

        public static string GetObjectName(object obj)
        {
            // Return the name of the object type (eg. "Byte")
            Type objType = obj.GetType();
            return objType.Name;
        }

        public static string GetObjectFullName(object obj)
        {
            // Return the full name of the object type (eg. "System.Byte")
            Type objType = obj.GetType();
            return objType.FullName;
        }

        public static bool IsValidEnumValue<T>(T value) where T : Enum
        {
            // This hack relies on three assumptions:
            // * Enum values in C# are only allowed to be integer (and nothing else)
            // * Enum names in C# must begin with an alphabetic character
            // * Enum names cannot begin with a minus sign

            // Note: "Enum.IsDefined(typeof(ENUM), VALUE)" may fail for enumerations with the [Flag] attribute
            var firstChar = value.ToString()[0];
            return (firstChar < '0' || firstChar > '9') && (firstChar != '-');
        }
    }
}