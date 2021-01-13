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
    }
}