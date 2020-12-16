using System;
using System.Runtime.InteropServices;

namespace DataTypes_CSharp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("### Data types in C# v7.0+ in Visual Studio 2019 ###");
            Console.WriteLine("  (In C#, all types can be cast to \"object\")");
            TypeTester t = new TypeTester();

            // Show information about data types
            Console.WriteLine();
	        Console.WriteLine("### Integer ###\n");
	        Console.WriteLine("\t\tName\t\tFull Name\t\tSize (bytes)\tMin\t\t\tMax");
	        {
		        // bool (System.Boolean)
		        bool min = false;
		        bool max = true;
                Console.WriteLine("bool\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }

            {
                // byte (System.Byte)
		        byte min = byte.MinValue;
		        byte max = byte.MaxValue;
                Console.WriteLine("byte\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }

            {
                // short (System.Int16)
		        short min = short.MinValue;
		        short max = short.MaxValue;
                Console.WriteLine("short\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }

            {
                // int (System.Int32)
		        int min = int.MinValue;
		        int max = int.MaxValue;
                Console.WriteLine("int\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }

            {
                // long (System.Int64)
		        long min = long.MinValue;
		        long max = long.MaxValue;
                Console.WriteLine("long\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }
            Console.WriteLine();

            Console.WriteLine("### Floating point ###\n");
	        Console.WriteLine("\t\tName\t\tFull Name\t\tSize (bytes)\tMin\t\t\t\tMax");
	        {
		        // float (System.Single)
		        float min = float.MinValue;
		        float max = float.MaxValue;
                Console.WriteLine("float\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }

            {
		        // double (System.Double)
		        double min = double.MinValue;
		        double max = double.MaxValue;
                Console.WriteLine("double\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }

            {
		        // decimal (System.Double)
		        decimal min = decimal.MinValue;
		        decimal max = decimal.MaxValue;
                Console.WriteLine("decimal\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t{4}",
                    t.GetObjectName(min), t.GetObjectFullName(min), t.GetObjectSize(min), min, max);
	        }
            Console.WriteLine();

            // Complete!
            Console.Write("All done! Press any key to exit...");
            Console.ReadLine();
        }
    }

    class TypeTester
    {
        // Constructor
        public TypeTester() { }

        public int GetObjectSize(object obj)
        {
            int objSize = -1;
            try
            {
                // This fails for types such as "System.String"
                objSize = Marshal.SizeOf(obj);
            }
            catch { }
            return objSize;
        }

        public string GetObjectName(object obj)
        {
            Type objType = obj.GetType();
            return objType.Name;
        }

        public string GetObjectFullName(object obj)
        {
            Type objType = obj.GetType();
            return objType.FullName;
        }
    }
}
