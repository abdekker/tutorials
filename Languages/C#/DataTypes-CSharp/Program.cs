using System;
using System.Runtime.InteropServices;

namespace DataTypes_CSharp
{
    class Program
    {
        // Class to test types in C#
        static void Main(string[] args)
        {
            Console.WriteLine("### Data types in C# v7.0+ in Visual Studio 2019 ###");
            Console.WriteLine("  (In C#, all types can be cast to \"object\")");

            // Show information about data types
            TypeTester t = new TypeTester();
            Console.WriteLine();
            t.Types_Integer();
            t.Types_FloatingPoint();

            // Complete!
            Console.Write("All done! Press any key to exit...");
            Console.ReadLine();
        }
    }

    class TypeTester
    {
        // Constructor
        public TypeTester() { }

        public void Types_Integer()
        {
            // Integers
            Console.WriteLine("### Integer ###\n");
            Console.WriteLine("\t\tName\t\tFull Name\t\tSize (bytes)\tMin\t\t\tMax");
            Console.WriteLine("  (signed)");
            {
                // bool (System.Boolean)
                bool min = false;
                bool max = true;
                Console.WriteLine("bool\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // sbyte (System.SByte)
                sbyte min = sbyte.MinValue;
                sbyte max = sbyte.MaxValue;
                Console.WriteLine("sbyte\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // short (System.Int16)
                short min = short.MinValue;
                short max = short.MaxValue;
                Console.WriteLine("short\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // int (System.Int32)
                int min = int.MinValue;
                int max = int.MaxValue;
                Console.WriteLine("int\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // long (System.Int64)
                long min = long.MinValue;
                long max = long.MaxValue;
                Console.WriteLine("long\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            Console.WriteLine();
            Console.WriteLine("  (unsigned)");
            {
                // byte (System.Byte)
                byte min = byte.MinValue;
                byte max = byte.MaxValue;
                Console.WriteLine("byte\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // ushort (System.UInt16)
                ushort min = ushort.MinValue;
                ushort max = ushort.MaxValue;
                Console.WriteLine("ushort\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // uint (System.UInt32)
                uint min = uint.MinValue;
                uint max = uint.MaxValue;
                Console.WriteLine("uint\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // ulong (System.UInt64)
                ulong min = ulong.MinValue;
                ulong max = ulong.MaxValue;
                Console.WriteLine("ulong\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }
            Console.WriteLine();
        }

        public void Types_FloatingPoint()
        {
            // Floating point numbers
            Console.WriteLine("### Floating point ###\n");
            Console.WriteLine("\t\tName\t\tFull Name\t\tSize (bytes)\tMin\t\t\t\tMax");
            {
                // float (System.Single)
                float min = float.MinValue;
                float max = float.MaxValue;
                Console.WriteLine("float\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // double (System.Double)
                double min = double.MinValue;
                double max = double.MaxValue;
                Console.WriteLine("double\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }

            {
                // decimal (System.Decimal)
                decimal min = decimal.MinValue;
                decimal max = decimal.MaxValue;
                Console.WriteLine("decimal\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t{4}",
                    GetObjectName(min), GetObjectFullName(min), GetObjectSize(min), min, max);
            }
            Console.WriteLine();
        }

        private int GetObjectSize(object obj)
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

        private string GetObjectName(object obj)
        {
            Type objType = obj.GetType();
            return objType.Name;
        }

        private string GetObjectFullName(object obj)
        {
            Type objType = obj.GetType();
            return objType.FullName;
        }
    }
}
