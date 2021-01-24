using System;
using System.Runtime.InteropServices;

using typesLib = systemHelperLibrary.TypesLibrary;

namespace DataTypes_CSharp
{
    class Program
    {
        // Class to test types in C#
        static void Main(string[] args)
        {
            Console.WriteLine("### Data types in C# v7.0+ in Visual Studio 2019 ###");
            Console.WriteLine();

            // Show information about data types
            TypeTester t = new TypeTester();
            t.Types_Integer();
            t.Types_FloatingPoint();
            t.Types_TypeConversions();

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
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // sbyte (System.SByte)
                sbyte min = sbyte.MinValue;
                sbyte max = sbyte.MaxValue;
                Console.WriteLine("sbyte\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // short (System.Int16)
                short min = short.MinValue;
                short max = short.MaxValue;
                Console.WriteLine("short\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // int (System.Int32)
                int min = int.MinValue;
                int max = int.MaxValue;
                Console.WriteLine("int\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // long (System.Int64)
                long min = long.MinValue;
                long max = long.MaxValue;
                Console.WriteLine("long\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            Console.WriteLine();
            Console.WriteLine("  (unsigned)");
            {
                // byte (System.Byte)
                byte min = byte.MinValue;
                byte max = byte.MaxValue;
                Console.WriteLine("byte\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // ushort (System.UInt16)
                ushort min = ushort.MinValue;
                ushort max = ushort.MaxValue;
                Console.WriteLine("ushort\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // uint (System.UInt32)
                uint min = uint.MinValue;
                uint max = uint.MaxValue;
                Console.WriteLine("uint\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // ulong (System.UInt64)
                ulong min = ulong.MinValue;
                ulong max = ulong.MaxValue;
                Console.WriteLine("ulong\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
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
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // double (System.Double)
                double min = double.MinValue;
                double max = double.MaxValue;
                Console.WriteLine("double\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }

            {
                // decimal (System.Decimal)
                decimal min = decimal.MinValue;
                decimal max = decimal.MaxValue;
                Console.WriteLine("decimal\t\t{0}\t\t{1}\t\t{2}\t\t{3}\t{4}",
                    typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max);
            }
            Console.WriteLine();
        }

        public void Types_TypeConversions()
        {
            // Converting between types
            Console.WriteLine("### Type Conversions ###");
            Console.WriteLine("  (In C#, all types can be cast to \"object\")\n");
            Console.WriteLine("\t\t\t\t\tValue\t\tType");
            Int16 myInt16 = 3;
            object objInt16 = myInt16;
            int myInt = (int)myInt16;

            Console.WriteLine("Int16\t\t\t\t\t{0}\t\t{1}",
                myInt16, typesLib.GetObjectFullName(myInt16));
            Console.WriteLine("Int16 (as object)\t\t\t{0}\t\t{1}",
                objInt16, typesLib.GetObjectFullName(objInt16));
            Console.WriteLine("int (cast from Int16)\t\t\t{0}\t\t{1}",
                myInt, typesLib.GetObjectFullName(myInt));
            Console.WriteLine();

            Console.WriteLine("  (Attempt cast from object of the wrong type)");
            try
            {
                myInt = (int)objInt16;
                Console.WriteLine("Success!...though you shouldn't get here");
            }
            catch (Exception e)
            {
                // The case fails because you are attempting to unbox an "int", so the object cannot be just anything that
                // can be converted to an "int".
                Console.WriteLine("Exception: {0}", e.Message);
                Console.WriteLine("  (The correct way to do this is to use \"Convert\")");
                myInt =  Convert.ToInt32(objInt16);     // Or: (int)Convert.ChangeType(objInt16, typeof(int));
                Console.WriteLine("int (cast from object as Int16)\t\t{0}\t\t{1}",
                    myInt, typesLib.GetObjectFullName(myInt));
            }
            Console.WriteLine();
        }
    }
}
