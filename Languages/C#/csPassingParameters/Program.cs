using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PassingParameters
{
    class TestClass
    {
        public string myString;
        public int myInt;
    }

    struct TestStruct
    {
        public string willIChange;
    }

    class Program
    {
        #region Helper methods (integer)
        private static void PassIntegerNoRef(int input)
        {
            // "int" is a value type. Local changes do not affect the calling object.
            input = 27;
            Console.WriteLine("      Inside PassIntegerNoRef. Value:        {0}", input);
        }

        private static void PassIntegerWithRef(ref int input)
        {
            // "int" is a value type. Because we are passing by reference, local changes affect the calling object.
            input = 27;
            Console.WriteLine("      Inside PassIntegerWithRef. Value:      {0}", input);
        }

        private static void SwapIntegerNoRef(int inputA, int inputB)
        {
            // "int" is a value type. Local changes do not affect the calling objects, which are NOT swapped.
            int tmp = inputA;
            inputA = inputB;
            inputB = tmp;
            Console.WriteLine("      Inside SwapIntegerNoRef. Values:       {0}, {1}", inputA, inputB);
        }

        private static void SwapIntegerWithRef(ref int inputA, ref int inputB)
        {
            // "int" is a value type. Because we are passing by reference, local changes affect the calling objects,
            // which are swapped.
            int tmp = inputA;
            inputA = inputB;
            inputB = tmp;
            Console.WriteLine("      Inside SwapIntegerWithRef. Values:     {0}, {1}", inputA, inputB);
        }
        #endregion // Helper methods (integer)

        #region Helper methods (boolean)
        private static void PassBoolNoRef(bool input)
        {
            // "bool" is a value type, see "PassIntegerNoRef"
            input = !input;
            Console.WriteLine("      Inside PassBoolNoRef. Value:           {0}", input);
        }

        private static void PassBoolWithRef(ref bool input)
        {
            // "bool" is a value type, see "PassIntegerWithRef"
            input = !input;
            Console.WriteLine("      Inside PassBoolWithRef. Value:         {0}", input);
        }

        private static void SwapBoolNoRef(bool inputA, bool inputB)
        {
            // "bool" is a value type, see "SwapIntegerNoRef"
            bool tmp = inputA;
            inputA = inputB;
            inputB = tmp;
            Console.WriteLine("      Inside SwapBoolNoRef. Values:          {0}, {1}", inputA, inputB);
        }

        private static void SwapBoolWithRef(ref bool inputA, ref bool inputB)
        {
            // "bool" is a value type, see "SwapIntegerWithRef"
            bool tmp = inputA;
            inputA = inputB;
            inputB = tmp;
            Console.WriteLine("      Inside SwapBoolWithRef. Values:        {0}, {1}", inputA, inputB);
        }
        #endregion // Helper methods (boolean)

        #region Helper methods (string)
        private static void PassStringNoRef(string input)
        {
            // "string" is a reference type
            input = "freddy!";
            Console.WriteLine("      Inside PassStringNoRef. Value:         {0}", input);
        }

        private static void PassStringWithRef(ref string input)
        {
            input = "freddy!";
            Console.WriteLine("      Inside PassStringWithRef. Value:       {0}", input);
        }

        private static void SwapStringNoRef(string inputA, string inputB)
        {
            string tmp = inputA;
            inputA = inputB;
            inputB = tmp;
            Console.WriteLine("      Inside SwapStringNoRef. Values:        {0}, {1}", inputA, inputB);
        }

        private static void SwapStringWithRef(ref string inputA, ref string inputB)
        {
            string tmp = inputA;
            inputA = inputB;
            inputB = tmp;
            Console.WriteLine("      Inside SwapStringWithRef. Values:      {0}, {1}", inputA, inputB);
        }
        #endregion // Helper methods (string)

        private static void ValueTypes()
        {
            Console.WriteLine("### Value-type variables contains its data directly ###");
            Console.WriteLine("  (when passing by value, a copy of variable is passed to method)");
            Console.WriteLine("  (changes inside the method have no effect on the original data)");
            Console.WriteLine("  (to change the value of the parameter, you must pass by reference)");
            Console.WriteLine();

            Console.WriteLine("  # Integer #");
            int testInt = 13;
            Console.WriteLine("    Simple pass (by value). Before:          {0}", testInt);
            PassIntegerNoRef(testInt);
            Console.WriteLine("    Simple pass (by value). After:           {0}", testInt);
            Console.WriteLine();

            Console.WriteLine("    Simple pass (by reference). Before:      {0}", testInt);
            PassIntegerWithRef(ref testInt);
            Console.WriteLine("    Simple pass (by reference): After:       {0}", testInt);
            Console.WriteLine();

            int testIntA = 13;
            int testIntB = 77;
            Console.WriteLine("    Swap (by value). Before:                 {0}, {1}", testIntA, testIntB);
            SwapIntegerNoRef(testIntA, testIntB);
            Console.WriteLine("    Swap (by value). After:                  {0}, {1}", testIntA, testIntB);
            Console.WriteLine();

            Console.WriteLine("    Swap (by reference). Before:             {0}, {1}", testIntA, testIntB);
            SwapIntegerWithRef(ref testIntA, ref testIntB);
            Console.WriteLine("    Swap (by reference). After:              {0}, {1}", testIntA, testIntB);
            Console.WriteLine("  #");
            Console.WriteLine();

            Console.WriteLine("  # Boolean #");
            bool testBool = false;
            Console.WriteLine("    Simple pass (by value). Before:          {0}", testBool);
            PassBoolNoRef(testBool);
            Console.WriteLine("    Simple pass (by value). After:           {0}", testBool);
            Console.WriteLine();

            testBool = false;
            Console.WriteLine("    Simple pass (by reference). Before:      {0}", testBool);
            PassBoolWithRef(ref testBool);
            Console.WriteLine("    Simple pass (by reference). After:       {0}", testBool);
            Console.WriteLine();

            bool testBoolA = false;
            bool testBoolB = true;
            Console.WriteLine("    Swap (by value). Before:                 {0}, {1}", testBoolA, testBoolB);
            SwapBoolNoRef(testBoolA, testBoolB);
            Console.WriteLine("    Swap (by value). After:                  {0}, {1}", testBoolA, testBoolB);
            Console.WriteLine();

            Console.WriteLine("    Swap (by reference). Before:             {0}, {1}", testBoolA, testBoolB);
            SwapBoolWithRef(ref testBoolA, ref testBoolB);
            Console.WriteLine("    Swap (by reference). After:              {0}, {1}", testBoolA, testBoolB);
            Console.WriteLine("  #");
            Console.WriteLine("#\n");
        }

        private static void ReferenceTypes()
        {
            Console.WriteLine("### Reference-type variables do not contain its data directly (but a reference to it) ###");
            Console.WriteLine("  (passing by value means the method receives a copy of the reference)");
            Console.WriteLine("  (changes to the value of the reference itself cannot be changed)");
            Console.WriteLine("  (to change the value of the reference, you must pass by reference)");
            Console.WriteLine();

            Console.WriteLine("  # String #");
            string testString = "donkey";
            Console.WriteLine("    Simple pass (by value). Before:          {0}", testString);
            PassStringNoRef(testString);
            Console.WriteLine("    Simple pass (by value). After:           {0}", testString);
            Console.WriteLine();

            testString = "donkey";
            Console.WriteLine("    Simple pass (by reference). Before:      {0}", testString);
            PassStringWithRef(ref testString);
            Console.WriteLine("    Simple pass (by reference). After:       {0}", testString);
            Console.WriteLine();

            string testStringA = "AAA";
            string testStringB = "zzz";
            Console.WriteLine("    Swap (no reference). Before:             {0}, {1}", testStringA, testStringB);
            SwapStringNoRef(testStringA, testStringB);
            Console.WriteLine("    Swap (no reference). After:              {0}, {1}", testStringA, testStringB);
            Console.WriteLine();

            testStringA = "AAA";
            testStringB = "zzz";
            Console.WriteLine("    Swap (with reference). Before:           {0}, {1}", testStringA, testStringB);
            SwapStringWithRef(ref testStringA, ref testStringB);
            Console.WriteLine("    Swap (with reference). After:            {0}, {1}", testStringA, testStringB);
            Console.WriteLine("  #");

            Console.WriteLine("#\n");
        }

        private static void StructClass()
        {
            Console.WriteLine("### Structs and classes ###");
            Console.WriteLine("  (struct is a value-type; passing by value and the method receives a copy of the struct)");
            Console.WriteLine("  (class instances to the value of the reference itself cannot be changed)");
            Console.WriteLine("  (to change the value of the reference, you must pass by reference)");
            Console.WriteLine();

            Console.WriteLine("  # String #");
            string testString = "donkey";
            Console.WriteLine("    No reference. Before:                    {0}", testString);
            PassStringNoRef(testString);
            Console.WriteLine("    No reference. After:                     {0}", testString);
            Console.WriteLine();

            testString = "donkey";
            Console.WriteLine("    With reference. Before:                  {0}", testString);
            PassStringWithRef(ref testString);
            Console.WriteLine("    With reference. After:                   {0}", testString);
            Console.WriteLine();

            string testStringA = "AAA";
            string testStringB = "zzz";
            Console.WriteLine("    Swap (no reference). Before:             {0}, {1}", testStringA, testStringB);
            SwapStringNoRef(testStringA, testStringB);
            Console.WriteLine("    Swap (no reference). After:              {0}, {1}", testStringA, testStringB);
            Console.WriteLine();

            testStringA = "AAA";
            testStringB = "zzz";
            Console.WriteLine("    Swap (with reference). Before:           {0}, {1}", testStringA, testStringB);
            SwapStringWithRef(ref testStringA, ref testStringB);
            Console.WriteLine("    Swap (with reference). After:            {0}, {1}", testStringA, testStringB);
            Console.WriteLine("  #");
            Console.WriteLine("#\n");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("### Exploring passing parameters in C# ###");
            Console.WriteLine("  (passing by value allows the method to change the data of the referenced object)");
            Console.WriteLine("  (passing by value allows the method to change the data of the referenced object)");
            Console.WriteLine("  (passing by value allows the method to change the data of the referenced object)");
            Console.WriteLine("  (passing by value allows the method to change the data of the referenced object)");
            Console.WriteLine("  (passing by value allows the method to change the data of the referenced object)");
            Console.WriteLine();

            ValueTypes();
            ReferenceTypes();
            StructClass();

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }
    }
}
