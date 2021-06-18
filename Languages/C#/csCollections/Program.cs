using System;
using System.Collections.Generic;
using System.Linq;

namespace csCollections
{
    class Program
    {
        #region Helper methods
        private static void PrintCollection<T>(ICollection<T> myCollection, string prefix = "    ", string separator = ",")
        {
            // Print out a generic collection object
            if (prefix.Length > 0)
                Console.Write(prefix);

            for (int index = 0; index < myCollection.Count; index++)
            {
                if (index != (myCollection.Count - 1))
                    Console.Write(myCollection.ElementAt(index).ToString() + separator);
                else
                    Console.Write(myCollection.ElementAt(index).ToString());
            }
            Console.WriteLine();
        }
        #endregion // Helper methods

        #region Main test methods
        private static void Info_List()
        {
            // List<T>
            Console.WriteLine("### List ###");
            Console.WriteLine("(strongly-typed list of objects accessible by index)");
            Console.WriteLine();

            Console.WriteLine("  (items added one by one)");
            List<string> salmons = new List<string>();
            salmons.Add("Chinook");
            salmons.Add("Coho");
            salmons.Add("Pink");
            salmons.Add("Sockeye");
            PrintCollection(salmons);
            Console.WriteLine();

            Console.WriteLine("  (items added using a collection initializer)");
            List<string> dogs = new List<string> { "Chihuahua", "Dalmatian", "Great Dane", "Jack Russell" };
            PrintCollection(dogs);
            Console.WriteLine();

            Console.WriteLine("  (odd numbers removed from list of numbers 0-9)");
            List<int> numbers = new List<int> { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
            for (int index = (numbers.Count - 1); index >= 0; index--)
            {
                if (numbers[index] % 2 == 1)
                    numbers.RemoveAt(index);
            }
            PrintCollection(numbers);
            Console.WriteLine();

            Console.WriteLine("  (same list printed using a lambda expression)");
            Console.Write("    ");
            numbers.ForEach(number => Console.Write(number + ","));
            Console.WriteLine();
            Console.WriteLine("#\n");
        }

        private static void Info_Dictionary()
        {
            // Dictionary<TKey,TValue>
            Console.WriteLine("### Dictionary ###");
            Console.WriteLine("(collection of keys and values)");
            Console.WriteLine();

            Console.WriteLine("  (set up a simple <string,string> dictionary)");
            Dictionary<string, string> openWith = new Dictionary<string, string>();

            // Add elements - there are no duplicate keys, but some of the values are duplicates
            openWith.Add("txt", "notepad.exe");
            openWith.Add("bmp", "paint.exe");
            openWith.Add("dib", "paint.exe");
            openWith.Add("rtf", "wordpad.exe");
            PrintCollection(openWith);
            Console.WriteLine();

            Console.WriteLine("  (elements accessed with the Item property (or \"indexer\") use the [] operator)");
            Console.WriteLine("    For key = \"rtf\", value = {0}", openWith["rtf"]);
            Console.WriteLine();

            Console.WriteLine("  (the indexer can be used to change the value associated with a key)");
            openWith["rtf"] = "winword.exe";
            Console.WriteLine("    For key = \"rtf\", value = {0}", openWith["rtf"]);
            Console.WriteLine();

            Console.WriteLine("  (if key does not exist, setting the indexer for that key adds a new key/value pair)");
            openWith["doc"] = "winword.exe";
            PrintCollection(openWith);
            Console.WriteLine();

            Console.WriteLine("  (adding a duplicate key throws an exception)");
            try { openWith.Add("txt", "winword.exe"); }
            catch (ArgumentException) { Console.WriteLine("    Exception: An element with Key = \"txt\" already exists"); }
            Console.WriteLine();

            Console.WriteLine("  (the indexer throws an exception if the requested key does not exist)");
            try { Console.WriteLine("    For key = \"tif\", value = {0}", openWith["tif"]); }
            catch (KeyNotFoundException) { Console.WriteLine("    Exception: Key = \"tif\" does not exist"); }
            Console.WriteLine("  (use \"TryGetValue\" or \"ContainsKey\" to test for keys)");
            Console.WriteLine();

            Console.WriteLine("  (when using \"foreach\", elements are retrieved as KeyValuePair objects)");
            foreach (KeyValuePair<string, string> kvp in openWith)
            {
                Console.WriteLine("    Key = {0}, Value = {1}", kvp.Key, kvp.Value);
            }
            Console.WriteLine();

            Console.WriteLine("  (keys and values can be extracted using \"ValueCollection\" and \"KeyCollection\")");
            Dictionary<string, string>.KeyCollection keys = openWith.Keys;
            Dictionary<string, string>.ValueCollection values = openWith.Values;
            PrintCollection(keys, "    Keys: ");
            PrintCollection(values, "    Values: ");
            Console.WriteLine("  (these can be converted to \"List<T>\" using \"ToList\")");
            Console.WriteLine();

            Console.WriteLine("  (removing key/value pair \"doc\")");
            openWith.Remove("doc");
            PrintCollection(openWith);
            Console.WriteLine("#\n");
        }
        #endregion // Main test methods

        static void Main(string[] args)
        {
            // Demonstrating some some .NET collection classes
            Console.WriteLine("=== .NET collection classes in C# ===");
            Console.WriteLine();

            // Which sections are we going to display?
            const uint TEST_ALL_SECTIONS            = 0xFFFFFFFF;   // Generally use this one
            const uint TEST_LIST                    = 0x00000001;
            const uint TEST_DICTIONARY              = 0x00000002;
            uint test = TEST_ALL_SECTIONS;

            if ((test & TEST_LIST) != 0)
                Info_List();

            if ((test & TEST_DICTIONARY) != 0)
                Info_Dictionary();

            Console.WriteLine("All done...press any key to continue");
            Console.ReadKey(false);
        }
    }
}
