using System;

namespace SimpleDbReader
{
    class Program
    {
        // Member variables
        private static DbTester m_db = null;

        // Access method for this application
        static void Main()
        {
            // Create a DbTester class
            m_db = new DbTester();
            m_db.Initialise();
            Console.WriteLine("This is the {0}", m_db.ToString());
            Console.WriteLine($"  (application is 64-bit? {Environment.Is64BitProcess})\n");

            // Not related to working with databases, just using an accessor list (which I'd not seen before)
            for (int sport = 0; sport <= 5; sport++)
                Console.WriteLine("  sport {0} is {1}", sport, m_db[sport]);

            Console.WriteLine("");

            // Test one of the database technologies supported in VS 2019 and C#
            m_db.TestDbTechnology(DatabaseTechnology.eDB_OleDbConnection);

            // Complete!
            Console.Write("Press any key to exit...");
            Console.ReadLine();
        }
    }
}
