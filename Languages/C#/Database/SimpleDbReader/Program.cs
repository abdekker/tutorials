using System;

using systemHelperLibrary;

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
            Console.WriteLine($"  64-bit? {SystemLibrary.Is64Bit()}\n");

            // Not related to working with databases, just using an accessor list (which I'd not seen before)
            Console.WriteLine("Demonstrate using indexed property accessors");
            for (int sport = 0; sport <= 8; sport++)
                Console.WriteLine("  sport {0} is {1}", sport, m_db[sport]);

            Console.WriteLine();

            // Test one of the database technologies supported in VS 2019 and C#
            m_db.TestDbTechnology(DatabaseTechnology.eDB_OleDB);
            m_db.TestDbTechnology(DatabaseTechnology.eDB_ODBC);
            Console.WriteLine();

            // Run some performance tests
            m_db.TestDbTechnologyPerformance(DatabaseTechnology.eDB_OleDB);
            m_db.TestDbTechnologyPerformance(DatabaseTechnology.eDB_ODBC);
            Console.WriteLine();

            // Demonstrate different query strings
            Console.WriteLine("Query using the WHERE clause 'LIKE' operator:");
            m_db.UpdateQuery(QueryType.eQueryLike);
            m_db.TestDbTechnology(DatabaseTechnology.eDB_OleDB);

            // Complete!
            Console.Write("Press any key to exit...");
            Console.ReadLine();
        }
    }
}
