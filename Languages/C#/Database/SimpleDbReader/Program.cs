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

            // Perform a dummy open and close of each database in DAO. For an unknown reason (to be
            // investigated) this makes subsequent access using ODBC and OleDB faster. This might be
            // related to Windows caching the database file in memory or DAO performing some obscure
            // database caching operation.
            Console.WriteLine("Open and close each database once with DAO...");
            m_db.OpenCloseDatabaseWithDAO();
            Console.WriteLine();

            // Test some of the database technologies supported in VS 2019 and C#
            m_db.TestDbTechnology(DatabaseTechnology.eDB_DAO);
            m_db.TestDbTechnology(DatabaseTechnology.eDB_ODBC);
            m_db.TestDbTechnology(DatabaseTechnology.eDB_OleDB);
            Console.WriteLine();

            // Run some performance tests
            m_db.TestDbTechnologyPerformance(DatabaseTechnology.eDB_DAO);
            m_db.TestDbTechnologyPerformance(DatabaseTechnology.eDB_ODBC);
            m_db.TestDbTechnologyPerformance(DatabaseTechnology.eDB_OleDB);
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
