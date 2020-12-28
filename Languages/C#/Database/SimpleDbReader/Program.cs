using System;
using System.Data;
using systemHelperLibrary;

namespace SimpleDbReader
{
    class Program
    {
        // Member variables
        private static DbTester m_db = null;

        // Tests to be performed
        private static UInt32 m_tests = 0x00000000;
        private static readonly UInt32 cDummyOpenClose          = 0x00000001;
        private static readonly UInt32 cBasicRead               = 0x00000002;
        private static readonly UInt32 cBasicWrite               = 0x00000004;
        private static readonly UInt32 cPerformanceTests        = 0x00000100;
        private static readonly UInt32 cDifferentQueryStrings   = 0x00001000;

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

            // Define the tests to be performed
            m_tests = (
                cDummyOpenClose +
                cBasicRead +
                cPerformanceTests +
                cDifferentQueryStrings
                );

            // Perform a dummy open and close of each database in DAO. For an unknown reason (to be
            // investigated) this makes subsequent access using ODBC and OleDB faster. This might be
            // related to Windows caching the database file in memory or DAO performing some obscure
            // database caching operation.
            if ((m_tests & cDummyOpenClose) != 0)
            {
                Console.WriteLine("Open and close each database once with DAO...");
                m_db.OpenCloseDatabaseWithDAO();
                Console.WriteLine();
            }

            // Use some database technologies supported in VS 2019 and C# to read some records
            if ((m_tests & cBasicRead) != 0)
            {
                m_db.TestDbRead(DatabaseTechnology.eDB_DAO);
                m_db.TestDbRead(DatabaseTechnology.eDB_ODBC);
                m_db.TestDbRead(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Use some database technologies to write some records
            if ((m_tests & cBasicWrite) != 0)
            {
                m_db.TestDbWrite(DatabaseTechnology.eDB_DAO);
                m_db.TestDbWrite(DatabaseTechnology.eDB_ODBC);
                m_db.TestDbWrite(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Run some performance tests
            if ((m_tests & cPerformanceTests) != 0)
            {
                m_db.TestDbPerformance(DatabaseTechnology.eDB_DAO);
                m_db.TestDbPerformance(DatabaseTechnology.eDB_ODBC);
                m_db.TestDbPerformance(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Demonstrate different query strings
            if ((m_tests & cDifferentQueryStrings) != 0)
            {
                Console.WriteLine("Query using the WHERE clause 'LIKE' operator:");
                m_db.UpdateQuery(QueryType.eQueryLike);
                m_db.TestDbRead(DatabaseTechnology.eDB_OleDB);
            }

            // Complete!
            Console.Write("Press any key to exit...");
            Console.ReadLine();
        }
    }
}
