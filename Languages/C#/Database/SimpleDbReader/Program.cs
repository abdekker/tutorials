using System;
using System.Data;
using systemHelperLibrary;

namespace SimpleDbReader
{
    // Tutorial based on:
    // 1) https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/ado-net-code-examples
    // 2) https://www.w3schools.com/sql/default.asp

    class Program
    {
        // Member variables
        private static DbTester m_db = null;

        // Tests to be performed
        private static UInt32 m_tests = 0x00000000;
        private static readonly UInt32 cSimpleCreateDB              = 0x00000001;
        private static readonly UInt32 cSimpleRead                  = 0x00000002;
        private static readonly UInt32 cSimpleModify                = 0x00000004;
        private static readonly UInt32 cNorthwindDummyOpenClose     = 0x00000010;
        private static readonly UInt32 cNorthwindRead               = 0x00000100;
        private static readonly UInt32 cNorthwindWrite              = 0x00000200;
        private static readonly UInt32 cNorthwindStats              = 0x00000400;
        private static readonly UInt32 cNorthwindPerformance        = 0x00000800;
        private static readonly UInt32 cDifferentQueryStrings       = 0x00100000;
        private static readonly UInt32 cOtherTests                  = 0x80000000;

        // Access method for this application
        static void Main()
        {
            // Create a DbTester class
            m_db = new DbTester();
            m_db.Initialise();
            Console.WriteLine("This is the {0}", m_db.ToString());
            Console.WriteLine($"  64-bit? {SystemLibrary.Is64Bit()}\n");

            // Define the tests to be performed
            m_tests = (
                //cSimpleCreateDB +
                //cSimpleRead +
                //cSimpleModify +
                //cOtherTests +
                //cNorthwindDummyOpenClose +
                //cNorthwindStats
                cNorthwindRead
                //cNorthwindWrite
                //cNorthwindPerformance +
                //cDifferentQueryStrings
                );

            // Other tests (unrelated to accessing databases
            if ((m_tests & cOtherTests) != 0)
            {
                // Using an accessor list (something which I'd not seen before)
                Console.WriteLine("Demonstrate using indexed property accessors");
                for (int sport = 0; sport <= 8; sport++)
                    Console.WriteLine("  sport {0} is {1}", sport, m_db[sport]);

                Console.WriteLine();
            }

            // Use database technologies supported in VS 2019 and C#

            // Using the SimpleTest.mdb database
            if ((m_tests & cSimpleCreateDB) != 0)
                m_db.SimpleCreateDB();

            if ((m_tests & cSimpleRead) != 0)
            {
                m_db.SimpleRead(DatabaseTechnology.eDB_DAO);
                m_db.SimpleRead(DatabaseTechnology.eDB_ODBC);
            }

            if ((m_tests & cSimpleModify) != 0)
            {
                m_db.SimpleWrite(DatabaseTechnology.eDB_DAO);
                m_db.SimpleWrite(DatabaseTechnology.eDB_ODBC);
            }

            // Using the Northwind sample databases

            // Perform a dummy open and close of each database in DAO. For an unknown reason (to be
            // investigated) this makes subsequent access using ODBC and OleDB faster. This might be
            // related to Windows caching the database file in memory or DAO performing some obscure
            // database caching operation.
            if ((m_tests & cNorthwindDummyOpenClose) != 0)
            {
                Console.WriteLine("Open and close each database once with DAO...");
                m_db.NorthwindOpenCloseWithDAO();
                Console.WriteLine();
            }

            // Count columns and other statistics
            if ((m_tests & cNorthwindStats) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindStats(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindStats(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindStats(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Use some database technologies supported in VS 2019 and C# to read some records
            if ((m_tests & cNorthwindRead) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindRead(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindRead(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindRead(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Northwind databases
            if ((m_tests & cNorthwindWrite) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindWrite(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindWrite(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindWrite(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Run some performance tests
            if ((m_tests & cNorthwindPerformance) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindPerformance(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindPerformance(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindPerformance(DatabaseTechnology.eDB_OleDB);
                Console.WriteLine();
            }

            // Demonstrate different query strings
            if ((m_tests & cDifferentQueryStrings) != 0)
            {
                Console.WriteLine("Query using the WHERE clause 'LIKE' operator:");
                m_db.UpdateQuery(QueryType.eQueryLike);
                m_db.NorthwindRead(DatabaseTechnology.eDB_DAO);
            }

            // Complete!
            Console.Write("Press any key to exit...");
            Console.ReadLine();
        }
    }
}
