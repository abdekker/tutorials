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
        private static readonly UInt32 cSimpleStats                 = 0x00000002;
        private static readonly UInt32 cSimpleRead                  = 0x00000004;
        private static readonly UInt32 cSimpleWriteable             = 0x00000008;   // Open database in readonly and writeable modes
        private static readonly UInt32 cSimpleInsert                = 0x00000010;
        private static readonly UInt32 cSimpleUpdate                = 0x00000020;
        private static readonly UInt32 cSimpleDelete                = 0x00000040;
        // No performance method for the basic database...test performance using the Northwind database

        private static readonly UInt32 cNorthwindDummyOpenClose     = 0x00001000;
        private static readonly UInt32 cNorthwindStats              = 0x00002000;
        private static readonly UInt32 cNorthwindRead               = 0x00004000;
        private static readonly UInt32 cNorthwindWriteable          = 0x00008000;   // Open database in readonly and writeable modes (unused)
        private static readonly UInt32 cNorthwindInsert             = 0x00010000;
        private static readonly UInt32 cNorthwindUpdate             = 0x00020000;
        private static readonly UInt32 cNorthwindDelete             = 0x00040000;
        private static readonly UInt32 cNorthwindPerformance        = 0x00100000;
        private static readonly UInt32 cDifferentQueryStrings       = 0x00200000;

        private static readonly UInt32 cOtherTests                  = 0x80000000;

        static void Main()
        {
            // Database technologies supported in VS 2019 and C# to read some records

            // Create a DbTester class
            m_db = new DbTester();
            m_db.Initialise();
            Console.WriteLine("This is the {0}", m_db.ToString());
            Console.WriteLine($"  64-bit? {SystemLibrary.Is64Bit()}\n");

            // Define the tests to be performed
            m_tests = (
                //cSimpleCreateDB
                //cSimpleStats
                //cSimpleRead
                //cSimpleWriteable
                //cSimpleInsert
                //cSimpleUpdate
                //cSimpleDelete
                //cOtherTests
                //cNorthwindDummyOpenClose
                //cNorthwindStats
                cNorthwindRead
                //cNorthwindWriteable
                //cNorthwindInsert
                //cNorthwindUpdate
                //cNorthwindDelete
                //cNorthwindPerformance
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

            #region SimpleTest.mdb database
            if ((m_tests & cSimpleCreateDB) != 0)
                m_db.SimpleCreateDB();

            if ((m_tests & cSimpleStats) != 0)
            {
                // Count tables, columns and other statistics
                m_db.SimpleStats(DatabaseTechnology.eDB_DAO);
                m_db.SimpleStats(DatabaseTechnology.eDB_ODBC);
                Console.WriteLine();
            }

            if ((m_tests & cSimpleRead) != 0)
            {
                m_db.SimpleRead(DatabaseTechnology.eDB_DAO);
                m_db.SimpleRead(DatabaseTechnology.eDB_ODBC);
            }

            if ((m_tests & cSimpleWriteable) != 0)
            {
                m_db.SimpleWriteable(DatabaseTechnology.eDB_DAO);
                m_db.SimpleWriteable(DatabaseTechnology.eDB_ODBC);
            }

            if ((m_tests & cSimpleInsert) != 0)
            {
                m_db.SimpleWriteable(DatabaseTechnology.eDB_DAO);
                m_db.SimpleWriteable(DatabaseTechnology.eDB_ODBC);
            }

            if ((m_tests & cSimpleUpdate) != 0)
            {
                m_db.SimpleWriteable(DatabaseTechnology.eDB_DAO);
                m_db.SimpleWriteable(DatabaseTechnology.eDB_ODBC);
            }

            if ((m_tests & cSimpleDelete) != 0)
            {
                m_db.SimpleWriteable(DatabaseTechnology.eDB_DAO);
                m_db.SimpleWriteable(DatabaseTechnology.eDB_ODBC);
            }
            #endregion // SimpleTest.mdb database

            #region Northwind sample database
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

            if ((m_tests & cNorthwindStats) != 0)
            {
                // Count tables, columns and other statistics
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindStats(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindStats(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindStats(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            if ((m_tests & cNorthwindRead) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindRead(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindRead(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindRead(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            if ((m_tests & cNorthwindWriteable) != 0)
            {
                // Not implemented...
                m_db.UpdateQuery(QueryType.eQueryStd1);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_DAO);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_ODBC);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            if ((m_tests & cNorthwindInsert) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindInsert(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindInsert(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindInsert(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            if ((m_tests & cNorthwindUpdate) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_DAO);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_ODBC);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            if ((m_tests & cNorthwindDelete) != 0)
            {
                m_db.UpdateQuery(QueryType.eQueryStd1);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_DAO);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_ODBC);
                //m_db.NorthwindWriteable(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            if ((m_tests & cNorthwindPerformance) != 0)
            {
                // Performance comparisons
                m_db.UpdateQuery(QueryType.eQueryStd1);
                m_db.NorthwindPerformance(DatabaseTechnology.eDB_DAO);
                m_db.NorthwindPerformance(DatabaseTechnology.eDB_ODBC);
                m_db.NorthwindPerformance(DatabaseTechnology.eDB_OleDb);
                Console.WriteLine();
            }

            // Demonstrate different query strings
            if ((m_tests & cDifferentQueryStrings) != 0)
            {
                Console.WriteLine("Query using the WHERE clause 'LIKE' operator:");
                m_db.UpdateQuery(QueryType.eQueryLike);
                m_db.NorthwindRead(DatabaseTechnology.eDB_DAO);
            }
            #endregion // Northwind sample database

            // Complete!
            Console.Write("Press any key to exit...");
            Console.ReadLine();
        }
    }
}
