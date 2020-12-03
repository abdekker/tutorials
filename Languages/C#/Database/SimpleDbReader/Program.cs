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
            Console.WriteLine("This application is: {0}", m_db.ToString());

            // Test one of the database technologies supported in VS 2019 and C#
            m_db.TestDbTechnology(DatabaseTechnology.eDB_Unknown);
            m_db.TestDbTechnology(DatabaseTechnology.eDB_OleDbConnection);

            // Complete!
            Console.ReadLine();
        }
    }
}
