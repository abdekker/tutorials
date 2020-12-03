using System;
using System.Data.OleDb;

namespace SimpleDbReader
{
    // Enumerations
    enum AccessDbType
    {
        // Access database formats
        eAccessUnknown,
        eAccess97,
        eAccess2000,
        eAccess2007_2016
    }

    class DbTester
    {
        // Member variables
        private string m_strDevDataPath = string.Empty;

        // Constructor
        public DbTester()
        {
            // The application assumes sample databases are located in %DevDataDirectory% folder
            // (eg. C:\Apps\Data). If not defined, add this User environment variable to Windows
            // and restart Visual Studio (or alternatively hard-code the path).
            DevDataPath = Environment.GetEnvironmentVariable("DevDataDirectory");
            //Console.WriteLine(DevDataPath);
        }
        public DbTester(string strDevDataPath) { DevDataPath = strDevDataPath; }

        // Properties
        private string DevDataPath
        {
            get { return m_strDevDataPath; }
            set { m_strDevDataPath = value; }

            // Since C# 7.0 (VS 2017, .NET 4.7) you can write single expression property get/set
            // accessors like below. In my opinion this is ugly!
            /*get => m_strDevDataPath;
            set => m_strDevDataPath = value;*/
    }

        // Start: Methods (public)
        public override string ToString()
        {
            // Method that overrides the base class (System.Object) implementation
            return "DbTester Sample Application";
        }

        public void TestDbTechnology(DatabaseTechnology eTechnology)
        {
            switch (eTechnology)
            {
                case DatabaseTechnology.eDB_OleDbConnection:
                    TestDB_OleDbConnection();
                    break;

                default:
                    Console.WriteLine("Unknown DB technology: {0} ({1})\n", eTechnology, (int)eTechnology);
                    break;
            }
        }
        // End: Methods (public)

        // Start: Methods (private)
        private void TestDB_OleDbConnection()
        {
            //  System.Data.OleDb.OleDbCommand
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbcommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand ###");

            // The connection string assumes that the Access Northwind database (.mdb) is located in
            // %DevDataDirectory% folder (eg. C:\Apps\Data). If not defined, add this User environment
            // variable and restart Visual Code (or just modify the code).
            string strDataProvider = string.Empty;
            string strSource = string.Empty;
            SetAccessVariables(AccessDbType.eAccess2000, ref strDataProvider, ref strSource);
            string connectionString = (strDataProvider + strSource + ";User Id=admin;Password=;");

            // Provide the query string with a parameter placeholder.
            string queryString =
                "SELECT ProductID, UnitPrice, ProductName from products "
                    + "WHERE UnitPrice > ? "
                    + "ORDER BY UnitPrice DESC;";

            // Specify the parameter value.
            int paramValue = 5;

            // Create and open the connection in a using block. This
            // ensures that all resources will be closed and disposed
            // when the code exits.
            using (OleDbConnection connection =
                new OleDbConnection(connectionString))
            {
                // Create the Command and Parameter objects.
                OleDbCommand command = new OleDbCommand(queryString, connection);
                command.Parameters.AddWithValue("@pricePoint", paramValue);

                // Open the connection in a try/catch block.
                // Create and execute the DataReader, writing the result
                // set to the console window.
                try
                {
                    connection.Open();
                    OleDbDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        Console.WriteLine("\t{0}\t{1}\t{2}",
                            reader[0], reader[1], reader[2]);
                    }
                    reader.Close();
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand ###");
        }

        // Helper methods
        private void SetAccessVariables(AccessDbType type,
            ref string strDataProvider, ref string strDataSource)
        {
            // Set up the connection string based on the version of the Access database
            string strDevDataPath = Environment.GetEnvironmentVariable("DevDataDirectory");
            strDataProvider = "Provider=";
            strDataSource = ("Data Source=" + strDevDataPath);
            switch (type)
            {
                case AccessDbType.eAccess97:
                    strDataProvider += "Microsoft.Jet.OLEDB.4.0;";
                    strDataSource += "\\Northwind 97.mdb;";
                    break;

                case AccessDbType.eAccess2000:
                    strDataProvider += "Microsoft.Jet.OLEDB.4.0;";
                    strDataSource += "\\Northwind 2000.mdb;";
                    break;

                case AccessDbType.eAccess2007_2016:
                    strDataProvider += "Microsoft.ACE.OLEDB.12.0;";     // "16" is also "not registered"
                    strDataSource += "\\Northwind 2007-2016.accdb;";
                    break;

                default:
                    strDataProvider += "Microsoft.Jet.OLEDB.4.0;";
                    strDataSource += "C:\\Data\\Northwind.mdb;";
                    break;
            }
        }
        // End: Methods (private)
    }
}
