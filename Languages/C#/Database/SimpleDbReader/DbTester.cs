using System;
using System.Data.Common;
using System.Data.OleDb;
using System.Data.Odbc;

using systemHelperLibrary;

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
        #region Member variables
        // Member variables
        private bool m_b64bit = false;
        private string m_strDevDataPath = string.Empty;
        private DatabaseTechnology m_tech = DatabaseTechnology.eDB_Unknown;

        // Example of using 
        private string[] sportTypes = { "Baseball", "Basketball", "Football",
                              "Hockey", "Soccer", "Tennis",
                              "Volleyball" };
        #endregion  // Member variables

        // Constructor
        public DbTester()
        {
            // The application assumes sample databases are located in %DevDataDirectory% folder
            // (eg. C:\Apps\Data). If not defined, add this User environment variable to Windows
            // and restart Visual Studio (or alternatively hard-code the path).
            DevDataPath = Environment.GetEnvironmentVariable("DevDataDirectory");
        }
        public DbTester(string strDevDataPath) { DevDataPath = strDevDataPath; }

        // Properties
        private string DevDataPath
        {
            get { return m_strDevDataPath; }
            set { m_strDevDataPath = value; }

            // Since C# 7.0 (VS 2017, .NET 4.7) you can write single expression property get/set
            // accessors like below. In my opinion this is ugly and more difficult to read!
            /*get => m_strDevDataPath;
            set => m_strDevDataPath = value;*/
        }

        public string this[int sport]
        {
            // Example usage of indexed "single expression" property accessors
            get => sportTypes[sport];
            set => sportTypes[sport] = value;
        }

        // Start: Methods (public)
        public override string ToString()
        {
            // Method that overrides the base class (System.Object) implementation
            return "DbTester Sample Application";
        }

        public void Initialise()
        {
            // Check whether this is a 32 or 64-bit application
            m_b64bit = SystemLibrary.Is64Bit();
        }

        public void TestDbTechnology(DatabaseTechnology eTechnology)
        {
            m_tech = eTechnology;
            switch (eTechnology)
            {
                case DatabaseTechnology.eDB_OleDB:
                    TestDB_OleDB();
                    break;

                case DatabaseTechnology.eDB_ODBC:
                    TestDB_ODBC();
                    break;

                default:
                    Console.WriteLine("Unknown DB technology: {0} ({1})\n", eTechnology, (int)eTechnology);
                    break;
            }
        }
        // End: Methods (public)

        // Start: Methods (private)
        private void TestDB_OleDB()
        {
            //  System.Data.OleDb.OleDbCommand
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbcommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand ###");

            // The connection string assumes that the Access Northwind database (.mdb) is located in
            // %DevDataDirectory% folder (eg. C:\Apps\Data). If not defined, add this User environment
            // variable and restart Visual Code (or just modify the code).
            //SetAccessVariables(AccessDbType.eAccess2000, ref strDataProvider, ref strSource);
            string strConnection = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_OleDB_SetConnectionString(type, ref strConnection))
                    TestDB_OleDB_Connect(strConnection);
            }

            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand ###\n");
        }

        private bool TestDB_OleDB_SetConnectionString(AccessDbType type, ref string strConnection)
        {
            // Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            string strDataDriver = "Provider=";
            string strDataSource = ("Data Source=" + DevDataPath);
            switch (type)
            {
                case AccessDbType.eAccess97:
                    // 32-bit only
                    if (!m_b64bit)
                    {
                        strDataDriver += "Microsoft.Jet.OLEDB.4.0;";
                        strDataSource += "\\Northwind 97.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("  ({0} does not support 64-bit)", HelperGetAccessName(type, false));
                        // Error is "The 'Microsoft.Jet.OLEDB.4.0' provider is not registered on the local machine."
                    }
                    break;

                case AccessDbType.eAccess2000:
                    // 32-bit only
                    if (!m_b64bit)
                    {
                        strDataDriver += "Microsoft.Jet.OLEDB.4.0;";
                        strDataSource += "\\Northwind 2000.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("  ({0} does not support 64-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    // 64-bit only
                    if (!m_b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("  ({0} does not support 32-bit)", HelperGetAccessName(type, false));
                        // Error is "The 'Microsoft.ACE.OLEDB.16.0' provider is not registered on the local machine."
                    }
                    else
                    {
                        strDataDriver += "Microsoft.ACE.OLEDB.16.0;";
                        strDataSource += "\\Northwind 2007-2016.accdb;";
                    }

                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            if (bHaveConnectionString)
                strConnection = (strDataDriver + strDataSource + "User Id=admin;Password=;");

            // Example, Access 97:
            //      Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\Northwind 97.mdb;;User Id=admin;Password=;

            return bHaveConnectionString;
        }

        private void TestDB_OleDB_Connect(string strConnection)
        {
            // Provide the query string with a parameter placeholder
            string queryString = HelperGetQueryString();

            // Specify the parameter value
            int paramValue = 5;

            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.

            // Note:
            // * "DbConnection" is the base class for OleDbConnection, OdbcConnection
            // * "DbCommand" is the base class for OleDbCommand, OdbcCommand
            // * "DbParameterCollection" is the base class for OleDbParameterCollection, OdbcParameterCollection
            // * "DbDataReader" is the base class for OleDbDataReader, OdbcDataReader
            // We could write a generic function to access the database with OleDB or ODBC, but it
            // is simpler to write an unique method for each technology.

            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OleDbCommand command = new OleDbCommand(queryString, connection);
                command.Parameters.AddWithValue("@pricePoint", paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader, writing the result to the console window
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

            Console.WriteLine("");
        }

        private void TestDB_ODBC()
        {
            // System.Data.Odbc.OdbcConnection
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.odbc.odbccommand
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection ###");

            // See "TestDB_OleDbConnection" for details on databases
            string strConnection = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_ODBC_SetConnectionString(type, ref strConnection))
                    TestDB_ODBC_Connect(strConnection, DatabaseTechnology.eDB_ODBC);
            }

            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection ###");
        }

        private bool TestDB_ODBC_SetConnectionString(AccessDbType type, ref string strConnection)
        {
            // Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            string strDataDriver = "Driver=";
            string strDataSource = ("Dbq=" + DevDataPath);
            switch (type)
            {
                case AccessDbType.eAccess97:
                    // 32-bit only
                    if (!m_b64bit)
                    {
                        strDataDriver += "{Microsoft Access Driver (*.mdb)};";
                        strDataSource += "\\Northwind 97.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("  ({0} does not supported 64-bit)", HelperGetAccessName(type, false));
                        // Error is "ERROR [IM002] [Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified"
                    }
                    break;

                case AccessDbType.eAccess2000:
                    // 32-bit only
                    if (!m_b64bit)
                    {
                        strDataDriver += "{Microsoft Access Driver (*.mdb)};";
                        strDataSource += "\\Northwind 2000.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("  ({0} does not supported 64-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    // 64-bit only
                    if (!m_b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("  ({0} does not supported 32-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    else
                    {
                        strDataDriver += "{Microsoft Access Driver (*.mdb, *.accdb)};";
                        strDataSource += "\\Northwind 2007-2016.accdb;";
                    }

                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            if (bHaveConnectionString)
                strConnection = (strDataDriver + strDataSource + "Uid=Admin;Pwd=;");

            // Example, Access 97:
            //      Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\Apps\Data\Northwind 97.mdb;Uid=Admin;Pwd=;

            return bHaveConnectionString;
        }

        private void TestDB_ODBC_Connect(string strConnection, DatabaseTechnology eDatabaseType)
        {
            // Provide the query string with a parameter placeholder
            string queryString = HelperGetQueryString();

            // Specify the parameter value
            int paramValue = 5;

            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            using (OdbcConnection  connection = new OdbcConnection (strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand  command = new OdbcCommand(queryString, connection);
                command.Parameters.AddWithValue("@pricePoint", paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader, writing the result to the console window
                    connection.Open();
                    OdbcDataReader  reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        Console.WriteLine("\t{0}\t{1:0.0}\t{2}",
                            reader[0], reader[1], reader[2]);
                    }
                    reader.Close();
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine("");
        }

        // Helper methods
        private string HelperGetAccessName(AccessDbType type, bool bFullDescription)
        {
            // Provide a human-readable name for the access database
            string strName = string.Empty;
            switch (type)
            {
                case AccessDbType.eAccess97:
                    strName = "Access 97";
                    if (bFullDescription)
                    {
                        if (m_tech == DatabaseTechnology.eDB_OleDB)
                            strName += " (32-bit using Microsoft.Jet.OLEDB.4.0)";
                        else if (m_tech == DatabaseTechnology.eDB_ODBC)
                            strName += " (32-bit using Microsoft Access Driver)";
                    }
                    break;

                case AccessDbType.eAccess2000:
                    strName = "Access 2000";
                    if (bFullDescription)
                    {
                        if (m_tech == DatabaseTechnology.eDB_OleDB)
                            strName += " (32-bit using Microsoft.Jet.OLEDB.4.0)";
                        else if (m_tech == DatabaseTechnology.eDB_ODBC)
                            strName += " (32-bit using Microsoft Access Driver)";
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    strName = "Access 2007-2016";
                    if (bFullDescription)
                    {
                        if (m_tech == DatabaseTechnology.eDB_OleDB)
                            strName += " (64-bit using Microsoft.ACE.OLEDB.16.0)";
                        else if (m_tech == DatabaseTechnology.eDB_ODBC)
                            strName += " (64-bit using Microsoft Access Driver)";
                    }
                    break;

                default:
                    strName = "Unknown Access database";
                    break;
            }

            return strName;
        }

        private string HelperGetQueryString()
        {
            // Use a standard query string while accessing the demo Northwind databases
            return (
                "SELECT ProductID, UnitPrice, ProductName from products " +
                "WHERE UnitPrice > ? " +
                "ORDER BY UnitPrice DESC;");
        }
        // End: Methods (private)
    }
}
