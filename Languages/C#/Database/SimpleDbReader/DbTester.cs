using System;
using System.Data;
using System.Data.OleDb;
using System.Data.Odbc;

using systemHelperLibrary;
using System.Linq.Expressions;

namespace SimpleDbReader
{
    #region Enumerations
    enum AccessDbType
    {
        // Access database formats
        eAccessUnknown,
        eAccess97,
        eAccess2000,
        eAccess2007_2016
    }
    #endregion  // Enumerations 

    class DbTester
    {
        #region Constants
        const int cPerformanceLoops = 200;
        #endregion  // Constants

        #region Member variables
        // Member variables
        private bool m_b64bit = false;
        private string m_strDevDataPath = string.Empty;
        private DatabaseTechnology m_tech = DatabaseTechnology.eDB_Unknown;
        private string m_strQuery;

        // For the std query on the Northwind DB, records returned based on the parameter are:
        //  Param    Records returned
        //    5         75
        //    25        28
        //    40        12
        private int m_paramValue = 5;

        // Perfomance tests
        private int m_nLoops = cPerformanceLoops;

        // Example ofr using indexed property accesors
        private string[] sportTypes = {
            "Baseball", "Basketball", "Chess", "Football", "Hockey",
            "Rugby", "Soccer", "Tennis", "Volleyball" };
        #endregion  // Member variables

        // Constructor
        public DbTester()
        {
            // Tutorial based on: https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/ado-net-code-examples

            // This application assumes sample databases are located in the %DevDataDirectory% folder
            // (eg. C:\Apps\Data). If not defined, add this User environment variable to Windows and
            // restart Visual Studio (or alternatively hard-code the path).
            DevDataPath = Environment.GetEnvironmentVariable("DevDataDirectory");
        }
        public DbTester(string strDevDataPath) { DevDataPath = strDevDataPath; }

        // Properties
        private string DevDataPath
        {
            get { return m_strDevDataPath; }
            set { m_strDevDataPath = value; }

            // Since C# 7.0 (VS 2017, .NET 4.7) you can write single expression property get/set
            // accessors like below. In my opinion this is more difficult to read!
            /*get => m_strDevDataPath;
            set => m_strDevDataPath = value;*/
        }

        public string this[int sport]
        {
            // Example usage of "single expression" indexed property accessors
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

            // Set a generic query string
            m_strQuery = HelperGetQueryString(QueryType.eQueryStd);
        }

        public void UpdateQuery(QueryType eQuery)
        {
            // Update the generic query string
            m_strQuery = HelperGetQueryString(eQuery);
        }

        public void TestDbTechnology(DatabaseTechnology eTechnology)
        {
            // Test one of the database technologies available in .NET
            m_tech = eTechnology;
            switch (eTechnology)
            {
                case DatabaseTechnology.eDB_DAO:
                    TestDB_DAO();
                    break;

                case DatabaseTechnology.eDB_ODBC:
                    TestDB_ODBC();
                    break;

                case DatabaseTechnology.eDB_OleDB:
                    TestDB_OleDB();
                    break;

                default:
                    Console.WriteLine("Unknown DB technology: {0} ({1})\n", eTechnology, (int)eTechnology);
                    break;
            }
        }

        public void TestDbTechnologyPerformance(DatabaseTechnology eTechnology, int nLoops = cPerformanceLoops)
        {
            // This version runs some performance tests
            m_tech = eTechnology;
            m_nLoops = nLoops;
            switch (eTechnology)
            {
                case DatabaseTechnology.eDB_OleDB:
                    TestDB_OleDB_Performance();
                    break;

                case DatabaseTechnology.eDB_ODBC:
                    TestDB_ODBC_Performance();
                    break;

                default:
                    Console.WriteLine("Unknown DB technology: {0} ({1})\n", eTechnology, (int)eTechnology);
                    break;
            }
        }
        // End: Methods (public)

        // Start: Methods (private)
        private void TestDB_DAO()
        {
            // DAO
            Console.WriteLine("### START: DAO ###");

            // See the class constructor for details on databases
            string strDatabase = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_DAO_SetDatabaseString(type, ref strDatabase))
                    TestDB_DAO_Connect(strDatabase);
            }

            Console.WriteLine("### END: DAO ###");
        }

        private bool TestDB_DAO_SetDatabaseString(AccessDbType type, ref string strDatabase)
        {
            // DAO: Set up the name of the Access database
            bool bHaveConnectionString = true;
            switch (type)
            {
                case AccessDbType.eAccess97:
                    // 32-bit only
                    if (!m_b64bit)
                        strDatabase = (DevDataPath + "\\Northwind 97.mdb");
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(type, false));
                    }
                    break;

                case AccessDbType.eAccess2000:
                    // 32-bit only
                    if (!m_b64bit)
                        strDatabase = (DevDataPath + "\\Northwind 2000.mdb");
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    // 64-bit only
                    if (!m_b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    else
                        strDatabase = (DevDataPath + "\\2007-2016");

                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            // Example, Access 97: C:\Apps\Data\Northwind 97.mdb
            return bHaveConnectionString;
        }

        private void TestDB_DAO_Connect(string strDatabase)
        {
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);

            DAO.Database db = dbEngine.OpenDatabase(strDatabase, false, false);
            DAO.Recordset rs = null;

            m_strQuery = "SELECT * FROM Products";
            rs = db.OpenRecordset(m_strQuery, DAO.RecordsetTypeEnum.dbOpenDynaset, DAO.RecordsetOptionEnum.dbReadOnly);
            //rs = db.OpenRecordset(m_strQuery);

            int Fred = 0;

            /*DAO.DBEngine =  .OpenDatabase(SystemCore.SysInfo.DatabaseFile, false, false);

            // Specify the parameter value
            int paramValue = 5;

            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand command = new OdbcCommand(m_strQuery, connection);
                command.Parameters.AddWithValue("@pricePoint", paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader, writing the result to the console window
                    Console.WriteLine("\t{0}\t{1}\t{2}",
                        "ProductID", "UnitPrice", "ProductName");
                    int recordsRead = 0;
                    connection.Open();
                    OdbcDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                        Console.WriteLine("\t{0}\t\t{1:0.0}\t\t{2}",
                            reader[0], reader[1], reader[2]);
                    }
                    reader.Close();
                    Console.WriteLine("    ({0} records)", recordsRead);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine();*/
        }

        private void TestDB_ODBC()
        {
            // System.Data.Odbc.OdbcConnection
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.odbc.odbccommand
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_ODBC_SetConnectionString(type, ref strConnection))
                    TestDB_ODBC_Connect(strConnection);
            }

            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection ###");
        }

        private void TestDB_ODBC_Performance()
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: ODBC - Performance tests ###");

            // See "TestDB_OleDbConnection" for details on databases
            string strConnection = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_ODBC_SetConnectionString(type, ref strConnection))
                   {
                    int totalRecordsRead = 0;
                    int startTicks = Environment.TickCount;
                    for (int loop = 0; loop < m_nLoops; loop++)
                        totalRecordsRead += TestDB_ODBC_Connect_Performance(strConnection);

                    int elapsedTicks = (Environment.TickCount - startTicks);
                    Console.WriteLine("    ({0} cycles: Took {1}ms at an avg of {2:0.00}ms to read an avg of {3:0.0} records)",
                        m_nLoops,
                        elapsedTicks,
                        (float)elapsedTicks/(float)m_nLoops,
                        (float)totalRecordsRead/(float)m_nLoops);
                }
            }

            Console.WriteLine("### END: ODBC - Performance tests ###");
        }

        private bool TestDB_ODBC_SetConnectionString(AccessDbType type, ref string strConnection)
        {
            // ODBC: Set up the connection string based on the version of the Access database
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
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(type, false));
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
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    // 64-bit only
                    if (!m_b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(type, false));
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

        private void TestDB_ODBC_Connect(string strConnection)
        {
            // Specify the parameter value
            int paramValue = 5;

            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand command = new OdbcCommand(m_strQuery, connection);
                command.Parameters.AddWithValue("@pricePoint", paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader, writing the result to the console window
                    Console.WriteLine("\t{0}\t{1}\t{2}",
                        "ProductID", "UnitPrice", "ProductName");
                    int recordsRead = 0;
                    connection.Open();
                    OdbcDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                        Console.WriteLine("\t{0}\t\t{1:0.0}\t\t{2}",
                            reader[0], reader[1], reader[2]);
                    }
                    reader.Close();
                    Console.WriteLine("    ({0} records)", recordsRead);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine();
        }

        private int TestDB_ODBC_Connect_Performance(string strConnection)
        {
            // Version for performance testing
            int recordsRead = 0;
            int paramValue = 5;

            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand command = new OdbcCommand(m_strQuery, connection);
                command.Parameters.AddWithValue("@pricePoint", paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader; for this performance version just count
                    // the number of records read
                    connection.Open();
                    OdbcDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                    }
                    reader.Close();
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            return recordsRead;
        }

        private void TestDB_OleDB()
        {
            //  System.Data.OleDb.OleDbCommand
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbcommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_OleDB_SetConnectionString(type, ref strConnection))
                {
                    // Choose how to read the data using OleDB
                    TestDB_OleDB_Connect_DataReader(strConnection);
                    //TestDB_OleDB_Connect_DataSet(strConnection);
                }
            }

            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand ###\n");
        }

        private void TestDB_OleDB_Performance()
        {
            //  System.Data.OleDb.OleDbCommand
            Console.WriteLine("### START: OleDb - Performance tests ###");
            string strConnection = string.Empty;
            foreach (AccessDbType type in Enum.GetValues(typeof(AccessDbType)))
            {
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(type, true));
                if (TestDB_OleDB_SetConnectionString(type, ref strConnection))
                {
                    int totalRecordsRead = 0;
                    int startTicks = Environment.TickCount;
                    for (int loop = 0; loop < m_nLoops; loop++)
                        totalRecordsRead += TestDB_OleDB_Connect_Performance(strConnection);

                    int elapsedTicks = (Environment.TickCount - startTicks);
                    Console.WriteLine("    ({0} cycles: Took {1}ms at an avg of {2:0.00}ms to read an avg of {3:0.0} records)",
                        m_nLoops,
                        elapsedTicks,
                        (float)elapsedTicks/(float)m_nLoops,
                        (float)totalRecordsRead/(float)m_nLoops);
                }
            }

            Console.WriteLine("### END: OleDb - Performance tests ###\n");
        }

        private bool TestDB_OleDB_SetConnectionString(AccessDbType type, ref string strConnection)
        {
            // OleDB: Set up the connection string based on the version of the Access database
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
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(type, false));
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
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(type, false));
                        // Error same as for Access 97
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    // 64-bit only
                    if (!m_b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(type, false));
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

        private void TestDB_OleDB_Connect_DataReader(string strConnection)
        {
            // This version uses System.Data.OleDb.OleDbDataReader

            // Specify the parameter value. For the std query, records returned based on the parameter are:
            //  Param    Records returned
            //    5         75
            //    25        28
            //    40        12
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
                // Open the connection in a try/catch block
                try
                {
                    // Create the Command and Parameter objects
                    OleDbCommand command = new OleDbCommand(m_strQuery, connection);
                    command.Parameters.AddWithValue("@pricePoint", paramValue);
                    // This also works: command.Parameters.AddWithValue(string.Empty, paramValue);

                    // Create and execute the DataReader, writing the result to the console window
                    Console.WriteLine("\t{0}\t{1}\t{2}",
                        "ProductID", "UnitPrice", "ProductName");
                    int recordsRead = 0;
                    connection.Open();
                    OleDbDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                        Console.WriteLine("\t{0}\t\t{1}\t\t{2}",
                            reader[0], reader[1], reader[2]);
                    }
                    reader.Close();
                    Console.WriteLine("    ({0} records)", recordsRead);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine();
        }

        private void TestDB_OleDB_Connect_DataSet(string strConnection)
        {
            // This version uses:
            // * System.Data.DataSet
            // * System.Data.OleDb.OleDbDataAdapter
            // * System.Data.DataRow

            // Specify the parameter value
            int paramValue = 5;

            // Create and open the connection in a using bloc
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                try
                {
                    // Create and fill the DataSet, writing the result to the console window
                    Console.WriteLine("\t{0}\t{1}\t{2}",
                        "ProductID", "UnitPrice", "ProductName");
                    int recordsRead = 0;
                    connection.Open();
                    DataSet ds = new DataSet();
                    OleDbDataAdapter adapter = new OleDbDataAdapter(m_strQuery, connection);
                    adapter.SelectCommand.Parameters.Add("@pricePoint", OleDbType.Integer).Value = paramValue;
                    adapter.Fill(ds);
                    foreach (DataRow row in ds.Tables[0].Rows)
                    {
                        recordsRead++;
                        Console.WriteLine("\t{0}\t\t{1}\t\t{2}",
                            row["ProductID"],
                            row["UnitPrice"],
                            row["ProductName"]);
                    }
                    Console.WriteLine("    ({0} records)", recordsRead);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine();
        }

        private int TestDB_OleDB_Connect_Performance(string strConnection)
        {
            // Version for performance testing
            int recordsRead = 0;
            int paramValue = 5;
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                // Open the connection in a try/catch block
                try
                {
                    // Create the Command and Parameter objects
                    OleDbCommand command = new OleDbCommand(m_strQuery, connection);
                    command.Parameters.AddWithValue("@pricePoint", paramValue);

                    // Create and execute the DataReader; for this performance version just count
                    // the number of records read
                    connection.Open();
                    OleDbDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                    }
                    reader.Close();

                    // To test using OleDbDataAdapter, uncomment this:
                    /*connection.Open();
                    DataSet ds = new DataSet();
                    OleDbDataAdapter adapter = new OleDbDataAdapter(m_strQuery, connection);
                    adapter.SelectCommand.Parameters.Add("@pricePoint", OleDbType.Integer).Value = paramValue;
                    adapter.Fill(ds);
                    foreach (DataRow row in ds.Tables[0].Rows)
                    {
                        recordsRead++;
                    }*/
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            return recordsRead;
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

        private string HelperGetQueryString(QueryType eQuery)
        {
            // Use a standard query string while accessing the demo Northwind databases
            // Note: The "@pricePoint" 
            string sqlQuery = string.Empty;
            switch (eQuery)
            {
                case QueryType.eQueryStd:
                default:
                    // This query is taken directly from the online tutorial
                    sqlQuery = (
                        "SELECT ProductID, UnitPrice, ProductName FROM Products " +
                        "WHERE UnitPrice > ? " +
                        "ORDER BY UnitPrice DESC;");
                    break;

                case QueryType.eQueryLike:
                    // Testing the "LIKE" operator of the WHERE clause. This query might return:
                    // * Vegie-spread
                    // * Grandma's Boysenberry Spread
                    // * Scottish Longbreads
                    sqlQuery = (
                        "SELECT ProductID, UnitPrice, ProductName FROM Products " +
                        "WHERE ProductName LIKE '%read%' " +
                        "ORDER BY UnitPrice DESC;");
                    break;
            }

            return sqlQuery;

            // Comments in SQL statements: Use "--" (single line only) or "/*...*/" (can span multi-lines):
            //  string sqlWithComments =
            //    "SELECT ProductID, UnitPrice FROM Products  -- Select two columns from the products table" +
            //    "WHERE UnitPrice > ?                        -- Only choose rows where the unit price is NOT null" +
            //    "ORDER BY UnitPrice DESC;                   -- Order rows by the UnitPrice";
        }
        // End: Methods (private)
    }
}
