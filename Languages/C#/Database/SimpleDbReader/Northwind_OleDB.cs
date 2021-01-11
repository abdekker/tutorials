using System;
using System.Data;
using System.Data.OleDb;

namespace SimpleDbReader
{
    class Northwind_OleDB : DatabaseCommon
    {
        // Enumerations
        private enum OleDBTechnology
        {
            // The OleDB technology used for reading data
            eOleDB_DataReader,      // System.Data.OleDb.OleDbDataReader
            eOleDB_DataAdapter      // System.Data.OleDb.OleDbDataAdapter
        };

        // Member variables
        private OleDBTechnology m_eOleDBTechnology = OleDBTechnology.eOleDB_DataReader;

        // Constructor
        public Northwind_OleDB(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses OleDB
            m_tech = DatabaseTechnology.eDB_OleDB;
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            //  System.Data.OleDb.OleDbCommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand (stats) ###");
            Console.WriteLine("  (TODO)");
            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand (stats) ###\n");
        }

        public override void Read()
        {
            //  System.Data.OleDb.OleDbCommand
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbcommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand (read) ###");

            // Use OleDbDataReader or OleDbDataAdapter?
            m_eOleDBTechnology = OleDBTechnology.eOleDB_DataReader;

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Read(strConnection);
            }

            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand (read) ###\n");
        }

        public override void Write()
        {
            //  System.Data.OleDb.OleDbCommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand (write) ###");
            Console.WriteLine("  (TODO)");
            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand (write) ###\n");
        }

        public override void PerformanceTest(int nLoops)
        {
            //  System.Data.OleDb.OleDbCommand
            Console.WriteLine("### START: OleDb - Performance tests ###");

            // Use OleDbDataReader or OleDbDataAdapter?
            m_eOleDBTechnology = OleDBTechnology.eOleDB_DataReader;

            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                {
                    int totalRecordsRead = 0;
                    int startTicks = Environment.TickCount;
                    for (int loop = 0; loop < nLoops; loop++)
                        totalRecordsRead += Connect_PerformanceTest(strConnection);

                    int elapsedTicks = (Environment.TickCount - startTicks);
                    Console.WriteLine("    ({0} cycles: Took {1}ms at an avg of {2:0.00}ms to read an avg of {3:0.0} records)",
                        nLoops,
                        elapsedTicks,
                        (float)elapsedTicks / (float)nLoops,
                        (float)totalRecordsRead / (float)nLoops);
                }
            }

            Console.WriteLine("### END: OleDb - Performance tests ###\n");
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            // OleDB: Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            string strDataDriver = "Provider=";
            string strDataSource = ("Data Source=" + m_cfgGeneral.strDevDataPath);
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        strDataDriver += "Microsoft.Jet.OLEDB.4.0;";
                        strDataSource += "\\Northwind 97.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error is "The 'Microsoft.Jet.OLEDB.4.0' provider is not registered on the local machine."
                    }
                    break;

                case MSAccessDbType.eMSAccess2000:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        strDataDriver += "Microsoft.Jet.OLEDB.4.0;";
                        strDataSource += "\\Northwind 2000.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error same as for Access 97
                    }
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    // 64-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(false));
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

            // Example (Access 97) =
            //      Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\Northwind 97.mdb;;User Id=admin;Password=;
            return bHaveConnectionString;
        }

        protected override void Connect_Stats(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Read(string strConnection)
        {
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                // Open the connection in a try/catch block
                try
                {
                    if (m_eOleDBTechnology == OleDBTechnology.eOleDB_DataReader)
                    {
                        // Using System.Data.OleDb.OleDbDataReader

                        // Create and open the connection in a using block. This ensures that all resources
                        // will be closed and disposed when the code exits.

                        // Note:
                        // * "DbConnection" is the base class for OleDbConnection, OdbcConnection
                        // * "DbCommand" is the base class for OleDbCommand, OdbcCommand
                        // * "DbParameterCollection" is the base class for OleDbParameterCollection, OdbcParameterCollection
                        // * "DbDataReader" is the base class for OleDbDataReader, OdbcDataReader
                        // We could write a generic function to access the database with OleDB or ODBC, but it
                        // is simpler to write an unique method for each technology.

                        // Create the Command object
                        OleDbCommand command = new OleDbCommand(m_cfgDatabase.strQuery, connection);
                        command.Parameters.AddWithValue("@pricePoint", m_cfgDatabase.paramValue);
                        // This also works: command.Parameters.AddWithValue(string.Empty, paramValue);

                        // Create and execute the DataReader, writing the result to the console window
                        int recordsRead = 0;
                        Console.WriteLine("\t{0}\t{1}\t{2}",
                            "ProductID", "UnitPrice", "ProductName");

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
                    else if (m_eOleDBTechnology == OleDBTechnology.eOleDB_DataAdapter)
                    {
                        // This version uses:
                        // * System.Data.DataSet
                        // * System.Data.OleDb.OleDbDataAdapter
                        // * System.Data.DataRow

                        // Create and fill the DataSet, writing the result to the console window
                        int recordsRead = 0;
                        Console.WriteLine("\t{0}\t{1}\t{2}",
                            "ProductID", "UnitPrice", "ProductName");

                        connection.Open();
                        DataSet ds = new DataSet();
                        OleDbDataAdapter adapter = new OleDbDataAdapter(m_cfgDatabase.strQuery, connection);
                        adapter.SelectCommand.Parameters.Add("@pricePoint", OleDbType.Integer).Value = m_cfgDatabase.paramValue;
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
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            Console.WriteLine();
        }

        protected override void Connect_Write(string strConnection)
        {
            // TODO
        }

        protected override int Connect_PerformanceTest(string strConnection)
        {
            // Version for performance testing
            int recordsRead = 0;

            // This version uses System.Data.OleDb.OleDbDataReader
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                // Open the connection in a try/catch block
                try
                {
                    if (m_eOleDBTechnology == OleDBTechnology.eOleDB_DataReader)
                    {
                        // Using System.Data.OleDb.OleDbDataReader

                        // Create the Command object
                        OleDbCommand command = new OleDbCommand(m_cfgDatabase.strQuery, connection);
                        command.Parameters.AddWithValue("@pricePoint", m_cfgDatabase.paramValue);

                        // Create and execute the DataReader; for this performance version just count
                        // the number of records read
                        connection.Open();
                        OleDbDataReader reader = command.ExecuteReader();
                        while (reader.Read())
                        {
                            recordsRead++;
                        }
                        reader.Close();
                    }
                    else if (m_eOleDBTechnology == OleDBTechnology.eOleDB_DataAdapter)
                    {
                        // Using System.Data.OleDb.OleDbDataAdapter
                        connection.Open();
                        DataSet ds = new DataSet();
                        OleDbDataAdapter adapter = new OleDbDataAdapter(m_cfgDatabase.strQuery, connection);
                        adapter.SelectCommand.Parameters.Add("@pricePoint", OleDbType.Integer).Value = m_cfgDatabase.paramValue;
                        adapter.Fill(ds);
                        foreach (DataRow row in ds.Tables[0].Rows)
                        {
                            recordsRead++;
                        }
                    }
                }
                catch (Exception ex)
                {
                    Console.WriteLine(ex.Message);
                }
            }

            return recordsRead;
        }
        #endregion // Abstract methods from the base class
    }
}
