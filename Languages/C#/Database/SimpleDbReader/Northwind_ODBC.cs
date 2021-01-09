using System;
using System.Data.Odbc;

namespace SimpleDbReader
{
    class Northwind_ODBC : DatabaseCommon
    {
        // Constructor
        public Northwind_ODBC(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses ODBC
            m_tech = DatabaseTechnology.eDB_ODBC;
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (stats) ###");
            Console.WriteLine("  (TODO)");
            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection (stats) ###\n");
        }

        public override void Read() 
        {
            // System.Data.Odbc.OdbcConnection
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.odbc.odbccommand
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (read) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (AccessDbType dbType in Enum.GetValues(typeof(AccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Read(strConnection);
            }

            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection (read) ###\n");
        }

        public override void Write()
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (write) ###");
            Console.WriteLine("  (TODO)");
            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection (write) ###\n");
        }

        public override void PerformanceTest(int nLoops)
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: ODBC - Performance tests ###");
            string strConnection = string.Empty;
            foreach (AccessDbType dbType in Enum.GetValues(typeof(AccessDbType)))
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

            Console.WriteLine("### END: ODBC - Performance tests ###\n");
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            // ODBC: Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            string strDataDriver = "Driver=";
            string strDataSource = ("Dbq=" + m_cfgGeneral.strDevDataPath);
            switch (m_cfgDatabase.dbType)
            {
                case AccessDbType.eAccess97:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        strDataDriver += "{Microsoft Access Driver (*.mdb)};";
                        strDataSource += "\\Northwind 97.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error is "ERROR [IM002] [Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified"
                    }
                    break;

                case AccessDbType.eAccess2000:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        strDataDriver += "{Microsoft Access Driver (*.mdb)};";
                        strDataSource += "\\Northwind 2000.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error same as for Access 97
                    }
                    break;

                case AccessDbType.eAccess2007_2016:
                    // 64-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(false));
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

            // Example (Access 97) =
            //      Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\Apps\Data\Northwind 97.mdb;Uid=Admin;Pwd=;
            return bHaveConnectionString;
        }

        protected override void Connect_Stats(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Read(string strConnection)
        {
            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand command = new OdbcCommand(m_cfgDatabase.strQuery, connection);
                command.Parameters.AddWithValue("@pricePoint", m_cfgDatabase.paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader, writing the result to the console window
                    int recordsRead = 0;
                    Console.WriteLine("\t{0}\t{1}\t{2}",
                        "ProductID", "UnitPrice", "ProductName");

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

        protected override void Connect_Write(string strConnection)
        {
            // TODO
        }

        protected override int Connect_PerformanceTest(string strConnection)
        {
            // Version for performance testing
            int recordsRead = 0;
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand command = new OdbcCommand(m_cfgDatabase.strQuery, connection);
                command.Parameters.AddWithValue("@pricePoint", m_cfgDatabase.paramValue);

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
        #endregion // Abstract methods from the base class
    }
}
