using System;
using System.Data.Odbc;

namespace SimpleDbReader
{
    class Simple_ODBC : DatabaseCommon
    {
        // Constructor
        public Simple_ODBC(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses DAO
            m_tech = DatabaseTechnology.eDB_DAO;

            // Set the main database query
            m_cfgDatabase.queryType = QueryType.eQueryStd2;
            m_cfgDatabase.strQuery = HelperGetQueryString();
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            // TODO
        }

        public override void Read()
        {
            // DAO (Data Access Objects)
            Console.WriteLine("### START: Simple ODBC (read) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (AccessDbType dbType in Enum.GetValues(typeof(AccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Read(strConnection);
            }

            Console.WriteLine("### END: Simple DAO (read) ###\n");
        }

        public override void Write()
        {
            // TODO
        }

        public override void PerformanceTest(int nLoops)
        {
            // TODO
        }

        protected override void Connect_Stats(string strConnection)
        {
            // TODO
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
                        strDataSource += "\\SimpleTest.mdb;";
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
                        strDataSource += "\\SimpleTest.mdb;";
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
                        strDataSource += "\\SimpleTest.accdb;";
                    }

                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            if (bHaveConnectionString)
                strConnection = (strDataDriver + strDataSource + "Uid=Admin;Pwd=;");

            // Example (Access 97) =
            //      Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\Apps\Data\SimpleTest.mdb;Uid=Admin;Pwd=;
            return bHaveConnectionString;
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
                    Console.WriteLine("\t{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}",
                        CommonSimple.colMemberID,
                        CommonSimple.colSurname,
                        CommonSimple.colFirstName,
                        CommonSimple.colDOB,
                        CommonSimple.colFee,
                        CommonSimple.colAccepted,
                        CommonSimple.colPoints);

                    connection.Open();
                    OdbcDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                        Console.WriteLine("\t{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}",
                            reader[0], reader[1], reader[2], reader[3], reader[4], reader[5], reader[6]);
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
            // TODO
            return 0;
        }
        #endregion // Abstract methods from the base class
    }
}
