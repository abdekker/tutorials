using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data;
using System.Data.Odbc;

namespace SimpleDbReader
{
    class Simple_ODBC : DatabaseCommon
    {
        // Member variables specific to this class
        private readonly Utilities_DbConnection m_utilsDbConnection = new Utilities_DbConnection(DatabaseTechnology.eDB_ODBC);

        private DatabaseReadTechnology m_eDbReadTechnology = DatabaseReadTechnology.eRbRead_DataReader;
        private DatabaseAccess m_dbAccess = DatabaseAccess.eDbAccess_Raw;

        // Constructor
        public Simple_ODBC(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses ODBC
            m_tech = DatabaseTechnology.eDB_ODBC;

            // Set the main database query
            m_cfgDatabase.queryType = QueryType.eQueryStd2;
            m_cfgDatabase.strQuery = HelperGetQueryString();
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (stats, SimpleTest.mdb) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Stats(strConnection);
            }

            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection (stats) ###\n");
        }

        public override void Read()
        {
            // DAO (Data Access Objects)
            Console.WriteLine("### START: Simple ODBC (read, SimpleTest.mdb) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                {
                    foreach (DatabaseAccess dbAccess in Enum.GetValues(typeof(DatabaseAccess)))
                    {
                        foreach (DatabaseReadTechnology dbReadTech in Enum.GetValues(typeof(DatabaseReadTechnology)))
                        {
                            m_dbAccess = dbAccess;
                            m_eDbReadTechnology = dbReadTech;
                            Connect_Read(strConnection);
                        }
                    }
                }
            }

            Console.WriteLine("### END: Simple DAO (read) ###\n");
        }

        public override void Writeable()
        {
            // TODO
        }

        public override void Insert()
        {
            // TODO
        }

        public override void Update()
        {
            // TODO
        }

        public override void Delete()
        {
            // TODO
        }

        public override void PerformanceTest(int nLoops)
        {
            // TODO
        }

        protected override void Connect_Stats(string strConnection)
        {
            string dbName = m_utilsDbConnection.GetDbName(strConnection);
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                connection.Open();
                DataTable schema = connection.GetSchema("Tables"); // Other useful schema include "Procedures" and "Views"
                List<string> tables = m_utilsDbConnection.GetSchemaInfo(connection, "Tables", true);
                List<string> fields;
                if (tables.Count > 0)
                {
                    Console.WriteLine("    ({0} tables in {1})", tables.Count, dbName);
                    foreach (string tb in tables)
                    {
                        Console.WriteLine("      {0}", tb);
                        fields = m_utilsDbConnection.GetFields(connection, tb);
                        foreach (string fd in fields)
                        {
                            Console.WriteLine("        {0}", fd);
                        }
                    }
                }
                else
                    Console.WriteLine("    (not tables in {0})", connection.Database);
            }
            Console.WriteLine();
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            // ODBC: Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            string strDataDriver = ("Driver=" + m_utilsDbConnection.GetConnectionDetailsDriver(m_cfgGeneral.b64bit) + ";");
            string strDataSource = ("Dbq=" + m_cfgGeneral.strDevDataPath);
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
                case MSAccessDbType.eMSAccess2000:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                        strDataSource += "\\SimpleTest.mdb;";
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error is "ERROR [IM002] [Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified"
                    }
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    // 64-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(false));
                        // Error same as for Access 97
                    }
                    else
                        strDataSource += "\\SimpleTest.accdb;";

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
            if (m_dbAccess == DatabaseAccess.eDbAccess_Raw)
                Connect_Read_Raw(strConnection);
            else if (m_dbAccess == DatabaseAccess.eDbAccess_Template)
                Connect_Read_Template(strConnection);
        }

        protected override void Connect_Writeable(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Insert(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Update(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Delete(string strConnection)
        {
            // TODO
        }

        protected override int Connect_PerformanceTest(string strConnection)
        {
            // TODO
            return 0;
        }
        #endregion // Abstract methods from the base class

        #region Methods specific to this class
        private void Connect_Read_Raw(string strConnection)
        {
            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            Console.WriteLine("(raw)");
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                // Create the Command and Parameter objects
                OdbcCommand command = new OdbcCommand(m_cfgDatabase.strQuery, connection);
                command.Parameters.AddWithValue("@pricePoint", m_cfgDatabase.paramValue);

                // Open the connection in a try/catch block
                try
                {
                    // Create and execute the DataReader, writing the result to the console window
                    Simple_Members rsTmp = new Simple_Members();
                    Console.WriteLine(rsTmp.GetRecordHeader());

                    int recordsRead = 0;
                    connection.Open();
                    OdbcDataReader reader = command.ExecuteReader();
                    while (reader.Read())
                    {
                        recordsRead++;
                        ConvertRecordset(in reader, ref rsTmp);
                        Console.WriteLine(rsTmp.GetRecordAsString());
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

        private void Connect_Read_Template(string strConnection)
        {
            // This method uses a template method to create a Data Access Layer (DAL) to the database
            Console.Write("(template)");
            Collection<Simple_Members> members = null;
            if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
            {
                // Using System.Data.Odbc.OdbcDataReader : IDataReader
                Console.WriteLine(" (Odbc.OdbcDataReader)");
                using (SimpleReader_Members reader = new SimpleReader_Members())
                {
                    reader.DbTechnology = m_tech;
                    reader.ConnectionString = strConnection;
                    reader.CmdText = m_cfgDatabase.strQuery;
                    members = reader.Execute();
                }
            }
            else if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataAdapter)
            {
                // Using System.Data.Odbc.OdbcDataAdapter : IDbDataAdapter
                Console.WriteLine(" (Odbc.OdbcDataAdapter)");
                using (SimpleAdapter_Members adapter = new SimpleAdapter_Members())
                {
                    adapter.DbTechnology = m_tech;
                    adapter.ConnectionString = strConnection;
                    adapter.CmdText = m_cfgDatabase.strQuery;
                    members = adapter.Execute();
                }
            }

            int recordsRead = 0;
            foreach (Simple_Members m in members)
            {
                if (recordsRead == 0)
                    Console.WriteLine(m.GetRecordHeader());

                recordsRead++;
                Console.WriteLine(m.GetRecordAsString());
            }
            Console.WriteLine("    ({0} records)", recordsRead);
            Console.WriteLine();
        }

        private void ConvertRecordset(in OdbcDataReader reader, ref Simple_Members rsMember)
        {
            // Convert the ODBC recordset to a local, strongly-typed, version
            rsMember.DefaultRecord();
            try
            {
                // Alternatively, use the column name:
                //      rsMember.MemberID = (int)m_utilsDAO.SafeGetFieldValue(rsDAO, Simple_Members.colMemberID); or
                //      rsMember.MemberID = (int)reader[Simple_Members.colMemberID];
                rsMember.MemberID = (int)reader[0];
            }
            catch { }

            try
            {
                rsMember.Surname = (string)reader[1];
            }
            catch { }

            try
            {
                rsMember.FirstName = (string)reader[2];
            }
            catch { }

            try
            {
                rsMember.DOB = (DateTime)reader[3];
            }
            catch { }

            try
            {
                rsMember.Fee = (Decimal)reader[4];
            }
            catch { }

            try
            {
                rsMember.Accepted = (bool)reader[5];
            }
            catch { }

            try
            {
                rsMember.Points = (int)reader[6];
            }
            catch { }
        }
        #endregion // Methods specific to this class
    }
}
