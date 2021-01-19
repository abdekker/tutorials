﻿using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data;
using System.Data.Odbc;

namespace SimpleDbReader
{
    class Northwind_ODBC : DatabaseCommon
    {
        // Member variables specific to this class
        private readonly Utilities_DbConnection m_utilsDbConnection = new Utilities_DbConnection(DatabaseTechnology.eDB_ODBC);

        private DatabaseReadTechnology m_eDbReadTechnology = DatabaseReadTechnology.eRbRead_DataReader;
        private DatabaseAccess m_dbAccess = DatabaseAccess.eDbAccess_Raw;

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
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (stats, Northwind) ###");

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
            // System.Data.Odbc.OdbcConnection
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.odbc.odbccommand
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (read, Northwind) ###");

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

            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection (read) ###\n");
        }

        public override void Write()
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: System.Data.Odbc.OdbcConnection (write, Northwind) ###");
            Console.WriteLine("  (TODO)");
            Console.WriteLine("### END: System.Data.Odbc.OdbcConnection (write) ###\n");
        }

        public override void PerformanceTest(int nLoops)
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: ODBC - Performance tests (Northwind) ###");
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                {
                    foreach (DatabaseReadTechnology dbReadTech in Enum.GetValues(typeof(DatabaseReadTechnology)))
                    {
                        m_eDbReadTechnology = dbReadTech;
                        Console.Write("    ({0}) ", m_utilsDbConnection.GetReadTechnologyAsString(m_eDbReadTechnology));

                        int totalRecordsRead = 0;
                        int startTicks = Environment.TickCount;
                        for (int loop = 0; loop < nLoops; loop++)
                            totalRecordsRead += Connect_PerformanceTest(strConnection);

                        int elapsedTicks = (Environment.TickCount - startTicks);
                        Console.WriteLine("({0} cycles: Took {1}ms at an avg of {2:0.00}ms to read an avg of {3:0.0} records)",
                            nLoops,
                            elapsedTicks,
                            (float)elapsedTicks / (float)nLoops,
                            (float)totalRecordsRead / (float)nLoops);
                    }
                }
            }

            Console.WriteLine("### END: ODBC - Performance tests ###\n");
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            //ADAD
            //OdbcCommandBuilder builder = new OdbcCommandBuilder(adapter);

            // ODBC: Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            string strDataDriver = "Driver=";
            string strDataSource = ("Dbq=" + m_cfgGeneral.strDevDataPath);
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
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

                case MSAccessDbType.eMSAccess2000:
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

                case MSAccessDbType.eMSAccess2007_2016:
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

        protected override void Connect_Read(string strConnection)
        {
            // Read records from the database using ODBC
            if (m_dbAccess == DatabaseAccess.eDbAccess_Raw)
                Connect_Read_Raw(strConnection);
            else if (m_dbAccess == DatabaseAccess.eDbAccess_Template)
                Connect_Read_Template(strConnection);
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
                // Use template methods
                Collection<Northwind_Products> products = null;
                if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
                {
                    // Using System.Data.Odbc.OdbcDataReader : IDataReader
                    using (NorthwindReader_Products reader = new NorthwindReader_Products())
                    {
                        reader.DbTechnology = m_tech;
                        reader.ConnectionString = strConnection;
                        reader.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                        products = reader.Execute();
                    }
                }
                else if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataAdapter)
                {
                    // Using System.Data.Odbc.OdbcDataAdapter : IDbDataAdapter
                    using (NorthwindAdapter_Products adapter = new NorthwindAdapter_Products())
                    {
                        adapter.DbTechnology = m_tech;
                        adapter.ConnectionString = strConnection;
                        adapter.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                        products = adapter.Execute();
                    }
                }

                recordsRead = products.Count;

                // Use raw DbDataReader
                /* // Create the Command and Parameter objects
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
                }*/
            }

            return recordsRead;
        }
        #endregion // Abstract methods from the base class

        #region Methods specific to this class
        private void Connect_Read_Raw(string strConnection)
        {
            // Create and open the connection in a using block. This ensures that all resources will be
            // closed and disposed when the code exits.
            if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
            {
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
                        Northwind_Products rsTmp = new Northwind_Products();
                        Console.WriteLine("\t{0}{1}{2}",
                            Northwind_Products.colProductID.PadRight(Northwind_Products.colProductIDWidth),
                            Northwind_Products.colUnitPrice.PadRight(Northwind_Products.colUnitPriceWidth),
                            Northwind_Products.colProductName);

                        int recordsRead = 0;
                        connection.Open();
                        OdbcDataReader reader = command.ExecuteReader();
                        while (reader.Read())
                        {
                            recordsRead++;
                            Console.WriteLine("\t{0}{1}{2}",
                                ((int)reader[Northwind_Products.colProductID]).ToString().PadRight(Northwind_Products.colProductIDWidth),
                                ((decimal)reader[Northwind_Products.colUnitPrice]).ToString("0.00").PadRight(Northwind_Products.colUnitPriceWidth),
                                (string)reader[Northwind_Products.colProductName]);
                            // Note: The reader returns 3 items (since the query was for three fields). You can access them
                            // with reader[0], reader[1], and so on. Using the field name is preferable, though, because
                            // then we are less dependent on the order of fields in the SQL query.
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
            // Do nothing for DatabaseReadTechnology.eRbRead_DataAdapter
        }

        private void Connect_Read_Template(string strConnection)
        {
            // This method uses a template method to create a Data Access Layer (DAL) to the database
            Console.Write("(template)");
            Collection<Northwind_Products> products = null;
            if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
            {
                // Using System.Data.Odbc.OdbcDataReader : IDataReader
                Console.WriteLine(" (Odbc.OdbcDataReader)");
                using (NorthwindReader_Products reader = new NorthwindReader_Products())
                {
                    reader.DbTechnology = m_tech;
                    reader.ConnectionString = strConnection;
                    reader.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                    products = reader.Execute();
                }
            }
            else if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataAdapter)
            {
                // Using System.Data.Odbc.OdbcDataAdapter : IDbDataAdapter
                Console.WriteLine(" (Odbc.OdbcDataAdapter)");
                using (NorthwindAdapter_Products adapter = new NorthwindAdapter_Products())
                {
                    adapter.DbTechnology = m_tech;
                    adapter.ConnectionString = strConnection;
                    adapter.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                    products = adapter.Execute();
                }
            }

            int recordsRead = 0;
            //Console.WriteLine(Northwind_Products.GetRecordHeader());
            Console.WriteLine("\t{0}{1}{2}",
                Northwind_Products.colProductID.PadRight(Northwind_Products.colProductIDWidth),
                Northwind_Products.colUnitPrice.PadRight(Northwind_Products.colUnitPriceWidth),
                Northwind_Products.colProductName);
            foreach (Northwind_Products p in products)
            {
                recordsRead++;
                //Console.WriteLine(p.GetRecordAsString());
                Console.WriteLine("\t{0}{1}{2}",
                    p.ProductID.ToString().PadRight(Northwind_Products.colProductIDWidth),
                    p.UnitPrice.ToString("0.00").PadRight(Northwind_Products.colUnitPriceWidth),
                    p.ProductName.ToString());
            }
            Console.WriteLine("    ({0} records)", recordsRead);
            Console.WriteLine();
        }

        private void ConvertRecordset(in OdbcDataReader reader, ref Northwind_Products rsProduct)
        {
            // Convert the ODBC recordset to a local, strongly-typed, version
            rsProduct.DefaultRecord();
            try
            {
                rsProduct.ProductID = (int)reader[0];
            }
            catch { }

            try
            {
                rsProduct.ProductName = (string)reader[1];
            }
            catch { }

            try
            {
                rsProduct.SupplierID = (int)reader[2];
            }
            catch { }

            try
            {
                rsProduct.CategoryID = (int)reader[3];
            }
            catch { }

            try
            {
                rsProduct.QuantityPerUnit = (string)reader[4];
            }
            catch { }

            try
            {
                rsProduct.UnitPrice = (decimal)reader[5];
            }
            catch { }

            try
            {
                rsProduct.UnitsInStock = (int)reader[6];
            }
            catch { }

            try
            {
                rsProduct.UnitsOnOrder = (int)reader[7];
            }
            catch { }

            try
            {
                rsProduct.ReorderLevel = (int)reader[8];
            }
            catch { }

            try
            {
                rsProduct.Discontinued = (bool)reader[9];
            }
            catch { }
        }
        #endregion // Methods specific to this class
    }
}
