using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data;
using System.Data.OleDb;

namespace SimpleDbReader
{
    class Northwind_OleDB : DatabaseCommon
    {
        // Member variables specific to this class
        private readonly Utilities_DbConnection m_utilsDbConnection = new Utilities_DbConnection(DatabaseTechnology.eDB_OleDb);

        private DatabaseReadTechnology m_eDbReadTechnology = DatabaseReadTechnology.eRbRead_DataReader;
        private DatabaseAccess m_dbAccess = DatabaseAccess.eDbAccess_Raw;

        // Constructor
        public Northwind_OleDB(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses OleDB
            m_tech = DatabaseTechnology.eDB_OleDb;
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            // System.Data.Odbc.OdbcConnection
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand (stats, Northwind) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Stats(strConnection);
            }

            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand (stats) ###\n");
        }

        public override void Read()
        {
            //  System.Data.OleDb.OleDbCommand
            // See: https://docs.microsoft.com/en-us/dotnet/api/system.data.oledb.oledbcommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand (read, Northwind) ###");

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

            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand (read) ###\n");
        }

        public override void Writeable()
        {
            //  System.Data.OleDb.OleDbCommand
            Console.WriteLine("### START: System.Data.OleDb.OleDbCommand (writeable, Northwind) ###");
            Console.WriteLine("  (TODO)");
            Console.WriteLine("### END: System.Data.OleDb.OleDbCommand (write) ###\n");
        }

        public override void Insert()
        {
            // System.Data.OleDb.OleDbCommand
            // TODO
        }

        public override void Update()
        {
            // System.Data.OleDb.OleDbCommand
            // TODO
        }

        public override void Delete()
        {
            // System.Data.OleDb.OleDbCommand
            // TODO
        }

        public override void PerformanceTest(int nLoops)
        {
            //  System.Data.OleDb.OleDbCommand
            Console.WriteLine("### START: OleDb - Performance tests (Northwind) ###");
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

            Console.WriteLine("### END: OleDb - Performance tests ###\n");
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            // OleDB: Set up the connection string based on the version of the Access database
            bool bHaveConnectionString = true;
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
                case MSAccessDbType.eMSAccess2000:
                    // 32-bit only
                    if (m_cfgGeneral.b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error is "The 'Microsoft.Jet.OLEDB.4.0' provider is not registered on the local machine."
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
                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            if (bHaveConnectionString)
            {
                string strDataDriver = ("Provider=" +
                    m_utilsDbConnection.GetConnectionDetailsDriver(m_cfgGeneral.b64bit) + ";");
                string strDataSource = ("Data Source=" +
                    m_cfgGeneral.strDevDataPath + "\\" +
                    m_utilsDbConnection.GetConnectionDetailsFilename(m_cfgDatabase.dbType) + ";");
                strConnection = (strDataDriver + strDataSource + "User Id=admin;Password=;");
            }

            // Example (Access 97) =
            //      Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\Northwind 97.mdb;;User Id=admin;Password=;
            return bHaveConnectionString;
        }

        protected override void Connect_Stats(string strConnection)
        {
            string dbName = m_utilsDbConnection.GetDbName(strConnection);
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                connection.Open();
                Console.WriteLine("  (connection is: {0})", m_utilsDbConnection.GetConnectionStateAsString(connection));

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
            // Read records from the database using OleDB
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
            // Version for performance testing
            int recordsRead = 0;

            // This version uses System.Data.OleDb.OleDbDataReader
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                Collection<Northwind_Products> products = null;
                if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
                {
                    // Using System.Data.OleDb.OleDbDataReader : IDataReader
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
                    // Using System.Data.OleDb.OleDbDataAdapter : IDbDataAdapter
                    using (NorthwindAdapter_Products adapter = new NorthwindAdapter_Products())
                    {
                        adapter.DbTechnology = m_tech;
                        adapter.ConnectionString = strConnection;
                        adapter.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                        products = adapter.Execute();
                    }
                }

                recordsRead = products.Count;
            }

            return recordsRead;
        }
        #endregion // Abstract methods from the base class

        #region Methods specific to this class
        private void Connect_Read_Raw(string strConnection)
        {
            // Create and open the connection in a using block. This ensures that all resources
            // will be closed and disposed when the code exits.
            Console.WriteLine("(raw)");
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                // Open the connection in a try/catch block
                try
                {
                    if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
                    {
                        // Using System.Data.OleDb.OleDbDataReader : IDataReader
                        Console.WriteLine("(OleDb.OleDbDataReader)");

                        // Create and open the connection in a using block. This ensures that all resources
                        // will be closed and disposed when the code exits.

                        // Note:
                        // * "DbConnection" is the base class for OleDbConnection, OdbcConnection
                        // * "DbCommand" is the base class for OleDbCommand, OdbcCommand
                        // * "DbParameterCollection" is the base class for OleDbParameterCollection, OdbcParameterCollection
                        // * "DbDataReader" is the base class for OleDbDataReader, OdbcDataReader
                        // We could write a generic function to access the database with OleDB or ODBC, but it
                        // is simpler to write an unique method for each technology.

                        // Create and execute the DataReader, writing the result to the console window
                        int recordsRead = 0;
                        Console.WriteLine("\t{0}{1}{2}",
                            Northwind_Products.colProductID.PadRight(Northwind_Products.colProductIDWidth),
                            Northwind_Products.colUnitPrice.PadRight(Northwind_Products.colUnitPriceWidth),
                            Northwind_Products.colProductName);

                        connection.Open();
                        OleDbCommand command = new OleDbCommand(m_cfgDatabase.strQuery, connection);
                        command.Parameters.AddWithValue("@pricePoint", m_cfgDatabase.paramValue);
                        // This also works: command.Parameters.AddWithValue(string.Empty, paramValue);

                        OleDbDataReader reader = command.ExecuteReader();
                        while (reader.Read())
                        {
                            recordsRead++;
                            Console.WriteLine("\t{0}{1}{2}",
                                ((int)reader[Northwind_Products.colProductID]).ToString().PadRight(Northwind_Products.colProductIDWidth),
                                ((decimal)reader[Northwind_Products.colUnitPrice]).ToString("0.00").PadRight(Northwind_Products.colUnitPriceWidth),
                                (string)reader[Northwind_Products.colProductName]);
                        }
                        reader.Close();
                        Console.WriteLine("    ({0} records)", recordsRead);
                    }
                    else if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
                    {
                        // This version uses:
                        // * System.Data.DataSet
                        // * System.Data.OleDb.OleDbDataAdapter : IDbDataAdapter
                        // * System.Data.DataRow
                        Console.WriteLine("(DataSet, OleDb.OleDbDataAdapter and DataRow)");

                        // Create and fill the DataSet, writing the result to the console window
                        int recordsRead = 0;
                        Console.WriteLine("\t{0}{1}{2}",
                            Northwind_Products.colProductID.PadRight(Northwind_Products.colProductIDWidth),
                            Northwind_Products.colUnitPrice.PadRight(Northwind_Products.colUnitPriceWidth),
                            Northwind_Products.colProductName);

                        connection.Open();
                        DataSet ds = new DataSet();
                        OleDbDataAdapter adapter = new OleDbDataAdapter(m_cfgDatabase.strQuery, connection);
                        adapter.SelectCommand.Parameters.Add("@pricePoint", OleDbType.Integer).Value = m_cfgDatabase.paramValue;
                        adapter.Fill(ds);
                        foreach (DataRow row in ds.Tables[0].Rows)
                        {
                            recordsRead++;
                            Console.WriteLine("\t{0}{1}{2}",
                                ((int)row[Northwind_Products.colProductID]).ToString().PadRight(Northwind_Products.colProductIDWidth),
                                ((decimal)row[Northwind_Products.colUnitPrice]).ToString("0.00").PadRight(Northwind_Products.colUnitPriceWidth),
                                (string)row[Northwind_Products.colProductName]);
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

        private void Connect_Read_Template(string strConnection)
        {
            // This method uses a template method to create a Data Access Layer (DAL) to the database
            Console.Write("(template)");
            if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
            {
                // Using System.Data.OleDb.OleDbDataReader : IDataReader
                Console.WriteLine(" (OleDb.OleDbDataReader, strongly typed)");
                Collection<Northwind_Products> products = null;
                using (NorthwindReader_Products reader = new NorthwindReader_Products())
                {
                    reader.DbTechnology = m_tech;
                    reader.ConnectionString = strConnection;
                    reader.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                    products = reader.Execute();
                }
                Connect_Read_Template_Typed(ref products);
            }
            else if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataAdapter)
            {
                // Using System.Data.OleDb.OleDbDataAdapter : IDbDataAdapter
                Console.WriteLine(" (OleDb.OleDbDataAdapter, strongly typed)");
                Collection<Northwind_Products> products = null;
                using (NorthwindAdapter_Products adapter = new NorthwindAdapter_Products())
                {
                    adapter.DbTechnology = m_tech;
                    adapter.ConnectionString = strConnection;
                    adapter.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                    products = adapter.Execute();
                }
                Connect_Read_Template_Typed(ref products);
            }
            else if (m_eDbReadTechnology == DatabaseReadTechnology.eRbRead_DataSet)
            {
                // Using System.Data.OleDb.OleDbDataAdapter : IDbDataAdapter
                Console.WriteLine(" (OleDb.OleDbDataAdapter, raw DataSet)");
                DataSet products = null;
                ObjectDataSetRaw adapter = new ObjectDataSetRaw();
                adapter.DbTechnology = m_tech;
                adapter.ConnectionString = strConnection;
                adapter.CmdText = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());
                products = adapter.Execute();
                Connect_Read_Template_Raw(ref products);
            }

            Console.WriteLine();
        }

        private void Connect_Read_Template_Typed(ref Collection<Northwind_Products> products)
        {
            // When reading strongly typed data from the database:
            // * DatabaseReadTechnology.eRbRead_DataReader
            // * DatabaseReadTechnology.eRbRead_DataAdapter
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
        }

        private void Connect_Read_Template_Raw(ref DataSet products)
        {
            // When reading raw data from the database (DatabaseReadTechnology.eRbRead_DataReader)
            int recordsRead = 0;
            Console.WriteLine("\t{0}{1}{2}",
                Northwind_Products.colProductID.PadRight(Northwind_Products.colProductIDWidth),
                Northwind_Products.colUnitPrice.PadRight(Northwind_Products.colUnitPriceWidth),
                Northwind_Products.colProductName);

            foreach (DataRow p in products.Tables[0].Rows)
            {
                try
                {
                    recordsRead++;
                    Console.WriteLine("\t{0}{1}{2}",
                        ((int)p[Northwind_Products.colProductID]).ToString().PadRight(Northwind_Products.colProductIDWidth),
                        ((decimal)p[Northwind_Products.colUnitPrice]).ToString("0.00").PadRight(Northwind_Products.colUnitPriceWidth),
                        (string)p[Northwind_Products.colProductName]);
                }
                catch
                {
                    //throw;
                    // Consider handling exception (instead of re-throwing) if graceful recovery is possible
                }
            }
            Console.WriteLine("    ({0} records)", recordsRead);
        }

        private void ConvertRecordset(in OleDbDataReader reader, ref Northwind_Products rsProduct)
        {
            // Convert the OleDbDataReader recordset to a local, strongly-typed, version (overload)
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
                rsProduct.Discontinued = (bool)reader[8];
            }
            catch { }
        }

        private void ConvertRecordset(in DataRow row, ref Northwind_Products rsProduct)
        {
            // Convert the DataRow record to a local, strongly-typed, version (overload)
            rsProduct.DefaultRecord();
            try
            {
                rsProduct.ProductID = (int)row[Northwind_Products.colProductID];
            }
            catch { }

            try
            {
                rsProduct.ProductName = (string)row[Northwind_Products.colProductName];
            }
            catch { }

            try
            {
                rsProduct.SupplierID = (int)row[Northwind_Products.colSupplierID];
            }
            catch { }

            try
            {
                rsProduct.CategoryID = (int)row[Northwind_Products.colCategoryID];
            }
            catch { }

            try
            {
                rsProduct.QuantityPerUnit = (string)row[Northwind_Products.colQuantityPerUnit];
            }
            catch { }

            try
            {
                rsProduct.UnitPrice = (decimal)row[Northwind_Products.colUnitPrice];
            }
            catch { }

            try
            {
                rsProduct.UnitsInStock = (int)row[Northwind_Products.colUnitsInStock];
            }
            catch { }

            try
            {
                rsProduct.UnitsOnOrder = (int)row[Northwind_Products.colUnitsOnOrder];
            }
            catch { }

            try
            {
                rsProduct.ReorderLevel = (int)row[Northwind_Products.colReorderLevel];
            }
            catch { }

            try
            {
                rsProduct.Discontinued = (bool)row[Northwind_Products.colDiscontinued];
            }
            catch { }
        }
        #endregion // Methods specific to this class
    }
}
