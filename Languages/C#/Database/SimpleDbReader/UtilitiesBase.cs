// Uncomment this to use a different test database
// Note: Use for generating statistics on the test database, or provide a custom (classes are written with a
// specific database structure in mind)
//#define USE_HMQ_DATABASE

using System.Data;

namespace SimpleDbReader
{
    abstract class UtilitiesBase
    {
        // Base class for utilities for using DAO, ODBC, OleDB, etc

        // Constants
        public readonly string Schema_Header_Column_Formatting = "{0,-25}{1,-8}{2,-18}{3,-13}{4}";
        public readonly string Schema_Header_Column_Name = "Name";
        public readonly string Schema_Header_Column_Type = "Type";
        public readonly string Schema_Header_Column_TypeName = "TypeName";
        public readonly string Schema_Header_Column_Size = "Size";
        public readonly string Schema_Header_Column_Nullable = "Nullable";

        private readonly string DbDriver_ODBC_32bit = "{Microsoft Access Driver (*.mdb)}";
        private readonly string DbDriver_ODBC_64bit = "{Microsoft Access Driver (*.mdb, *.accdb)}";   
        private readonly string DbDriver_OleDb_32bit = "Microsoft.Jet.OLEDB.4.0";
        private readonly string DbDriver_OleDb_64bit = "Microsoft.ACE.OLEDB.16.0";

        //private readonly string DbName_Simple = "SimpleTest.mdb";
        private readonly string DbName_Northwind97 = "Northwind 97.mdb";
        private readonly string DbName_Northwind2000 = "Northwind 97.mdb";
        private readonly string DbName_Northwind2007_2016 = "Northwind 2007-2016.accdb";
        private readonly string DbName_HMQ = "HMQ.mdb";     // Used when "USE_HMQ_DATABASE" is defined

        // Member variables
        protected DatabaseTechnology m_tech;

        // Property accessors (for member variables)
        public abstract DatabaseTechnology DbTechnology { get; set; }

        // Abstract methods
        public abstract string GetDbName(string strConnection);

        // Methods
        public string GetConnectionDetailsDriver(bool b64bit)
        {
            // Driver details
            string driver = string.Empty;
            switch (m_tech)
            {
                case DatabaseTechnology.eDB_DAO:
                    // DAO uses the database name only, so no action required
                    break;

                case DatabaseTechnology.eDB_ODBC:
                    driver = (b64bit)
                        ? DbDriver_ODBC_64bit
                        : DbDriver_ODBC_32bit;
                    break;

                case DatabaseTechnology.eDB_OleDb:
                    driver = (b64bit)
                        ? DbDriver_OleDb_64bit
                        : DbDriver_OleDb_32bit;
                    break;
            }
            
            return driver;
        }

        public string GetConnectionDetailsFilename(MSAccessDbType dbType)
        {
            // Physical database filename
            string source = string.Empty;
            switch (dbType)
            {
                case MSAccessDbType.eMSAccess97:
                    source = DbName_Northwind97;
                    break;

                case MSAccessDbType.eMSAccess2000:
                    source = DbName_Northwind2000;
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    source = DbName_Northwind2007_2016;
                    break;
            }

            // Override the database filename if using the HMQ database
            #if USE_HMQ_DATABASE
                source = DbName_HMQ;
            #endif

            return source;
        }

        public string GetConnectionStateAsString(IDbConnection connection)
        {
            // The System.Data.ConnectionState is marked with [Flags] (FlagsAttribute), so can be treated as a bitmask
            return string.Format("{0:G}", connection.State);
        }

        public string GetReadTechnologyAsString(DatabaseReadTechnology eReadTechnology)
        {
            // Return a header for the console which describes the performance test
            string header = string.Empty;
            if (eReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
                header += "IDataReader";
            else if (eReadTechnology == DatabaseReadTechnology.eRbRead_DataAdapter)
                header += "IDbDataAdapter";

            return header;
        }
    }
}
