namespace SimpleDbReader
{
    // Common definitions

    // Enumerations
    public enum DatabaseTechnology
    {
        // DB technology being tested (approximate chronological order when they were introduced)
        // Note: Modern database technologies (ODBC, OleDB, SqlServer, ...) derive from DbConnection
        eDB_Unknown,
        eDB_DAO,                // DAO (add reference to Microsoft DAO 3.6 Object Library)
        eDB_ODBC,               // System.Data.Odbc.OdbcConnection : DbConnection
        eDB_OleDB,              // System.Data.OleDb.OleDbConnection : DbConnection
        eDB_SqlServer           // System.Data.SqlClient.SqlConnection : DbConnection
    };

    // Enumerations
    public enum DatabaseReadTechnology
    {
        // The OleDB technology used for reading data
        eRbRead_DataReader,     // System.Data.OleDb.OleDbDataReader
        eRbRead_DataAdapter     // System.Data.OleDb.OleDbDataAdapter
    };

    public enum DatabaseAccess
    {
        // How the database is read (raw or through a templated mapper)
        eDbAccess_Raw,
        eDbAccess_Template
    };

    public enum MSAccessDbType
    {
        // Microsoft Access database formats
        eMSAccessUnknown,
        eMSAccess97,              // .mdb
        eMSAccess2000,            // .mdb
        eMSAccess2007_2016        // .mdb and .accdb
    }

    public enum QueryType
    {
        // Playing with various SQL queries
        eQueryStd1,     // From the Microsoft tutorial (Northwind database)
        eQueryStd2,     // From the w3schools tutorial (SimpelTest database)
        eQueryLike      // Demonstrate the "LIKE" operator of "WHERE"
    };

    // Structures
    public struct ConfigGeneral
    {
        // General configuration information

        // 64-bit?
        public bool b64bit;

        // Location of databases
        public string strDevDataPath;
    }

    public struct ConfigDatabase
    {
        // Configuration information about the database being accessed

        // Microsoft Access database format
        public MSAccessDbType dbType;

        // Main query
        public QueryType queryType;
        public string strQuery;

        // Parameter for the standard query on the Northwind DB. Records returned:
        //  Parameter    Records returned
        //    5                 75
        //    25                28
        //    40                12 
        public int paramValue;
    }
}
