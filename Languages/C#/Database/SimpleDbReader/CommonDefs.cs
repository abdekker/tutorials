namespace SimpleDbReader
{
    // Common definitions

    // Enumerations
    public enum DatabaseTechnology
    {
        // DB technology being tested (approximate chronological order when they were introduced)
        eDB_Unknown,
        eDB_DAO,        // DAO (add reference to Microsoft DAO 3.6 Object Library)
        eDB_ODBC,       // System.Data.Odbc.OdbcConnection
        eDB_OleDB       // System.Data.OleDb.OleDbConnection
    };

    public enum AccessDbType
    {
        // Access database formats
        eAccessUnknown,
        eAccess97,
        eAccess2000,
        eAccess2007_2016
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

        // Access database format
        public AccessDbType dbType;

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

    class CommonSimple
    {
        #region Constants
        // Constants unique the SimpleTest.mdb database
        public const string colMemberID = "MemberID";
        public const string colSurname = "Surname";
        public const string colFirstName = "FirstName";
        public const string colDOB = "DOB";
        public const string colFee = "Fee";
        public const string colAccepted = "Accepted";
        public const string colPoints = "Points";
        #endregion // Constants
    };
}
