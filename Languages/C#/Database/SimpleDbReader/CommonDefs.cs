namespace SimpleDbReader
{
    // Common definitions

    // Enumerations
    public enum DatabaseTechnology
    {
        // DB technology being tested
        eDB_Unknown,
        eDB_OleDB,          // System.Data.OleDb.OleDbConnection
        eDB_ODBC            // System.Data.Odbc.OdbcConnection
    };

    public enum QueryType
    {
        // Playing with various SQL queries
        eQueryStd,      // From the tutorial
        eQueryLike      // Demonstrate the "LIKE" operator of "WHERE"
    };
}
