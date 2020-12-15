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

    public enum QueryType
    {
        // Playing with various SQL queries
        eQueryStd,      // From the tutorial
        eQueryLike      // Demonstrate the "LIKE" operator of "WHERE"
    };
}
