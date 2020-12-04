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
    }
}
