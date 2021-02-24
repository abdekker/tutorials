using System.Data.Common;
#if UseOleDB
    using System.Data.OleDb;
#elif UseODBC
    using System.Data.Odbc;
#endif

namespace AccessLoginApp_MDB
{
    // Utilities class to assist with read/write access to the database
    public class DatabaseUtils
    {
        public DatabaseUtils() { }

        #region Public methods
        public DbConnection GetDbConnection()
        {
            // Return an OleDB or ODBC database connection object
#if UseOleDB
            // OleDbConnection : DbConnection, ICloneable, IDbConnection, IDisposable
            return new OleDbConnection();
#elif UseODBC
            // OdbcConnection : DbConnection, ICloneable
            return new OdbcConnection();
#endif
        }

        public string GetDbConnectionString()
        {
            // Return an OleDB or ODBC connection string for our test database

            // Connection string for Access 2007+ (OleDB):
            //      @"Provider=Microsoft.ACE.OLEDB.12.0;Data Source=C:\MyDatabase.accdb;Persist Security Info=False;";

            // Connection string for older versions of Access (OleDB):
            //      @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\MyDatabase.mdb;User Id=admin;Password=;";

            // Connection string for ODBC:
            //      @"Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\MyDatabase.mdb;Uid=Admin;Pwd=;";
#if UseOleDB
            return @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\AccessLogin.mdb;User Id=admin;Password=;";
#elif UseODBC
            return @"Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\Apps\Data\AccessLogin.mdb;Uid=Admin;Pwd=;";
#endif
        }

        public DbCommand GetDbCommand()
        {
            // Return an OleDB or ODBC database command object
#if UseOleDB
            // OleDbCommand : DbCommand, ICloneable, IDbCommand, IDisposable
            return new OleDbCommand();
#elif UseODBC
            // OdbcCommand : DbCommand, ICloneable
            return new OdbcCommand();
#endif
        }

        public DbDataAdapter GetDbDataAdapter(DbCommand command)
        {
            // Return an OleDB or ODBC data adapter object
#if UseOleDB
            // OleDbDataAdapter : DbDataAdapter, IDbDataAdapter, IDataAdapter, ICloneable
            return new OleDbDataAdapter((OleDbCommand)command);
#elif UseODBC
            // OdbcDataAdapter : DbDataAdapter, IDbDataAdapter, IDataAdapter, ICloneable
            return new OdbcDataAdapter((OdbcCommand)command);
#endif
        }
        #endregion // Public methods
    }
}
