using System;
using System.Collections.ObjectModel;
using System.Data;
using System.Data.Odbc;
using System.Data.OleDb;
using System.Data.SqlClient;

namespace SimpleDbReader
{
    // Template mapper and object readers are adapted from:
    // https://www.c-sharpcorner.com/article/an-elegant-C-Sharp-data-access-layer-using-the-template-pattern-a/

    #region Base class for records
    interface IRecordBase
    {
        // Base class for all database records

        // Helper methods (which might be used for printing to the console)
        void DefaultRecord();
        string GetRecordHeader();
        string GetRecordAsString();
    }
    # endregion // Base class for records

    #region Mapper base class (for SQL Server, ODBC, ...)
    abstract class MapperBase<T>
    {
        // This mapper can be used with SQL Server, ODBC, etc.
        protected abstract T Map(IDataRecord record);
        public Collection<T> MapAll(IDataReader reader)
        {
            Collection<T> collection = new Collection<T>();
            while (reader.Read())
            {
                try
                {
                    collection.Add(Map(reader));
                }
                catch
                {
                    //throw;
                    // Consider handling exception (instead of re-throwing) if graceful recovery is possible
                }
            }

            return collection;
        }
    }
    #endregion // Mapper base class (for SQL Server, ODBC, ...)

    #region Reader base class (for SQL Server, ODBC, ...)
    abstract class ObjectReaderBase<T>
    {
        // Member variables
        protected DatabaseTechnology m_tech;
        protected string m_connectionString;
        protected string m_cmdText;
        protected CommandType m_cmdType;

        // Property accessors (for member variables)
        public abstract DatabaseTechnology DbTechnology { get; set; }
        public abstract string ConnectionString { get; set; }
        public abstract string CmdText { get; set; }
        public abstract CommandType CmdType { get; set; }

        // Constructor
        public ObjectReaderBase()
        {
            // Set defaults for the technology and connection strings (these should be updated by the user)
            DbTechnology = DatabaseTechnology.eDB_ODBC;
            ConnectionString = @"Driver={Microsoft Access Driver (*.mdb)};Dbq=YOUR_DATABASE_HERE;Uid=Admin;Pwd=;";
            CmdText = "SELECT * FROM SOME_TABLE";
            CmdType = CommandType.Text;
        }

        // Methods
        protected abstract IDbConnection GetConnection();
        protected abstract Collection<IDataParameter> GetParameters(IDbCommand command);
        protected abstract MapperBase<T> GetMapper();

        // Execute method
        public Collection<T> Execute()
        {
            Collection<T> collection = new Collection<T>();
            using (IDbConnection connection = GetConnection())
            {
                IDbCommand command = connection.CreateCommand();
                command.Connection = connection;
                command.CommandText = this.CmdText;
                command.CommandType = this.CmdType;

                foreach (IDataParameter param in this.GetParameters(command))
                    command.Parameters.Add(param);

                try
                {
                    connection.Open();
                    using (IDataReader reader = command.ExecuteReader())
                    {
                        try
                        {
                            MapperBase<T> mapper = GetMapper();
                            collection = mapper.MapAll(reader);
                        }
                        catch
                        {
                            //throw;
                        }
                        finally
                        {
                            reader.Close();
                        }
                    }
                }
                catch
                {
                    //throw;
                }
                finally
                {
                    connection.Close();
                }
            }

            return collection;
        }
    }

    abstract class ObjectReaderWithConnection<T> : ObjectReaderBase<T>
    {
        // SQL Server
        //private static string m_connectionString =
        //    @"Data Source=C:\\Apps\\Data\\SimpleTest.mdb;Initial Catalog=Test;Integrated Security=True";

        // ODBC
        //private static string m_connectionString =
        //    @"Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\Apps\Data\SimpleTest.mdb;Uid=Admin;Pwd=;";
        protected override IDbConnection GetConnection()
        {
            // Update to get your connection here
            IDbConnection connection = null;
            if (DbTechnology == DatabaseTechnology.eDB_ODBC)
                connection = new OdbcConnection(m_connectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_OleDB)
                connection = new OleDbConnection(m_connectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_SqlServer)
                connection = new SqlConnection(m_connectionString);

            return connection;
        }
    }
    #endregion // Reader base class (for SQL Server, ODBC, ...)
}
