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

    // DataSet or DataReader?
    // See https://stackoverflow.com/questions/1083193/
    // The analogy used in the top-rated answer there is between a bucket and a hosepipe.
    // * DataSet is like a bucket. This allows you to carry around a disconnected set of data. Heavyweight,
    //      but flexible.
    // * DataReader is like a hosepipe. Provides read-only access to data as it flies past, and needs to be
    //      connected at all times. Lightweight, but inflexible.

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

    #region Mapper base classes (for SQL Server, ODBC, ...)
    abstract class MapperReaderBase<T>
    {
        // IDataReader, IDataRecord (strongly typed)
        protected abstract T Map(IDataRecord record, UInt64 uRecordsToRead);
        public Collection<T> MapAll(IDataReader reader, UInt64 uRecordsToRead)
        {
            Collection<T> collection = new Collection<T>();
            while (reader.Read())
            {
                try
                {
                    collection.Add(Map(reader, uRecordsToRead));
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

    abstract class MapperAdapterBase<T>
    {
        // IDbDataAdapter, DataSet, DataRow (strongly typed)
        protected abstract T Map(DataRow record, UInt64 uRecordsToRead);
        public Collection<T> MapAll(DataSet ds, UInt64 uRecordsToRead)
        {
            Collection<T> collection = new Collection<T>();
            foreach (DataRow row in ds.Tables[0].Rows)
            {
                try
                {
                    collection.Add(Map(row, uRecordsToRead));
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
    #endregion // Mapper base classes (for SQL Server, ODBC, ...)

    #region Reader base classes (for SQL Server, ODBC, ...)
    abstract class ObjectReaderBase<T>
    {
        // IDataReader, IDataRecord (strongly typed)

        // Member variables
        protected DatabaseTechnology m_tech;
        protected string m_connectionString;
        protected string m_cmdText;
        protected CommandType m_cmdType;
        protected UInt64 m_uRecordsToRead;

        // Property accessors (for member variables)
        public abstract DatabaseTechnology DbTechnology { get; set; }
        public abstract string ConnectionString { get; set; }
        public abstract string CmdText { get; set; }
        public abstract CommandType CmdType { get; set; }
        public abstract UInt64 RecordsToRead { get; set; }

        // Constructor
        public ObjectReaderBase()
        {
            // Set defaults for the technology and connection strings (these should be updated by the user)
            DbTechnology = DatabaseTechnology.eDB_ODBC;
            ConnectionString = @"Driver={Microsoft Access Driver (*.mdb)};Dbq=YOUR_DATABASE_HERE;Uid=Admin;Pwd=;";
            CmdText = "SELECT * FROM SOME_TABLE";
            CmdType = CommandType.Text;
            RecordsToRead = 0;
        }

        // Abstract methods
        protected abstract Collection<IDataParameter> GetParameters(IDbCommand command);
        protected abstract MapperReaderBase<T> GetMapperReader();

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
                            MapperReaderBase<T> mapper = GetMapperReader();
                            collection = mapper.MapAll(reader, RecordsToRead);
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

        // Connection method
        public IDbConnection GetConnection()
        {
            // Examples for "m_connectionString"

            // ODBC (OdbcConnection)
            //    @"Driver={Microsoft Access Driver (*.mdb)};Dbq=PATH\\TO\\DATABASE;Uid=Admin;Pwd=;";

            // OleDB (OleDbConnection)
            //  @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=PATH\\TO\\DATABASE;User Id=admin;Password=;"

            // SQL Server (SqlConnection)
            //    @"Data Source=PATH\\TO\\DATABASE;Initial Catalog=Test;Integrated Security=True";
            IDbConnection connection = null;
            if (DbTechnology == DatabaseTechnology.eDB_ODBC)
                connection = new OdbcConnection(ConnectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_OleDb)
                connection = new OleDbConnection(ConnectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_SqlServer)
                connection = new SqlConnection(ConnectionString);

            return connection;
        }
    }

    abstract class ObjectAdapterBase<T>
    {
        // IDbDataAdapter, DataSet, DataRow (strongly typed)

        // Member variables
        protected DatabaseTechnology m_tech;
        protected string m_connectionString;
        protected string m_cmdText;
        protected CommandType m_cmdType;
        protected UInt64 m_uRecordsToRead;

        // Property accessors (for member variables)
        public abstract DatabaseTechnology DbTechnology { get; set; }
        public abstract string ConnectionString { get; set; }
        public abstract string CmdText { get; set; }
        public abstract CommandType CmdType { get; set; }
        public abstract UInt64 RecordsToRead { get; set; }

        // Constructor
        public ObjectAdapterBase()
        {
            // Set defaults for the technology and connection strings (these should be updated by the user)
            DbTechnology = DatabaseTechnology.eDB_ODBC;
            ConnectionString = @"Driver={Microsoft Access Driver (*.mdb)};Dbq=YOUR_DATABASE_HERE;Uid=Admin;Pwd=;";
            CmdText = "SELECT * FROM SOME_TABLE";
            CmdType = CommandType.Text;
            RecordsToRead = 0;
        }

        // Abstract methods
        protected abstract Collection<IDataParameter> GetParameters(IDbCommand command);
        protected abstract MapperAdapterBase<T> GetMapperAdapter();

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

                IDbDataAdapter adapter = GetAdapter();
                adapter.SelectCommand = command;

                try
                {
                    connection.Open();
                    DataSet ds = new DataSet();
                    adapter.Fill(ds);
                    try
                    {
                        MapperAdapterBase<T> mapper = GetMapperAdapter();
                        collection = mapper.MapAll(ds, RecordsToRead);
                    }
                    catch
                    {
                        //throw;
                    }
                    finally
                    {
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

        public IDbConnection GetConnection()
        {
            // See ObjectReaderBase::GetConnection for examples for "m_connectionString"
            IDbConnection connection = null;
            if (DbTechnology == DatabaseTechnology.eDB_ODBC)
                connection = new OdbcConnection(ConnectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_OleDb)
                connection = new OleDbConnection(ConnectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_SqlServer)
                connection = new SqlConnection(ConnectionString);

            return connection;
        }

        public IDbDataAdapter GetAdapter()
        {
            IDbDataAdapter adapter = null;
            if (DbTechnology == DatabaseTechnology.eDB_ODBC)
                adapter = new OdbcDataAdapter();
            else if (DbTechnology == DatabaseTechnology.eDB_OleDb)
                adapter = new OleDbDataAdapter();
            else if (DbTechnology == DatabaseTechnology.eDB_SqlServer)
                adapter = new SqlDataAdapter();

            return adapter;
        }
    }

    public class ObjectDataSetRaw
    {
        // IDbDataAdapter, DataSet (raw, caller uses DataRow to manipulate data)
        // Note: This version is agnostic to the data returned and the caller should manipulate the data manually

        // Property accessors (for member variables)
        public DatabaseTechnology DbTechnology { get; set; }
        public string ConnectionString { get; set; }
        public string CmdText { get; set; }
        public CommandType CmdType { get; set; }

        // Constructor
        public ObjectDataSetRaw()
        {
            // Set defaults for the technology and connection strings (these should be updated by the user)
            DbTechnology = DatabaseTechnology.eDB_ODBC;
            ConnectionString = @"Driver={Microsoft Access Driver (*.mdb)};Dbq=YOUR_DATABASE_HERE;Uid=Admin;Pwd=;";
            CmdText = "SELECT * FROM SOME_TABLE";
            CmdType = CommandType.Text;
        }

        protected Collection<IDataParameter> GetParameters(IDbCommand command)
        {
            Collection<IDataParameter> collection = new Collection<IDataParameter>();
            return collection;

            // If you have parameters:
            //IDataParameter param1 = command.CreateParameter();
            //param1.ParameterName = "paramName 1";     // Put the parameter name here
            //param1.Value = 5;                         // Put the parameter value here
            //collection.Add(param1);
            //return collection;   
        }

        // Execute method
        public DataSet Execute()
        {
            DataSet ds = new DataSet();
            using (IDbConnection connection = GetConnection())
            {
                IDbCommand command = connection.CreateCommand();
                command.Connection = connection;
                command.CommandText = CmdText;
                command.CommandType = CmdType;
                foreach (IDataParameter param in GetParameters(command))
                    command.Parameters.Add(param);

                IDbDataAdapter adapter = GetAdapter();
                adapter.SelectCommand = command;
                try
                {
                    connection.Open();
                    adapter.Fill(ds);
                }
                catch
                {
                    // Throw?
                }
                finally
                {
                    connection.Close();
                }
            }

            return ds;
        }

        protected IDbConnection GetConnection()
        {
            // See ObjectReaderBase::GetConnection for examples for "m_connectionString"
            IDbConnection connection = null;
            if (DbTechnology == DatabaseTechnology.eDB_ODBC)
                connection = new OdbcConnection(ConnectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_OleDb)
                connection = new OleDbConnection(ConnectionString);
            else if (DbTechnology == DatabaseTechnology.eDB_SqlServer)
                connection = new SqlConnection(ConnectionString);

            return connection;
        }

        protected IDbDataAdapter GetAdapter()
        {
            IDbDataAdapter adapter = null;
            if (DbTechnology == DatabaseTechnology.eDB_ODBC)
                adapter = new OdbcDataAdapter();
            else if (DbTechnology == DatabaseTechnology.eDB_OleDb)
                adapter = new OleDbDataAdapter();
            else if (DbTechnology == DatabaseTechnology.eDB_SqlServer)
                adapter = new SqlDataAdapter();

            return adapter;
        }
    }
    #endregion // Reader base classes (for SQL Server, ODBC, ...)
}
