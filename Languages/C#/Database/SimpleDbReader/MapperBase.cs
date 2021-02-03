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
                catch (Exception ex)
                {
                    //throw;
                    // Consider handling exception (instead of re-throwing) if graceful recovery is possible
                    Console.WriteLine(UtilitiesGeneral.FormatException(
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
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
                catch (Exception ex)
                {
                    //throw;
                    // Consider handling exception (instead of re-throwing) if graceful recovery is possible
                    Console.WriteLine(UtilitiesGeneral.FormatException(
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
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

        // Property accessors
        // Note: In general, automatic properties can be used in the derived class. A body only needs to be provided
        // if special processing is required.
        public abstract DatabaseTechnology DbTechnology { get; set; }
        public abstract string ConnectionString { get; set; }

        // Example SQL queries:
        //      SELECT * FROM Members;
        //      SELECT MemberID,Surname FROM Members;
        //      SELECT ProductID, UnitPrice, ProductName FROM Products
        //          WHERE UnitPrice > ?
        //          ORDER BY UnitPrice DESC;

        public abstract string CmdText { get; set; }

        // Available command types are:
        // * Text (standard SQL query) = 1,
        // * StoredProcedure
        // * TableDirect (direct table access) [appears to be a shortcut to "SELECT * FROM TABLENAME"]
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
                        catch (Exception ex)
                        {
                            //throw;
                            Console.WriteLine(string.Format("{0}::{1} 1: {2}",
                                this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
                        }
                        finally
                        {
                            reader.Close();
                        }
                    }
                }
                catch (Exception ex)
                {
                    //throw;
                    Console.WriteLine(string.Format("{0}::{1} 2: {2}",
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
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
            // Notes:
            // * The examples below are for Access 97, 2000, 2002 and 2003 (.mdb files); generally just change
            // * Generally only two changes are required for Access 2007+ (.accdb) files:
            //      - For ODBC, change the "Driver" to "{Microsoft Access Driver (*.mdb, *.accdb)}"
            //      - Change the database name to "myDB.accdb"

            // Examples connection strings (see https://www.connectionstrings.com/ for more examples)

            // ODBC (OdbcConnection)
            //  32-bit, std security:           Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\myFolder\myDB.mdb;Uid=Admin;Pwd=;
            //  32-bit, exclusive access:       Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\myFolder\myDB.mdb;Exclusive=1;Uid=Admin;Pwd=;
            //  32-bit, admin statements:       Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\myFolder\myDB.mdb;Uid=Admin;Pwd=;ExtendedAnsiSQL=1;
            //  32-bit, workgroup (system DB):  Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\myFolder\myDB.mdb;SystemDB=C:\sysFolder\sysDB.mdw;

            //  64-bit, std security:           Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=C:\myFolder\myDB.accdb;
            //  (etc, change driver and database name)

            // OleDB (OleDbConnection)
            //  32-bit, std security:           Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\myFolder\myDB.mdb;User Id=admin;Password=;
            //  32-bit, exclusive access:       Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\myFolder\myDB.mdb;Mode=Share Exclusive;User Id=admin;Password=;
            //  32-bit, with password:          Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\myFolder\myDB.mdb;Jet OLEDB:Database Password=myPassword;
            //  32-bit, workgroup (system DB):  Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\myFolder\myDB.mdb;Jet OLEDB:System Database=C:\sysFolder\sysDB.mdw;
            //  32-bit, workgroup with user:    Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\myFolder\myDB.mdb;Jet OLEDB:System Database=C:\sysFolder\sysDB.mdw;User ID=myUser;Password=myPassword;
            //  32-bit, network location:       Provider=Microsoft.Jet.OLEDB.4.0;Data Source=\\serverName\shareName\myFolder\myDB.mdb;User Id=admin;Password=;
            //  32-bit, DataDirectory:          Provider=Microsoft.Jet.OLEDB.4.0;Data Source=|DataDirectory|\myDB.mdb;User Id=admin;Password=;

            //  64-bit, std security:           Provider=Microsoft.ACE.OLEDB.12.0;Data Source=C:\myFolder\myDB.mdb;Persist Security Info=False;
            //  64-bit, with password:          Provider=Microsoft.ACE.OLEDB.12.0;Data Source=C:\myFolder\myDB.mdb;Jet OLEDB:Database Password=myPassword;
            //  64-bit, DataDirectory:          Provider=Microsoft.ACE.OLEDB.12.0;Data Source=|DataDirectory|\myDB.mdb;Persist Security Info=False;
            //  64-bit, network location:       Provider=Microsoft.ACE.OLEDB.12.0;Data Source=\\serverName\shareName\myFolder\myDB.mdb;

            // SQL Server (SqlConnection)
            //  Local database:                 Data Source=C:\myFolder\myDB.mdf;Initial Catalog=Test;Integrated Security=True;
            //  SqlClient, std security:        Server=myServerAddress;Database=myDataBase.mdf;User Id=myUser;Password=myPassword;
            //  OLE DB driver for SQL Server:   Provider=MSOLEDBSQL;Server=myServerAddress;Database=myDataBase;UID=myUsername;PWD=myPassword;
            //  Connect via IP address:         Data Source=190.190.200.100,1433;Network Library=DBMSSOCN;Initial Catalog=myDataBase.mdf;User ID=myUser;Password=myPassword;
            // SQL Server 2019, std security:   Driver={ODBC Driver 17 for SQL Server};Server=myServerAddress;Database=myDataBase.mdf;UID=myUser;PWD=myPassword;

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
        /*protected DatabaseTechnology m_tech;
        protected string m_connectionString;
        protected string m_cmdText;
        protected CommandType m_cmdType;
        protected UInt64 m_uRecordsToRead;*/

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
                    catch (Exception ex)
                    {
                        //throw;
                        Console.WriteLine(string.Format("{0}::{1} 1: {2}",
                            this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
                    }
                    finally
                    {
                    }
                }
                catch (Exception ex)
                {
                    //throw;
                    Console.WriteLine(string.Format("{0}::{1} 2: {2}",
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
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
                catch (Exception ex)
                {
                    // throw?
                    Console.WriteLine(UtilitiesGeneral.FormatException(
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
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
