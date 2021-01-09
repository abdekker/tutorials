using System.Collections.ObjectModel;
using System.Data;
using System.Data.Odbc;
using System.Data.SqlClient;

namespace SimpleDbReader
{
    // Template mapper and object reader base are adapted from:
    // https://www.c-sharpcorner.com/article/an-elegant-C-Sharp-data-access-layer-using-the-template-pattern-a/
    #region Base classes for SQL Server
    abstract class MapperBaseSqlServer<T>
    {
        // This mapper is based on SQL Server
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
                    // NOTE: Consider handling exception here instead of re-throwing if graceful
                    // recovery can be accomplished  
                }
            }

            return collection;
        }
    }

    abstract class ObjectReaderBaseSqlServer<T>
    {
        protected abstract IDbConnection GetConnection();
        protected abstract string CommandText { get; }
        protected abstract CommandType CommandType { get; }
        protected abstract Collection<IDataParameter> GetParameters(IDbCommand command);
        protected abstract MapperBaseSqlServer<T> GetMapper();

        public Collection<T> Execute()
        {
            Collection<T> collection = new Collection<T>();
            using (IDbConnection connection = GetConnection())
            {
                IDbCommand command = connection.CreateCommand();
                command.Connection = connection;
                command.CommandText = this.CommandText;
                command.CommandType = this.CommandType;

                foreach (IDataParameter param in this.GetParameters(command))
                    command.Parameters.Add(param);

                try
                {
                    connection.Open();
                    using (IDataReader reader = command.ExecuteReader())
                    {
                        try
                        {
                            MapperBaseSqlServer<T> mapper = GetMapper();
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

    abstract class ObjectReaderWithConnectionSqlServer<T> : ObjectReaderBaseSqlServer<T>
    {
        private static string m_connectionString =
            @"Data Source=C:\\Apps\\Data\\SimpleTest.mdb;Initial Catalog=Test;Integrated Security=True";
        protected override IDbConnection GetConnection()
        {
            // update to get your connection here
            IDbConnection connection = new SqlConnection(m_connectionString);
            return connection;
        }
    }
    #endregion // Base classes for SQL Server

    #region Base classes for ODBC
    abstract class MapperBaseODBC<T>
    {
        // This mapper is based on ODBC
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
                    // NOTE: Consider handling exception here instead of re-throwing if graceful
                    // recovery can be accomplished  
                }
            }

            return collection;
        }
    }

    abstract class ObjectReaderBaseODBC<T>
    {
        protected abstract IDbConnection GetConnection();
        protected abstract string CmdText { get; }
        protected abstract CommandType CmdType { get; }
        protected abstract Collection<IDataParameter> GetParameters(IDbCommand command);
        protected abstract MapperBaseODBC<T> GetMapper();

        public Collection<T> Execute()
        {
            Collection<T> collection = new Collection<T>();
            using (IDbConnection connection = GetConnection())
            {
                IDbCommand command = connection.CreateCommand();
                command.Connection = connection;
                command.CommandText = this.CmdText;
                command.CommandType = this.CmdType;

                //OdbcCommand command2 = new OdbcCommand(this.CommandText, (OdbcConnection)connection);

                foreach (IDataParameter param in this.GetParameters(command))
                    command.Parameters.Add(param);

                try
                {
                    connection.Open();
                    using (IDataReader reader = command.ExecuteReader())
                    {
                        try
                        {
                            MapperBaseODBC<T> mapper = GetMapper();
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
                    throw;
                }
                finally
                {
                    connection.Close();
                }
            }

            return collection;
        }
    }

    abstract class ObjectReaderWithConnectionODBC<T> : ObjectReaderBaseODBC<T>
    {
        private static string m_connectionString =
            @"Driver ={Microsoft Access Driver(*.mdb)}; Dbq=C:\Apps\Data\SimpleTest.mdb;Uid=Admin;Pwd=;";
        protected override IDbConnection GetConnection()
        {
            // Update to get your connection here
            IDbConnection connection = new OdbcConnection(m_connectionString);
            return connection;
        }
    }
    #endregion // Base classes for SQL Server
}
