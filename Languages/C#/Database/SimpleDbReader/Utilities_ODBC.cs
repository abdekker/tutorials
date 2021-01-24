using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;

using System.Data.Odbc;

namespace SimpleDbReader
{
    class Utilities_ODBC : UtilitiesBase
    {
        // Utilities for using ODBC

        #region Constants
        // Tables schema
        public readonly string Schema_Tables_ODBC_SystemTable = "SYSTEM TABLE";

        // Columns schema
        public readonly string Schema_Columns_ODBC_Name = "COLUMN_NAME";
        public readonly string Schema_Columns_ODBC_Type = "DATA_TYPE";
        public readonly string Schema_Columns_ODBC_TypeName = "TYPE_NAME";
        public readonly string Schema_Columns_ODBC_Size = "COLUMN_SIZE";
        public readonly string Schema_Columns_ODBC_Nullable = "IS_NULLABLE";
        #endregion // Constants

        public Utilities_ODBC()
        {
            // This utility class uses ODBC
            DbTechnology = DatabaseTechnology.eDB_ODBC;
        }

        #region Properties and methods from UtilitiesBase
        public override DatabaseTechnology DbTechnology
        {
            get { return m_tech; }
            set { m_tech = value; }
        }

        public override string GetDbName(string strConnection)
        {
            // Get the name of the database associated with the connection string
            string dbName = string.Empty;
            using (OdbcConnection connection = new OdbcConnection(strConnection))
            {
                connection.Open();
                dbName = connection.Database;
                if (connection.DataSource.Equals("ACCESS"))
                    dbName += ".mdb";
            }

            return dbName;
        }
        #endregion // Properties and methods from UtilitiesBase
    }
}
