using System;
using System.Collections.Generic;
using System.Data.OleDb;

namespace SimpleDbReader
{
    class Utilities_OleDb : UtilitiesBase
    {
        // Utilities for using OleDB

        #region Member variables
        Dictionary<int, string> m_OleDbSchemaSizeColForType = new Dictionary<int, string>();
        #endregion // Member variables

        #region Constants
        // Tables schema
        public readonly string Schema_Tables_OleDB_SystemTable = "ACCESS TABLE";
        public readonly string Schema_Tables_OleDB_ViewTable = "VIEW";

        // Columns schema
        public readonly string Schema_Columns_OleDB_Name = "COLUMN_NAME";
        public readonly string Schema_Columns_OleDB_Type = "DATA_TYPE";
        public readonly string Schema_Columns_OleDB_Size_Number = "NUMERIC_PRECISION";
        public readonly string Schema_Columns_OleDB_Size_String = "CHARACTER_MAXIMUM_LENGTH";
        public readonly string Schema_Columns_OleDB_Size_Date = "DATETIME_PRECISION";
        public readonly string Schema_Columns_OleDB_Nullable = "IS_NULLABLE";
        #endregion // Constants

        public Utilities_OleDb()
        {
            // This utility class uses OleDb
            DbTechnology = DatabaseTechnology.eDB_OleDb;

            // Initialise a dictionary to convert between System.Data.OleDb.OleDbType and the column name in the schema
            // which contains the size for that data type
            InitialiseOleDbTypeSchemaSizeColumn();
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
            using (OleDbConnection connection = new OleDbConnection(strConnection))
            {
                connection.Open();
                dbName = connection.DataSource;
            }

            return dbName;
        }
        #endregion // Properties and methods from UtilitiesBase

        #region Public methods
        public string GetOleDBTypeSchemaSizeColumn(int type)
        {
            // Convert the System.Data.OleDb.OleDbType (represented as an integer) to a string (representing the
            // column name in the schema containing the size of the data type in the database)
            string column = string.Empty;
            if (m_OleDbSchemaSizeColForType.ContainsKey(type))
                column = m_OleDbSchemaSizeColForType[type];

            return column;
        }
        #endregion // Public methods

        #region Private methods
        private void InitialiseOleDbTypeSchemaSizeColumn()
        {
            // Convert the OleDB type to the schema column name required to read the size of that type
            // Note: With only a limited set of sample databases, most of these are guessed!
            string column;
            foreach (OleDbType dbType in Enum.GetValues(typeof(OleDbType)))
            {
                column = Schema_Columns_OleDB_Size_Number;
                switch (dbType)
                {
                    case OleDbType.Empty:
                    case OleDbType.SmallInt:
                    case OleDbType.Integer:
                    case OleDbType.Single:
                    case OleDbType.Double:
                    case OleDbType.Currency:
                    case OleDbType.IDispatch:
                    case OleDbType.Error:
                    case OleDbType.Variant:
                    case OleDbType.IUnknown:
                    case OleDbType.Decimal:
                    case OleDbType.TinyInt:
                    case OleDbType.UnsignedTinyInt:
                    case OleDbType.UnsignedSmallInt:
                    case OleDbType.UnsignedInt:
                    case OleDbType.UnsignedBigInt:
                    case OleDbType.Guid:
                    case OleDbType.Numeric:
                    case OleDbType.PropVariant:
                    case OleDbType.VarNumeric:
                    case OleDbType.VarBinary:
                    case OleDbType.LongVarBinary:
                        column = Schema_Columns_OleDB_Size_Number;
                        break;

                    case OleDbType.Date:
                    case OleDbType.Filetime:
                    case OleDbType.DBDate:
                    case OleDbType.DBTime:
                    case OleDbType.DBTimeStamp:
                        column = Schema_Columns_OleDB_Size_Date;
                        break;

                    case OleDbType.BSTR:
                        case OleDbType.Boolean:
                    case OleDbType.Binary:
                    case OleDbType.Char:
                    case OleDbType.WChar:
                    case OleDbType.VarChar:
                    case OleDbType.LongVarChar:
                    case OleDbType.VarWChar:
                    case OleDbType.LongVarWChar:
                        column = Schema_Columns_OleDB_Size_String;
                        break;
                }

                // Add type and size column name to the dictionary
                m_OleDbSchemaSizeColForType.Add((int)dbType, column);
            }
        }
        #endregion // Private methods
    }
}
