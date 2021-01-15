using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;

namespace SimpleDbReader
{
    class Utilities_DbConnection
    {
        // Utilities for using:
        // * System.Data.DbConnection (such as System.Data.Odbc.OdbcConnection)
        // * System.Data.DataTable (such as returned from DbConnection::GetSchema)
        #region Member variables
        private string m_fieldHeader;

        struct SchemaFieldDefinition
        {
            // Subset of schema information available for fields (columns)
            public string name;
            public string type;
            public int size;
            public string nullable;
        };
        #endregion // Member variables

        #region Constants
        private readonly string Schema_Tables = "Tables";
        private readonly string Schema_Columns = "Columns";

        private readonly string Schema_Tables_Column_TableName = "TABLE_NAME";
        private readonly string Schema_Tables_Column_TableType = "TABLE_TYPE";

        private readonly string Schema_Tables_ODBC_SystemTable = "SYSTEM TABLE";

        private readonly string Schema_Tables_OleDB_SystemTable = "ACCESS TABLE";
        private readonly string Schema_Tables_OleDB_ViewTable = "VIEW";

        private readonly string Schema_Columns_ODBC_Name = "COLUMN_NAME";
        private readonly string Schema_Columns_ODBC_Type = "TYPE_NAME";
        private readonly string Schema_Columns_ODBC_Size = "COLUMN_SIZE";
        private readonly string Schema_Columns_ODBC_Nullable = "IS_NULLABLE";

        private readonly string Schema_Columns_OleDB_Name = "COLUMN_NAME";
        private readonly string Schema_Columns_OleDB_Type = "DATA_TYPE";
        private readonly string Schema_Columns_OleDB_Size_Number = "NUMERIC_PRECISION";
        private readonly string Schema_Columns_OleDB_Size_String = "CHARACTER_MAXIMUM_LENGTH";
        private readonly string Schema_Columns_OleDB_Size_Date = "DATETIME_PRECISION";
        private readonly string Schema_Columns_OleDB_Nullable = "IS_NULLABLE";

        private readonly string Schema_Header_Column_Formatting = "{0,-20}{1,-15}{2,-13}{3}";
        private readonly string Schema_Header_Column_Name = "Name";
        private readonly string Schema_Header_Column_Type = "Type";
        private readonly string Schema_Header_Column_Size = "Size";
        private readonly string Schema_Header_Column_Nullable = "Nullable";
        #endregion // Constants

        public Utilities_DbConnection()
        {
            // Header when displaying field information
            m_fieldHeader = string.Format(Schema_Header_Column_Formatting,
                Schema_Header_Column_Name,
                Schema_Header_Column_Type,
                Schema_Header_Column_Size,
                Schema_Header_Column_Nullable);
        }

        #region Public methods
        public List<string> GetSchemaInfo(DbConnection connection, string collection, bool removeSysTables = false)
        {
            // Return a list of the tables in the supplied database schema
            List<string> schemaInfo = new List<string>();
            if (collection.Equals(Schema_Tables))
            {
                GetSchemaTables(ref schemaInfo, connection, removeSysTables);
                //GetSchemaTablesFull(ref schemaInfo, connection); // Uncomment to get full schema information
            }

            return schemaInfo;
        }

        public List<string> GetFields(DbConnection connection, string table)
        {
            // Return a list of the columns in the supplied table schema
            List<string> columns = new List<string>();
            GetSchemaColumns(ref columns, connection, table);
            //GetSchemaColumnsFull(ref columns, connection, table); // Uncomment to get full schema information
            return columns;
        }
        #endregion // Public methods

        #region Private methods
        private void GetSchemaTables(ref List<string> tables, DbConnection connection, bool stdTablesOnly = false)
        {
            // Retrieve "Tables" schema information, which differs between connection types. To see all columns, use "GetSchemaTablesFull".
            
            #region Schema details
            // ### OdbcConnection ###
            // * TABLE_CAT          Name of database (eg. C:\Apps\Data\Northwind 2000)
            // * TABLE_SCHEM        ?
            // * TABLE_NAME         Name of table (eg. Customers)
            // * TABLE_TYPE         Type of table ("SYSTEM TABLE" or "TABLE") [the ODBC "Tables" schema does not include Views]
            // * REMARKS            Description of table
            // Note: Schema information may differ between ODBC drivers

            // ### OleDbConnection ###
            // * TABLE_CATALOG      ?
            // * TABLE_SCHEMA       ?
            // * TABLE_NAME         Name of table (eg. Customers)
            // * TABLE_TYPE         Type of table or view ("ACCESS TABLE", "TABLE" or "VIEW")
            // * TABLE_GUID         ?
            // * DESCRIPTION        Description of table
            // * TABLE_PROPID       ?
            // * DATE_CREATED       Date created (eg. 13/09/1995 10:51:45)
            // * DATE_MODIFIED      Date modified (eg. 12/03/2003 05:09:58)
            #endregion // Schema details

            DataTable schemaTables = connection.GetSchema(Schema_Tables);
            bool addTable = true;
            foreach (DataRow row in schemaTables.Rows)
            {
                if (stdTablesOnly)
                {
                    try
                    {
                        if (connection is System.Data.Odbc.OdbcConnection)
                        {
                            // ODBC
                            addTable = (!row[Schema_Tables_Column_TableType].Equals(Schema_Tables_ODBC_SystemTable));
                        }
                        else if (connection is System.Data.OleDb.OleDbConnection)
                        {
                            // OleDB
                            addTable = (
                                (!row[Schema_Tables_Column_TableType].Equals(Schema_Tables_OleDB_SystemTable)) &&
                                (!row[Schema_Tables_Column_TableType].Equals(Schema_Tables_OleDB_ViewTable)));

                        }
                    }
                    catch { }
                }

                if (addTable)
                {
                    foreach (DataColumn col in schemaTables.Columns)
                    {
                        // Add the tables names. These are vheld in the "TABLE_NAME" column.
                        if (col.ColumnName.Equals(Schema_Tables_Column_TableName))
                        {
                            tables.Add((string)row[col]);
                        }
                    }
                }
            }
        }
        
        private void GetSchemaTablesFull(ref List<string> tables, DbConnection connection)
        {
            // Retrieve full "Tables" schema information
            DataTable schemaTables = connection.GetSchema(Schema_Tables);
            foreach (DataRow row in schemaTables.Rows)
            {
                foreach (DataColumn col in schemaTables.Columns)
                {
                    tables.Add(string.Format("{0} = {1}", col.ColumnName, row[col]));
                }
            }
        }

        private void GetSchemaColumns(ref List<string> columns, DbConnection connection, string table)
        {
            // Retrieve "Columns" schema information, which differs between connection types. To see all columns, use "GetSchemaColumnsFull".

            #region Schema details
            // ### OdbcConnection ###
            // * TABLE_CAT          Name of database (eg. C:\Apps\Data\Northwind 97)
            // * TABLE_SCHEM        ?
            // * TABLE_NAME         Name of table to which this column belongs (eg. Products)
            // * COLUMN_NAME        Field name (eg. ProductID)
            // * DATA_TYPE          Numerical code for the data in the column (eg. 4 representing COUNTER or INTEGER)
            // * TYPE_NAME          Data type as a string (eg. COUNTER)
            // * COLUMN_SIZE        Set by the data type, except VARCHAR and NVARCHAR (eg. 1 for BIT, 10 for INTEGER, etc)
            // * BUFFER_LENGTH      As above (eg. 1 for BIT, 2 for SMALLINT, 4 for INTEGER, 21 for CURRENCY, etc)
            // * DECIMAL_DIGITS     Null for string types and 0 for integral types
            // * NUM_PREC_RADIX     ? (Appears to be the base for numerical fields)
            // * NULLABLE           Does this field allow null values? 0 for the key column.
            // * REMARKS            Description of field
            // * COLUMN_DEF         ?
            // * SQL_DATA_TYPE      (same as DATA_TYPE)
            // * SQL_DATETIME_SUB   Appears to be 3 for fields of type "DATETIME", otherwise null
            // * CHAR_OCTET_LENGTH  Appears to be the same as BUFFER_LENGTH for non-integral types
            // * ORDINAL_POSITION   Column number (eg. 1)
            // * IS_NULLABLE        Does this field allow null values as string? NO for the key column.
            // * ORDINAL            Column number (eg. 1)
            // Note: Schema information may differ between ODBC drivers

            // ### OleDbConnection ###
            // * TABLE_CATALOG              ?
            // * TABLE_SCHEMA               ?
            // * TABLE_NAME                 Name of table to which this column belongs (eg. Products)
            // * COLUMN_NAME                Field name (eg. HireDate)
            // * COLUMN_GUID                ?
            // * COLUMN_PROPID              ?
            // * ORDINAL_POSITION           Column number (eg. 1)
            // * COLUMN_HASDEFAULT          Mostly null. If available, then COLUMN_DEFAULT will have a value.
            // * COLUMN_DEFAULT             Mostly null. If available, the default value for the field.
            // * COLUMN_FLAGS               ?
            // * IS_NULLABLE                Does this field allow null values? False for the key column.
            // * DATA_TYPE                  Numerical value representing the data type (eg. 130 for "WChar" or string)
            // * TYPE_GUID                  ?
            // * CHARACTER_MAXIMUM_LENGTH   Null for integral types. Number of characters for non-integral types.
            // * CHARACTER_OCTET_LENGTH     If available, appears to be double CHARACTER_MAXIMUM_LENGTH
            // * NUMERIC_PRECISION          Set by the data type, analogous to OdbcConnection::COLUMN_SIZE
            // * NUMERIC_SCALE              ?
            // * DATETIME_PRECISION         Mostly null. If available, appears to indicate the field of type "Date".
            // * CHARACTER_SET_CATALOG      ?
            // * CHARACTER_SET_SCHEMA       ?
            // * CHARACTER_SET_NAME         ?
            // * COLLATION_CATALOG          ?
            // * COLLATION_SCHEMA           ?
            // * COLLATION_NAME             ?
            // * DOMAIN_CATALOG             ?
            // * DOMAIN_SCHEMA              ?
            // * DOMAIN_NAME                ?
            // * DESCRIPTION                Description of field
            #endregion // Schema details

            // Specify the Catalog, Schema, Table Name, Column Name to get the specified column(s)
            // * Index 0 represents Catalog
            // * Index 1 represents Schema
            // * Index 2 represents Table Name
            // * Index 3 represents Column Name
            SchemaFieldDefinition fd = new SchemaFieldDefinition();
            string[] columnRestrictions = new String[4];
            columnRestrictions[2] = table;
            DataTable schemaColumns = connection.GetSchema(Schema_Columns, columnRestrictions);
            if (schemaColumns.Rows.Count > 0)
            {
                columns.Add(m_fieldHeader);
                foreach (DataRow row in schemaColumns.Rows)
                {
                    if (connection is System.Data.Odbc.OdbcConnection)
                    {
                        // ODBC
                        fd.name = "fred";
                        columns.Add(string.Format(Schema_Header_Column_Formatting,
                            row[Schema_Columns_ODBC_Name],
                            row[Schema_Columns_ODBC_Type],
                            row[Schema_Columns_ODBC_Size],
                            row[Schema_Columns_ODBC_Nullable]));
                    }
                    else if (connection is System.Data.OleDb.OleDbConnection)
                    {
                        // OleDB
                        columns.Add(string.Format(Schema_Header_Column_Formatting,
                            row[Schema_Columns_OleDB_Name],
                            row[Schema_Columns_OleDB_Type],
                            row[Schema_Columns_OleDB_Size_Number],
                            row[Schema_Columns_OleDB_Nullable]));
                    }
                }
            }

            //System.Data.Odbc.OdbcType
        }

        private void GetSchemaColumnsFull(ref List<string> columns, DbConnection connection, string table)
        {
            // Retrieve full "Columns" schema information
            string[] columnRestrictions = new String[4];
            columnRestrictions[2] = table;
            DataTable schemaColumns = connection.GetSchema(Schema_Columns, columnRestrictions);
            foreach (DataRow row in schemaColumns.Rows)
            {
                foreach (DataColumn col in schemaColumns.Columns)
                {
                    columns.Add(string.Format("{0} = {1}", col.ColumnName, row[col]));
                }
            }
        }

        /*
         //
        // Summary:
        //     Specifies the data type of a field, a property, for use in an System.Data.OleDb.OleDbParameter.
        public enum OleDbType
        {
            //
            // Summary:
            //     No value (DBTYPE_EMPTY).
            Empty = 0,
            //
            // Summary:
            //     A 16-bit signed integer (DBTYPE_I2). This maps to System.Int16.
            SmallInt = 2,
            //
            // Summary:
            //     A 32-bit signed integer (DBTYPE_I4). This maps to System.Int32.
            Integer = 3,
            //
            // Summary:
            //     A floating-point number within the range of -3.40E +38 through 3.40E +38 (DBTYPE_R4).
            //     This maps to System.Single.
            Single = 4,
            //
            // Summary:
            //     A floating-point number within the range of -1.79E +308 through 1.79E +308 (DBTYPE_R8).
            //     This maps to System.Double.
            Double = 5,
            //
            // Summary:
            //     A currency value ranging from -2 63 (or -922,337,203,685,477.5808) to 2 63 -1
            //     (or +922,337,203,685,477.5807) with an accuracy to a ten-thousandth of a currency
            //     unit (DBTYPE_CY). This maps to System.Decimal.
            Currency = 6,
            //
            // Summary:
            //     Date data, stored as a double (DBTYPE_DATE). The whole portion is the number
            //     of days since December 30, 1899, and the fractional portion is a fraction of
            //     a day. This maps to System.DateTime.
            Date = 7,
            //
            // Summary:
            //     A null-terminated character string of Unicode characters (DBTYPE_BSTR). This
            //     maps to System.String.
            BSTR = 8,
            //
            // Summary:
            //     A pointer to an IDispatch interface (DBTYPE_IDISPATCH). This maps to System.Object.
            IDispatch = 9,
            //
            // Summary:
            //     A 32-bit error code (DBTYPE_ERROR). This maps to System.Exception.
            Error = 10,
            //
            // Summary:
            //     A Boolean value (DBTYPE_BOOL). This maps to System.Boolean.
            Boolean = 11,
            //
            // Summary:
            //     A special data type that can contain numeric, string, binary, or date data, and
            //     also the special values Empty and Null (DBTYPE_VARIANT). This type is assumed
            //     if no other is specified. This maps to System.Object.
            Variant = 12,
            //
            // Summary:
            //     A pointer to an IUnknown interface (DBTYPE_UNKNOWN). This maps to System.Object.
            IUnknown = 13,
            //
            // Summary:
            //     A fixed precision and scale numeric value between -10 38 -1 and 10 38 -1 (DBTYPE_DECIMAL).
            //     This maps to System.Decimal.
            Decimal = 14,
            //
            // Summary:
            //     A 8-bit signed integer (DBTYPE_I1). This maps to System.SByte.
            TinyInt = 16,
            //
            // Summary:
            //     A 8-bit unsigned integer (DBTYPE_UI1). This maps to System.Byte.
            UnsignedTinyInt = 17,
            //
            // Summary:
            //     A 16-bit unsigned integer (DBTYPE_UI2). This maps to System.UInt16.
            UnsignedSmallInt = 18,
            //
            // Summary:
            //     A 32-bit unsigned integer (DBTYPE_UI4). This maps to System.UInt32.
            UnsignedInt = 19,
            //
            // Summary:
            //     A 64-bit signed integer (DBTYPE_I8). This maps to System.Int64.
            BigInt = 20,
            //
            // Summary:
            //     A 64-bit unsigned integer (DBTYPE_UI8). This maps to System.UInt64.
            UnsignedBigInt = 21,
            //
            // Summary:
            //     A 64-bit unsigned integer representing the number of 100-nanosecond intervals
            //     since January 1, 1601 (DBTYPE_FILETIME). This maps to System.DateTime.
            Filetime = 64,
            //
            // Summary:
            //     A globally unique identifier (or GUID) (DBTYPE_GUID). This maps to System.Guid.
            Guid = 72,
            //
            // Summary:
            //     A stream of binary data (DBTYPE_BYTES). This maps to an System.Array of type
            //     System.Byte.
            Binary = 128,
            //
            // Summary:
            //     A character string (DBTYPE_STR). This maps to System.String.
            Char = 129,
            //
            // Summary:
            //     A null-terminated stream of Unicode characters (DBTYPE_WSTR). This maps to System.String.
            WChar = 130,
            //
            // Summary:
            //     An exact numeric value with a fixed precision and scale (DBTYPE_NUMERIC). This
            //     maps to System.Decimal.
            Numeric = 131,
            //
            // Summary:
            //     Date data in the format yyyymmdd (DBTYPE_DBDATE). This maps to System.DateTime.
            DBDate = 133,
            //
            // Summary:
            //     Time data in the format hhmmss (DBTYPE_DBTIME). This maps to System.TimeSpan.
            DBTime = 134,
            //
            // Summary:
            //     Data and time data in the format yyyymmddhhmmss (DBTYPE_DBTIMESTAMP). This maps
            //     to System.DateTime.
            DBTimeStamp = 135,
            //
            // Summary:
            //     An automation PROPVARIANT (DBTYPE_PROP_VARIANT). This maps to System.Object.
            PropVariant = 138,
            //
            // Summary:
            //     A variable-length numeric value (System.Data.OleDb.OleDbParameter only). This
            //     maps to System.Decimal.
            VarNumeric = 139,
            //
            // Summary:
            //     A variable-length stream of non-Unicode characters (System.Data.OleDb.OleDbParameter
            //     only). This maps to System.String.
            VarChar = 200,
            //
            // Summary:
            //     A long string value (System.Data.OleDb.OleDbParameter only). This maps to System.String.
            LongVarChar = 201,
            //
            // Summary:
            //     A variable-length, null-terminated stream of Unicode characters (System.Data.OleDb.OleDbParameter
            //     only). This maps to System.String.
            VarWChar = 202,
            //
            // Summary:
            //     A long null-terminated Unicode string value (System.Data.OleDb.OleDbParameter
            //     only). This maps to System.String.
            LongVarWChar = 203,
            //
            // Summary:
            //     A variable-length stream of binary data (System.Data.OleDb.OleDbParameter only).
            //     This maps to an System.Array of type System.Byte.
            VarBinary = 204,
            //
            // Summary:
            //     A long binary value (System.Data.OleDb.OleDbParameter only). This maps to an
            //     System.Array of type System.Byte.
            LongVarBinary = 205
        }
        */
        #endregion // Private methods
    }
}
