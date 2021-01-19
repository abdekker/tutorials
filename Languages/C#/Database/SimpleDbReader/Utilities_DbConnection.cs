using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;

using typeLib = systemHelperLibrary.TypesLibrary;

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
            public int type;
            public string typeName;
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
        private readonly string Schema_Columns_ODBC_Type = "DATA_TYPE";
        private readonly string Schema_Columns_ODBC_TypeName = "TYPE_NAME";
        private readonly string Schema_Columns_ODBC_Size = "COLUMN_SIZE";
        private readonly string Schema_Columns_ODBC_Nullable = "IS_NULLABLE";

        private readonly string Schema_Columns_OleDB_Name = "COLUMN_NAME";
        private readonly string Schema_Columns_OleDB_Type = "DATA_TYPE";
        private readonly string Schema_Columns_OleDB_Size_Number = "NUMERIC_PRECISION";
        private readonly string Schema_Columns_OleDB_Size_String = "CHARACTER_MAXIMUM_LENGTH";
        private readonly string Schema_Columns_OleDB_Size_Date = "DATETIME_PRECISION";
        private readonly string Schema_Columns_OleDB_Nullable = "IS_NULLABLE";

        private readonly string Schema_Header_Column_Formatting = "{0,-25}{1,-8}{2,-15}{3,-13}{4}";
        private readonly string Schema_Header_Column_Name = "Name";
        private readonly string Schema_Header_Column_Type = "Type";
        private readonly string Schema_Header_Column_TypeName = "Type Name";
        private readonly string Schema_Header_Column_Size = "Size";
        private readonly string Schema_Header_Column_Nullable = "Nullable";
        #endregion // Constants

        public Utilities_DbConnection()
        {
            // Header when displaying field information
            m_fieldHeader = string.Format(Schema_Header_Column_Formatting,
                Schema_Header_Column_Name,
                Schema_Header_Column_Type,
                Schema_Header_Column_TypeName,
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
                addTable = true;
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
                        // Add the tables names. These are held in the "TABLE_NAME" column.
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
            /* ### OdbcConnection ###
            See https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/odbc-schema-collections
            ColumnName                  DataType    Description
            TABLE_CAT                   String      Name of database (eg. C:\Apps\Data\Northwind 97)
            TABLE_SCHEM                 String      ?
            TABLE_NAME                  String      Name of table to which this column belongs (eg. Products)
            COLUMN_NAME                 String      Field name (eg. ProductID)
            DATA_TYPE                   Int16       Data type as a string numerical code (eg. "4" representing COUNTER or INTEGER)
            TYPE_NAME                   String      Data type as a string (eg. COUNTER)
            COLUMN_SIZE                 Int32       Set by the data type, except VARCHAR and NVARCHAR (eg. 1 for BIT, 10 for INTEGER, etc)
            BUFFER_LENGTH               Int32       As above (eg. 1 for BIT, 2 for SMALLINT, 4 for INTEGER, 21 for CURRENCY, etc)
            DECIMAL_DIGITS              Int16       Null for string types and 0 for integral types
            NUM_PREC_RADIX              Int16       ? (Appears to be the base for numerical fields)
            NULLABLE                    Int16       Does this field allow null values? 0 for the key column.
            REMARKS                     String      Description of field
            COLUMN_DEF                  String      ?
            SQL_DATA_TYPE               Int16       Appears to be the same as DATA_TYPE
            SQL_DATETIME_SUB            Int16       Appears to be 3 for fields of type "DATETIME", otherwise null
            CHAR_OCTET_LENGTH           Int32       Appears to be the same as BUFFER_LENGTH for non-integral types
            ORDINAL_POSITION            Int32       Column number (eg. 1)
            IS_NULLABLE                 String      Does this field allow null values as string? NO for the key column.
            ORDINAL                     ?           Column number (eg. 1)
            Note: Schema information may differ between ODBC drivers */

            /* ### OleDbConnection ###
            See https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/ole-db-schema-collections
            ColumnName                  DataType    Description
            TABLE_CATALOG               String      ?
            TABLE_SCHEMA                String      ?
            TABLE_NAME                  String      Name of table to which this column belongs (eg. Products)
            COLUMN_NAME                 String      Field name (eg. HireDate)
            COLUMN_GUID                 Guid        ?
            COLUMN_PROPID               Int64       ?
            ORDINAL_POSITION            Int64       Column number (eg. 1)
            COLUMN_HASDEFAULT           Boolean     Mostly null. If available, then COLUMN_DEFAULT will have a value.
            COLUMN_DEFAULT              String      Mostly empty. If available, the default value for the field.
            COLUMN_FLAGS                Int64       ?
            IS_NULLABLE                 Boolean     Does this field allow null values? False for the key column.
            DATA_TYPE                   Int32       Numerical value representing the data type (eg. 130 for "WChar" or string)
            TYPE_GUID                   Guid        ?
            CHARACTER_MAXIMUM_LENGTH    Int64       Null for integral types. Number of characters for non-integral types.
            CHARACTER_OCTET_LENGTH      Int64       If available, appears to be double CHARACTER_MAXIMUM_LENGTH
            NUMERIC_PRECISION           Int32       Set by the data type, analogous to OdbcConnection::COLUMN_SIZE
            NUMERIC_SCALE               Int16       ?
            DATETIME_PRECISION          Int64       Mostly null. If available, appears to indicate the field of type "Date".
            CHARACTER_SET_CATALOG       String      ?
            CHARACTER_SET_SCHEMA        String      ?
            CHARACTER_SET_NAME          String      ?
            COLLATION_CATALOG           String      ?
            COLLATION_SCHEMA            String      ?
            COLLATION_NAME              String      ?
            DOMAIN_CATALOG              String      ?
            DOMAIN_SCHEMA               String      ?
            DOMAIN_NAME                 String      ?
            DESCRIPTION                 String      Description of field */
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
                    ConvertRowToSchemaColumns(ref fd, row, connection);
                    columns.Add(string.Format(Schema_Header_Column_Formatting,
                        fd.name,
                        fd.type,
                        fd.typeName,
                        fd.size,
                        fd.nullable));

                    // Alternatively, you code it right here:
                    /*if (connection is System.Data.Odbc.OdbcConnection)
                    {
                        // ODBC
                        columns.Add(string.Format(Schema_Header_Column_Formatting,
                            row[Schema_Columns_ODBC_Name],
                            row[Schema_Columns_ODBC_Type],
                            row[Schema_Columns_ODBC_TypeName],
                            row[Schema_Columns_ODBC_Size],
                            row[Schema_Columns_ODBC_Nullable]));                        
                    }
                    else if (connection is System.Data.OleDb.OleDbConnection)
                    {
                        // OleDB
                        columns.Add(string.Format(Schema_Header_Column_Formatting,
                            row[Schema_Columns_OleDB_Name],
                            row[Schema_Columns_OleDB_Type],
                            "(name)",
                            row[Schema_Columns_OleDB_Size_Number],
                            row[Schema_Columns_OleDB_Nullable]));
                    }*/
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

        private void ConvertRowToSchemaColumns(ref SchemaFieldDefinition fd, DataRow row, DbConnection connection)
        {
            // Helper function to convert schema information (in a DataRow variable) into our own format
            if (connection is System.Data.Odbc.OdbcConnection)
            {
                // ODBC
                try
                {
                    fd.name = (string)row[Schema_Columns_ODBC_Name];
                }
                catch { fd.name = "(name)"; }

                try
                {
                    // For ODBC data types, preferentially use "TYPE_NAME" to determine the field type. ODBC must work
                    // across multiple data sources and the mappings from underlying SQL data types to ODBC type
                    // identifiers are only approximate.
                    // Note: The "DATA_TYPE" column for ODBC data sources is actually an Int16
                    fd.type = Convert.ToInt32(row[Schema_Columns_ODBC_Type]);
                }
                catch
                {
                    // The value contained in COUNTER or INTEGER columns is "4", but the value of "4" in
                    // System.Data.Odbc.OdbcType is "Char" (and the "Int" type is "10"). These appear to be different
                    // types...to be investigated.
                    fd.type = (int)System.Data.Odbc.OdbcType.Int;
                }

                try
                {
                    fd.typeName = (string)row[Schema_Columns_ODBC_TypeName];
                }
                catch
                { fd.typeName = "(type)"; }

                try
                {
                    fd.size = (int)row[Schema_Columns_ODBC_Size];
                }
                catch { fd.size = -1; }

                try
                {
                    fd.nullable = (string)row[Schema_Columns_ODBC_Nullable];
                }
                catch { fd.nullable = "(nullable)"; }
            }
            else if (connection is System.Data.OleDb.OleDbConnection)
            {
                // OleDB
                try
                {
                    fd.name = (string)row[Schema_Columns_OleDB_Name];
                }
                catch { fd.name = "(name)"; }

                try
                {
                    fd.type = (int)row[Schema_Columns_OleDB_Type];
                }
                catch { fd.type = (int)System.Data.OleDb.OleDbType.Integer; }

                try
                {
                    if (Enum.IsDefined(typeof(System.Data.OleDb.OleDbType), fd.type))
                        fd.typeName = ((System.Data.OleDb.OleDbType)fd.type).ToString();
                }
                catch { fd.typeName = "(type)"; }

                try
                {
                    // Note:
                    // * "NUMERIC_PRECISION"            => Int32
                    // * "CHARACTER_MAXIMUM_LENGTH"     =>  Int64
                    // * "DATETIME_PRECISION"           =>  Int64
                    fd.size =Convert.ToInt32(row[GetOleDBTypeSchemaSizeColumn(fd.type)]);
                }
                catch { fd.size = -1; }

                try
                {
                    //fd.nullable = (string)row[Schema_Columns_OleDB_Nullable];
                    fd.nullable = Convert.ToString(row[Schema_Columns_OleDB_Nullable]);
                }
                catch { fd.nullable = "(nullable)"; }
            }
        }

        private string GetOleDBTypeSchemaSizeColumn(int type)
        {
            // Convert the OleDB type to the schema column name required to read the size of that type
            // Note: Some of these are guessed because I only have a limited set of sample databases
            string column = Schema_Columns_OleDB_Size_Number;
            if (Enum.IsDefined(typeof(System.Data.OleDb.OleDbType), type))
            {
                System.Data.OleDb.OleDbType typeOleDb = ((System.Data.OleDb.OleDbType)type);
                switch (typeOleDb)
                {
                    case System.Data.OleDb.OleDbType.Empty:
                    case System.Data.OleDb.OleDbType.SmallInt:
                    case System.Data.OleDb.OleDbType.Integer:
                    case System.Data.OleDb.OleDbType.Single:
                    case System.Data.OleDb.OleDbType.Double:
                    case System.Data.OleDb.OleDbType.Currency:
                    case System.Data.OleDb.OleDbType.IDispatch:
                    case System.Data.OleDb.OleDbType.Error:
                    case System.Data.OleDb.OleDbType.Variant:
                    case System.Data.OleDb.OleDbType.IUnknown:
                    case System.Data.OleDb.OleDbType.Decimal:
                    case System.Data.OleDb.OleDbType.TinyInt:
                    case System.Data.OleDb.OleDbType.UnsignedTinyInt:
                    case System.Data.OleDb.OleDbType.UnsignedSmallInt:
                    case System.Data.OleDb.OleDbType.UnsignedInt:
                    case System.Data.OleDb.OleDbType.UnsignedBigInt:
                    case System.Data.OleDb.OleDbType.Guid:
                    case System.Data.OleDb.OleDbType.Numeric:
                    case System.Data.OleDb.OleDbType.PropVariant:
                    case System.Data.OleDb.OleDbType.VarNumeric:
                    case System.Data.OleDb.OleDbType.VarBinary:
                    case System.Data.OleDb.OleDbType.LongVarBinary:
                        column = Schema_Columns_OleDB_Size_Number;
                        break;

                    case System.Data.OleDb.OleDbType.Date:
                    case System.Data.OleDb.OleDbType.Filetime:
                    case System.Data.OleDb.OleDbType.DBDate:
                    case System.Data.OleDb.OleDbType.DBTime:
                    case System.Data.OleDb.OleDbType.DBTimeStamp:
                        column = Schema_Columns_OleDB_Size_Date;
                        break;

                    case System.Data.OleDb.OleDbType.BSTR:
                        case System.Data.OleDb.OleDbType.Boolean:
                    case System.Data.OleDb.OleDbType.Binary:
                    case System.Data.OleDb.OleDbType.Char:
                    case System.Data.OleDb.OleDbType.WChar:
                    case System.Data.OleDb.OleDbType.VarChar:
                    case System.Data.OleDb.OleDbType.LongVarChar:
                    case System.Data.OleDb.OleDbType.VarWChar:
                    case System.Data.OleDb.OleDbType.LongVarWChar:
                        column = Schema_Columns_OleDB_Size_String;
                        break;
                }
            }

            return column;
        }
        #endregion // Private methods
    }
}
