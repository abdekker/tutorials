using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;

using System.Data.Odbc;
using System.Data.OleDb;

namespace SimpleDbReader
{
    class Utilities_DbConnection : UtilitiesBase
    {
        // Utilities for using both ODBC and OleDB:
        // * System.Data.DbConnection (such as System.Data.Odbc.OdbcConnection)
        // * System.Data.DataTable (such as returned from DbConnection::GetSchema)

        #region Member variables
        private readonly Utilities_ODBC m_utilsODBC = new Utilities_ODBC();
        private readonly Utilities_OleDB m_utilsOleDB = new Utilities_OleDB();

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
        // Tables schema
        private readonly string Schema_Tables = "Tables";
        private readonly string Schema_Tables_Column_TableName = "TABLE_NAME";
        private readonly string Schema_Tables_Column_TableType = "TABLE_TYPE";

        // Columns schema
        private readonly string Schema_Columns = "Columns";
        #endregion // Constants

        public Utilities_DbConnection(DatabaseTechnology tech)
        {
            // This utility class could be used for ODBC, OleDB, SQL Server, etc
            DbTechnology = tech;

            // Header when displaying field information
            m_fieldHeader = string.Format(Schema_Header_Column_Formatting,
                Schema_Header_Column_Name,
                Schema_Header_Column_Type,
                Schema_Header_Column_TypeName,
                Schema_Header_Column_Size,
                Schema_Header_Column_Nullable);
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
            if (m_tech == DatabaseTechnology.eDB_ODBC)
                return m_utilsODBC.GetDbName(strConnection);
            else if (m_tech == DatabaseTechnology.eDB_OleDB)
                return m_utilsOleDB.GetDbName(strConnection);
            else
                return string.Empty;
        }

        public override string GetDbName(IDbConnection connection)
        {
            // Get the name of the database associated with the connection string
            if (m_tech == DatabaseTechnology.eDB_ODBC)
                return m_utilsODBC.GetDbName(connection);
            else if (m_tech == DatabaseTechnology.eDB_OleDB)
                return m_utilsOleDB.GetDbName(connection);
            else
                return string.Empty;
        }
        #endregion // Properties and methods from UtilitiesBase

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
            /* ### OdbcConnection ###
            See https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/odbc-schema-collections
            ColumnName          DataType    Description
            TABLE_CAT           String      Name of database (eg. C:\Apps\Data\Northwind 2000)
            TABLE_SCHEM         String      ?
            TABLE_NAME          String      Name of table (eg. Customers)
            TABLE_TYPE          String      Type of table ("SYSTEM TABLE" or "TABLE") [the ODBC "Tables" schema does not include Views]
            REMARKS             String      Description of table
            * Note, 1: Schema information may differ between ODBC drivers
            * Note, 2: The above schema applies to Views as well */

            /* ### OleDbConnection ###
            See https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/ole-db-schema-collections
            ColumnName          DataType    Description
            TABLE_CATALOG       String      ?
            TABLE_SCHEMA        String      ?
            TABLE_NAME          String      Name of table (eg. Customers)
            TABLE_TYPE          String      Type of table or view ("ACCESS TABLE", "TABLE" or "VIEW")
            TABLE_GUID          Guid        ?
            DESCRIPTION         String      Description of table
            TABLE_PROPID        Int64       ?
            DATE_CREATED        DateTime    Date created (eg. 13/09/1995 10:51:45)
            DATE_MODIFIED       DateTime    Date modified (eg. 12/03/2003 05:09:58) */
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
                        if (connection is OdbcConnection)
                        {
                            // ODBC
                            addTable = (!row[Schema_Tables_Column_TableType].Equals(m_utilsODBC.Schema_Tables_ODBC_SystemTable));
                        }
                        else if (connection is OleDbConnection)
                        {
                            // OleDB
                            addTable = (
                                (!row[Schema_Tables_Column_TableType].Equals(m_utilsOleDB.Schema_Tables_OleDB_SystemTable)) &&
                                (!row[Schema_Tables_Column_TableType].Equals(m_utilsOleDB.Schema_Tables_OleDB_ViewTable)));

                        }
                    }
                    catch (Exception ex)
                    {
                        Console.WriteLine(UtilitiesGeneral.FormatException(
                            this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
                    }
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
            // See the "Data type conversions" section for conversion of the ODBC "TYPE_NAME" or OleDB "DATA_TYPE" to .NET types

            /* ### OdbcConnection ###
            See https://docs.microsoft.com/en-us/dotnet/framework/data/adonet/odbc-schema-collections
            ColumnName                  DataType    Description
            TABLE_CAT                   String      Name of database (eg. C:\Apps\Data\Northwind 97)
            TABLE_SCHEM                 String      ?
            TABLE_NAME                  String      Name of table to which this column belongs (eg. Products)
            COLUMN_NAME                 String      Field name (eg. ProductID)
            DATA_TYPE                   Int16       Data type as a numerical code (eg. "4" representing COUNTER or INTEGER)
            TYPE_NAME                   String      Data type as a string (eg. "COUNTER")
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

            #region Data type conversions
            /*          DAO                 ODBC                OleDB                   C#          VB
                        TypeName    Type    TypeName    Type    TypeName        Type
                        ================    ================    ====================    ===================
            String      Text        10      VARCHAR     -9      WChar           130     string      String
                        Memo        1       LONGCHAR    -1      WChar           130     string      String

            Integer     Boolean     1       BIT         -7      Boolean         11      bool        Boolean
                        Byte        2       BYTE        -6      UnsignedTinyInt 17      byte        Byte
                        Integer     3       SMALLINT    5       SmallInt        2       short       Short
                        Long        4       INTEGER     4       Integer         3       int         Integer

            Float       Single      6       REAL        7       Single          4       float       Single
                        Double      7       DOUBLE      8       Double          5       double      Double
                        Currency    5       CURRENCY    2       Currency        6       decimal     Decimal

            DateTime    Date        8       DATETIME    93      Date            7       DateTime    Date

            Other       LongBinary  11      LONGBINARY  -4      Binary          128     ?           ? */
            #endregion // Data type conversions


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
                    /*if (connection is OdbcConnection)
                    {
                        // ODBC
                        columns.Add(string.Format(Schema_Header_Column_Formatting,
                            row[Schema_Columns_ODBC_Name],
                            row[Schema_Columns_ODBC_Type],
                            row[Schema_Columns_ODBC_TypeName],
                            row[Schema_Columns_ODBC_Size],
                            row[Schema_Columns_ODBC_Nullable]));
                    }
                    (etc) */
                }
            }
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
            if (connection is OdbcConnection)
            {
                // ODBC
                try
                {
                    fd.name = (string)row[m_utilsODBC.Schema_Columns_ODBC_Name];

                    // For ODBC data types, preferentially use "TYPE_NAME" to determine the field type. ODBC must work
                    // across multiple data sources and the mappings from underlying SQL data types to ODBC type
                    // identifiers are only approximate.
                    // Note: The "DATA_TYPE" column for ODBC data sources is actually an Int16
                    fd.type = Convert.ToInt32(row[m_utilsODBC.Schema_Columns_ODBC_Type]);

                    fd.typeName = (string)row[m_utilsODBC.Schema_Columns_ODBC_TypeName];
                    fd.size = (int)row[m_utilsODBC.Schema_Columns_ODBC_Size];
                    fd.nullable = (string)row[m_utilsODBC.Schema_Columns_ODBC_Nullable];
                }
                catch (Exception ex)
                {
                    Console.WriteLine(UtilitiesGeneral.FormatException(
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
                    fd.name = "(name)";

                    // The value contained in COUNTER or INTEGER columns is "4", but the value of "4" in
                    // System.Data.Odbc.OdbcType is "Char" (and the "Int" type is "10"). These appear to be different
                    // types...to be investigated.
                    fd.type = (int)OdbcType.Int;

                    fd.typeName = "(type)";
                    fd.size = -1;
                    fd.nullable = "(nullable)";
                }
            }
            else if (connection is OleDbConnection)
            {
                // OleDB
                try
                {
                    fd.name = (string)row[m_utilsOleDB.Schema_Columns_OleDB_Name];
                    fd.type = (int)row[m_utilsOleDB.Schema_Columns_OleDB_Type];
                    if (Enum.IsDefined(typeof(OleDbType), fd.type))
                        fd.typeName = ((OleDbType)fd.type).ToString();

                    // Note:
                    // NUMERIC_PRECISION            => Int32
                    // CHARACTER_MAXIMUM_LENGTH     => Int64
                    // DATETIME_PRECISION           => Int64
                    fd.size =Convert.ToInt32(row[m_utilsOleDB.GetOleDBTypeSchemaSizeColumn(fd.type)]);
                    fd.nullable = Convert.ToString(row[m_utilsOleDB.Schema_Columns_OleDB_Nullable]);
                }
                catch (Exception ex)
                {
                    Console.WriteLine(UtilitiesGeneral.FormatException(
                        this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, ex.Message));
                    fd.name = "(name)";
                    fd.type = (int)OleDbType.Integer;
                    fd.typeName = "(type)";
                    fd.size = -1;
                    fd.nullable = "(nullable)";
                }
            }
        }
        #endregion // Private methods
    }
}
