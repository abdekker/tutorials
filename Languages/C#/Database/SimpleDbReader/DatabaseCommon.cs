namespace SimpleDbReader
{
    // Base class for the testing the Northwind database
    public abstract class DatabaseCommon
    {
        #region Constants
        // Date/Time strings in ISO 8601 format (yyyy-MM-ddTHH:mm:ss)
        public const string cszDateTimeISO8601 = "yyyy-MM-ddTHH:mm:ss";
        public const string cszDateISO8601 = "yyyy-MM-dd";
        public const string cszTimeISO8601 = "HH:mm:ss";
        #endregion // Constants

        #region Member variables
        protected ConfigGeneral m_cfgGeneral;
        protected ConfigDatabase m_cfgDatabase;
        #endregion  // Member variables

        // Constructor
        public DatabaseCommon(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase)
        {
            m_cfgGeneral = cfgGeneral;
            m_cfgDatabase = cfgDatabase;
            m_cfgDatabase.querySELECT = HelperGetQuerySELECT();
            m_cfgDatabase.queryINSERT = HelperGetQueryINSERT();
            m_cfgDatabase.queryUPDATE = HelperGetQueryUPDATE();
            m_cfgDatabase.queryDELETE = HelperGetQueryDELETE();
        }

        public void SetQuerySELECT(QueryType eQuery)
        {
            // Update the query
            m_cfgDatabase.queryType = eQuery;
            m_cfgDatabase.querySELECT = HelperGetQuerySELECT();
        }

        public void SetQueryINSERT(QueryType eQuery)
        {
            // Update the query
            m_cfgDatabase.queryType = eQuery;
            m_cfgDatabase.queryINSERT = HelperGetQueryINSERT();
        }

        public void SetQueryUPDATE(QueryType eQuery)
        {
            // Update the query
            m_cfgDatabase.queryType = eQuery;
            m_cfgDatabase.queryUPDATE = HelperGetQueryUPDATE();
        }

        public void SetQueryDELETE(QueryType eQuery)
        {
            // Update the query
            m_cfgDatabase.queryType = eQuery;
            m_cfgDatabase.queryDELETE = HelperGetQueryDELETE();
        }

        // Abstract methods
        public abstract void GetStats();
        public abstract void Read();
        public abstract void Writeable();
        public abstract void Insert();
        public abstract void Update();
        public abstract void Delete();
        public abstract void PerformanceTest(int nLoops);
        public abstract bool SetConnectionString(ref string strConnection);

        protected abstract void Connect_Stats(string strConnection);
        protected abstract void Connect_Read(string strConnection);
        protected abstract void Connect_Writeable(string strConnection);
        protected abstract void Connect_Insert(string strConnection);
        protected abstract void Connect_Update(string strConnection);
        protected abstract void Connect_Delete(string strConnection);
        protected abstract int Connect_PerformanceTest(string strConnection);

        // Helper methods
        protected string HelperGetAccessName(bool bFullDescription)
        {
            // Provide a human-readable name for the access database
            string strName;
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
                    strName = "Access 97";
                    if (bFullDescription)
                    {
                        if ((m_cfgDatabase.dbTech == DatabaseTechnology.eDB_DAO) ||
                            (m_cfgDatabase.dbTech == DatabaseTechnology.eDB_OleDb))
                            strName += " (32-bit using Microsoft.Jet.OLEDB.4.0)";
                        else if (m_cfgDatabase.dbTech == DatabaseTechnology.eDB_ODBC)
                            strName += " (32-bit using Microsoft Access Driver)";
                    }
                    break;

                case MSAccessDbType.eMSAccess2000:
                    strName = "Access 2000";
                    if (bFullDescription)
                    {
                        if ((m_cfgDatabase.dbTech == DatabaseTechnology.eDB_DAO) ||
                            (m_cfgDatabase.dbTech == DatabaseTechnology.eDB_OleDb))
                            strName += " (32-bit using Microsoft.Jet.OLEDB.4.0)";
                        else if (m_cfgDatabase.dbTech == DatabaseTechnology.eDB_ODBC)
                            strName += " (32-bit using Microsoft Access Driver)";
                    }
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    strName = "Access 2007-2016";
                    if (bFullDescription)
                    {
                        if ((m_cfgDatabase.dbTech == DatabaseTechnology.eDB_DAO) ||
                            (m_cfgDatabase.dbTech == DatabaseTechnology.eDB_OleDb))
                            strName += " (64-bit using Microsoft.ACE.OLEDB.16.0)";
                        else if (m_cfgDatabase.dbTech == DatabaseTechnology.eDB_ODBC)
                            strName += " (64-bit using Microsoft Access Driver)";
                    }
                    break;

                default:
                    strName = "Unknown Access database";
                    break;
            }

            return strName;
        }

        protected string HelperGetQuerySELECT()
        {
            // Create a simple SELECT query string. General format of SELECT queries:
            // #    SELECT * FROM table
            // #    SELECT column1, column2, ... FROM table
            // #    SELECT * FROM table WHERE column = 'value'
            // #    SELECT * FROM table ORDER BY column ASC
            // #    SELECT * FROM table WHERE column1 = 'value' ORDER BY column2 DESC
            // #    SELECT DISTINCT column FROM table

            // Note: The "@pricePoint" parameter only applies to the sample Northwind databases
            string sqlQuery = string.Empty;
            switch (m_cfgDatabase.queryType)
            {
                case QueryType.eQueryStd1:
                default:
                    // Query taken directly from the Microsoft tutorial (Northwind)
                    sqlQuery = (
                        "SELECT ProductID, UnitPrice, ProductName FROM Products " +
                        "WHERE UnitPrice > ? " +
                        "ORDER BY UnitPrice DESC;");
                    break;

                case QueryType.eQueryStd2:
                    // Query taken directly from the w3schools tutorial (SimpleTest)
                    sqlQuery = ("SELECT * FROM Members;");
                    break;

                case QueryType.eQueryLike:
                    // Testing the "LIKE" operator of the WHERE clause. This query might return:
                    // * Vegie-spread
                    // * Grandma's Boysenberry Spread
                    // * Scottish Longbreads
                    sqlQuery = (
                        "SELECT ProductID, UnitPrice, ProductName FROM Products " +
                        "WHERE ProductName LIKE '%read%' " +
                        "ORDER BY UnitPrice DESC;");
                    break;
            }

            return sqlQuery;

            // Comments in SQL statements: Use "--" (single line only) or "/*...*/" (can span multi-lines):
            //  string sqlWithComments =
            //    "SELECT ProductID, UnitPrice FROM Products  -- Select two columns from the products table" +
            //    "WHERE UnitPrice > ?                        -- Only choose rows where the unit price is NOT null" +
            //    "ORDER BY UnitPrice DESC;                   -- Order rows by the UnitPrice";
        }

        protected string HelperGetQueryINSERT()
        {
            // Create a INSERT query string to add a record to the database. General format of INSERT queries:
            // #    INSERT INTO table (column1,column2,...)
            //      VALUES(value1,value2,...)
            // Note: If one of the columns is designated as "AutoInc", the column can be omitted

            // General rules for values:
            // * Give number values as is eg. 92
            // * Enclose string variables in '' eg. 'Germany'

            /* Products table in the Northwind Database
                Name                     Type    TypeName          Size         Nullable
                ProductID                4       COUNTER           10           NO
                ProductName              -9      VARCHAR           40           YES
                SupplierID               4       INTEGER           10           YES
                CategoryID               4       INTEGER           10           YES
                QuantityPerUnit          -9      VARCHAR           20           YES
                UnitPrice                2       CURRENCY          19           YES
                UnitsInStock             5       SMALLINT          5            YES
                UnitsOnOrder             5       SMALLINT          5            YES
                ReorderLevel             5       SMALLINT          5            YES
                Discontinued             -7      BIT               1            NO

            NB! SupplierID must match a supplier in the Suppliers table; range is 1 (Exotic Liquids) to 29 (Forêts d'érables)
            NB! CategoryID must match a category in the Categories table; range is 1 (Beverages) to 8 (Seafood) */
            string sqlQuery = string.Empty;
            switch (m_cfgDatabase.queryType)
            {
                case QueryType.eQueryStd1:
                default:
                    // Northwind sample database
                    sqlQuery = (
                        "INSERT INTO Products (ProductID,ProductName,SupplierID,CategoryID,QuantityPerUnit,UnitPrice,UnitsInStock,UnitsOnOrder,ReorderLevel,Discontinued) " +
                        "VALUES (999,'TestProduct1',27,8,'TestQuantity1',9.87,9977,17,12,0);");
                    break;

                case QueryType.eQueryStd2:
                    // SimpleTest test database
                   // TODO
                    break;

                case QueryType.eQueryLike:
                    // TODO
                    break;
            }

            return sqlQuery;
        }

        protected string HelperGetQueryUPDATE()
        {
            // TODO
            return string.Empty;
        }

        protected string HelperGetQueryDELETE()
        {
            // TODO
            return string.Empty;
        }
    }
}
