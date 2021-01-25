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
        protected DatabaseTechnology m_tech = DatabaseTechnology.eDB_Unknown;
        #endregion  // Member variables

        // Constructor
        public DatabaseCommon(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase)
        {
            NorthwindConfigGeneral = cfgGeneral;
            NorthwindConfigDatabase = cfgDatabase;
        }

        //public void UpdateConfigGeneral(ConfigGeneral cfg) { NorthwindConfigGeneral = cfg; }
        //public void UpdateConfigDatabase(ConfigDatabase cfg) { NorthwindConfigDatabase = cfg; }
        public void UpdateQuery(QueryType eQuery)
        {
            // Update the query
            m_cfgDatabase.queryType = eQuery;
            m_cfgDatabase.strQuery = HelperGetQueryString();
        }

        // Properties
        private ConfigGeneral NorthwindConfigGeneral
        {
            get { return m_cfgGeneral; }
            set { m_cfgGeneral = value; }

            // Since C# 7.0 (VS 2017, .NET 4.7) you can write single expression property get/set
            // accessors like below. In my opinion this is more difficult to read!
            /*get => m_cfgGeneral;
            set => m_cfgGeneral = value;*/
        }
        private ConfigDatabase NorthwindConfigDatabase
        {
            get { return m_cfgDatabase; }
            set
            {
                m_cfgDatabase = value;
                m_cfgDatabase.strQuery = HelperGetQueryString();
            }
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
                        if ((m_tech == DatabaseTechnology.eDB_DAO) ||
                            (m_tech == DatabaseTechnology.eDB_OleDb))
                            strName += " (32-bit using Microsoft.Jet.OLEDB.4.0)";
                        else if (m_tech == DatabaseTechnology.eDB_ODBC)
                            strName += " (32-bit using Microsoft Access Driver)";
                    }
                    break;

                case MSAccessDbType.eMSAccess2000:
                    strName = "Access 2000";
                    if (bFullDescription)
                    {
                        if ((m_tech == DatabaseTechnology.eDB_DAO) ||
                            (m_tech == DatabaseTechnology.eDB_OleDb))
                            strName += " (32-bit using Microsoft.Jet.OLEDB.4.0)";
                        else if (m_tech == DatabaseTechnology.eDB_ODBC)
                            strName += " (32-bit using Microsoft Access Driver)";
                    }
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    strName = "Access 2007-2016";
                    if (bFullDescription)
                    {
                        if ((m_tech == DatabaseTechnology.eDB_DAO) ||
                            (m_tech == DatabaseTechnology.eDB_OleDb))
                            strName += " (64-bit using Microsoft.ACE.OLEDB.16.0)";
                        else if (m_tech == DatabaseTechnology.eDB_ODBC)
                            strName += " (64-bit using Microsoft Access Driver)";
                    }
                    break;

                default:
                    strName = "Unknown Access database";
                    break;
            }

            return strName;
        }
        protected string HelperGetQueryString()
        {
            // Use a standard query string while accessing the demo Northwind databases
            // Note: The "@pricePoint" 
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
    }
}
