using System;
using System.IO;

using sysLib = systemHelperLibrary.SystemLibrary;
using System.Linq.Expressions;

namespace SimpleDbReader
{
    class DbTester
    {
        #region Constants
        // Database names
        private const int cPerformanceLoops = 100;

        //P:\HqVar\_clin\Data
        #endregion  // Constants

        #region Member variables
        private Simple_DAO m_simpleDAO = null;      // SimpleTest.mdb
        private Simple_ODBC m_simpleODBC = null;

        private Northwind_DAO m_nwDAO = null;       // Northwind databases
        private Northwind_ODBC m_nwODBC = null;
        private Northwind_OleDB m_nwOleDB = null;

        // Example for using indexed property accessors
        private string[] sportTypes = {
            "Baseball", "Basketball", "Chess", "Football", "Hockey",
            "Rugby", "Soccer", "Tennis", "Volleyball" };
        #endregion  // Member variables

        // Constructor
        public DbTester() { }

        // Properties
        public string this[int sport]
        {
            // Example usage of "single expression" indexed property accessors
            get => sportTypes[sport];
            set => sportTypes[sport] = value;
        }

        // Start: Methods (public)
        public override string ToString()
        {
            // Method that overrides the base class (System.Object) implementation
            return "DbTester Sample Application";
        }

        public void Initialise()
        {
            // Set up general configuration (such as where databases are located)
            // Note: Sample databases are assumed to be located in the %DevDataDirectory% folder
            // (eg. C:\Apps\Data). If not defined, add this User environment variable to Windows and
            // restart Visual Studio (or alternatively hard-code the path).
            ConfigGeneral cfgGeneral = new ConfigGeneral();
            cfgGeneral.b64bit = sysLib.Is64Bit();
            cfgGeneral.strDevDataPath = Environment.GetEnvironmentVariable("DevDataDirectory");

            // Set generic query details (these can be updated later)
            ConfigDatabase cfgDatabase = new ConfigDatabase();
            cfgDatabase.queryType = QueryType.eQueryStd1;
            cfgDatabase.paramValue = 5;

            // Create objects to test databases
            m_simpleDAO = new Simple_DAO(cfgGeneral, cfgDatabase);
            m_simpleODBC = new Simple_ODBC(cfgGeneral, cfgDatabase);

            m_nwDAO = new Northwind_DAO(cfgGeneral, cfgDatabase);
            m_nwODBC = new Northwind_ODBC(cfgGeneral, cfgDatabase);
            m_nwOleDB = new Northwind_OleDB(cfgGeneral, cfgDatabase);
        }

        public void SetQuerySELECT(QueryType eQuery)
        {
            // Update the generic SELECT query string
            m_simpleDAO.SetQuerySELECT(eQuery);
            m_simpleODBC.SetQuerySELECT(eQuery);

            m_nwDAO.SetQuerySELECT(eQuery);
            m_nwODBC.SetQuerySELECT(eQuery);
            m_nwOleDB.SetQuerySELECT(eQuery);
        }

        public void SetQueryINSERT(QueryType eQuery)
        {
            // Update the INSERT query string
            m_simpleDAO.SetQueryINSERT(eQuery);
            m_simpleODBC.SetQueryINSERT(eQuery);

            m_nwDAO.SetQueryINSERT(eQuery);
            m_nwODBC.SetQueryINSERT(eQuery);
            m_nwOleDB.SetQueryINSERT(eQuery);
        }

        public void SetQueryUPDATE(QueryType eQuery)
        {
            // Set the UPDATE query string
            m_simpleDAO.SetQueryUPDATE(eQuery);
            m_simpleODBC.SetQueryUPDATE(eQuery);

            m_nwDAO.SetQueryUPDATE(eQuery);
            m_nwODBC.SetQueryUPDATE(eQuery);
            m_nwOleDB.SetQueryUPDATE(eQuery);
        }

        public void SetQueryDELETE(QueryType eQuery)
        {
            // Update the DELETE query string
            m_simpleDAO.SetQueryDELETE(eQuery);
            m_simpleODBC.SetQueryDELETE(eQuery);

            m_nwDAO.SetQueryDELETE(eQuery);
            m_nwODBC.SetQueryDELETE(eQuery);
            m_nwOleDB.SetQueryDELETE(eQuery);
        }

        public void SimpleCreateDB()
        {
            // Create a database, add some tables and columns, then finally delete the database
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);
            DAO.Database db = null;
            string strDatabase = "C:\\Apps\\Data\\SimpleTestTmp.mdb";
            if (File.Exists(strDatabase))
            {
                // Temporary database already exists, so just open it
                db = dbEngine.OpenDatabase(strDatabase, false, false);
            }
            else
            {
                // Temporary database does not exist, so create it
                string strLocale = DAO.LanguageConstants.dbLangGeneral;
                db = dbEngine.CreateDatabase(
                    strDatabase,
                    strLocale,                          // Append "";pwd=NewPassword" to use a password
                    DAO.DatabaseTypeEnum.dbVersion40    // Using JET 4.0 (before Access 2007)
                    );
            }

            if (db != null)
            {
                //db.
                //DAO.TableDef td = db.CreateTableDef("Contacts");
                //td.Fields.Append.Create
                //db.Close();
            }

            // Finally delete the temporary database (comment out if you want to view the DB)
            //if (File.Exists(strDatabase))
            //    File.Delete(strDatabase);
        }

        public void SimpleStats(DatabaseTechnology eTechnology)
        {
            // Statistics and schema information for the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_simpleDAO.GetStats();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_simpleODBC.GetStats();

            // TODO: OleDB
        }

        public void SimpleRead(DatabaseTechnology eTechnology)
        {
            // Read some data from the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_simpleDAO.Read();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_simpleODBC.Read();

            // TODO: OleDB
        }

        public void SimpleWriteable(DatabaseTechnology eTechnology)
        {
            // Open the database in read-only and writeable mode
            // TODO
        }

        public void SimpleInsert(DatabaseTechnology eTechnology)
        {
            // Insert a record into the database
            // TODO
        }

        public void SimpleUpdate(DatabaseTechnology eTechnology)
        {
            // Update an existing record in the database
            // TODO
        }

        public void SimpleDelete(DatabaseTechnology eTechnology)
        {
            // Delete an existing record in the database
            // TODO
        }

        public void NorthwindOpenCloseWithDAO()
        {
            // Dummy open/close of the database (which seems to affect performance tests)
            m_nwDAO.DummyOpenClose();
        }

        public void NorthwindStats(DatabaseTechnology eTechnology)
        {
            // Statistics and schema information for the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.GetStats();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.GetStats();
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.GetStats();
        }

        public void NorthwindRead(DatabaseTechnology eTechnology)
        {
            // Read some data from the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.Read();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.Read();
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.Read();
        }

        public void NorthwindWriteable(DatabaseTechnology eTechnology)
        {
            // Open the database in read-only and writeable mode
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.Writeable();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.Writeable();
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.Writeable();
        }

        public void NorthwindInsert(DatabaseTechnology eTechnology)
        {
            // Insert a record into the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.Insert();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.Insert();
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.Insert();
        }

        public void NorthwindUpdate(DatabaseTechnology eTechnology)
        {
            // Update an existing record in the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.Update();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.Update();
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.Update();
        }

        public void NorthwindDelete(DatabaseTechnology eTechnology)
        {
            // Delete an existing record in the database
            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.Delete();
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.Delete();
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.Delete();
        }

        public void NorthwindPerformance(DatabaseTechnology eTechnology, int nLoops = cPerformanceLoops)
        {
            // This version runs some performance tests
            if (nLoops <= 0)
                nLoops = 1;

            if (nLoops > 100000)
                nLoops = cPerformanceLoops;

            if (eTechnology == DatabaseTechnology.eDB_DAO)
                m_nwDAO.PerformanceTest(nLoops);
            else if (eTechnology == DatabaseTechnology.eDB_ODBC)
                m_nwODBC.PerformanceTest(nLoops);
            else if (eTechnology == DatabaseTechnology.eDB_OleDB)
                m_nwOleDB.PerformanceTest(nLoops);
        }
        // End: Methods (public)     
    }
}