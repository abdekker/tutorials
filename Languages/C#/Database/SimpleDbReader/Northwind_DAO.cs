using System;

namespace SimpleDbReader
{
    public class Northwind_DAO : DatabaseCommon
    {
        // Member variables unique to this class
        private readonly Utilities_DAO m_utilsDAO = new Utilities_DAO();

        // Constructor
        public Northwind_DAO(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses DAO
            m_tech = DatabaseTechnology.eDB_DAO;

            // Set the main database query
            m_cfgDatabase.strQuery = HelperGetQueryString();
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            // DAO (Data Access Objects)
            Console.WriteLine("### START: DAO (stats, Northwind) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Stats(strConnection);
            }

            Console.WriteLine("### END: DAO (stats) ###\n");
        }

        public override void Read()
        {
            // DAO (Data Access Objects)
            Console.WriteLine("### START: DAO (read) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Read(strConnection);
            }

            Console.WriteLine("### END: DAO (read) ###\n");
        }

        public override void Write()
        {
            // DAO (Data Access Objects)
            Console.WriteLine("### START: DAO (write) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Write(strConnection);
            }

            Console.WriteLine("### END: DAO (write) ###\n");
        }

        public override void PerformanceTest(int nLoops)
        {
            // DAO
            Console.WriteLine("### START: DAO - Performance tests ###");
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                {
                    int totalRecordsRead = 0;
                    int startTicks = Environment.TickCount;
                    for (int loop = 0; loop < nLoops; loop++)
                        totalRecordsRead += Connect_PerformanceTest(strConnection);

                    int elapsedTicks = (Environment.TickCount - startTicks);
                    Console.WriteLine("    ({0} cycles: Took {1}ms at an avg of {2:0.00}ms to read an avg of {3:0.0} records)",
                        nLoops,
                        elapsedTicks,
                        (float)elapsedTicks / (float)nLoops,
                        (float)totalRecordsRead / (float)nLoops);
                }
            }

            Console.WriteLine("### END: DAO - Performance tests ###\n");
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            // DAO: Set up the name (or connection string) for the Access database
            bool bHaveConnectionString = true;
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                        strConnection = (m_cfgGeneral.strDevDataPath + "\\Northwind 97.mdb");
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                    }
                    break;

                case MSAccessDbType.eMSAccess2000:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                        strConnection = (m_cfgGeneral.strDevDataPath + "\\Northwind 2000.mdb");
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                    }
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    // 64-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 32-bit)", HelperGetAccessName(false));
                    }
                    else
                        strConnection = (m_cfgGeneral.strDevDataPath + "\\2007-2016");

                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            // Example (Access 97) = C:\Apps\Data\Northwind 97.mdb
            return bHaveConnectionString;
        }

        protected override void Connect_Stats(string strConnection)
        {
            // Generate some statistics about the selected database
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);
            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);

            // Tables
            if (db.TableDefs.Count > 0)
            {
                // Note: Access 97 databases tend to come with 
                Console.WriteLine("    ({0} tables in {1})", db.TableDefs.Count, db.Name);
                foreach (DAO.TableDef td in db.TableDefs)
                {
                    Console.WriteLine("      {0}", td.Name);
                }
            }
            else
                Console.WriteLine("    (There are no tables in {0}!)", db.Name);

            db.Close();
            Console.WriteLine();
        }

        protected override void Connect_Read(string strConnection)
        {
            // Use the DAO::DBEngine to open an Access database and read recordsets

            // Note: On one machine running Windows 10 and Office 365, the DBEngine had these details:
            // * TypeLib = {4AC9E1DA-5BAD-4AC7-86E3-24F4CDCECA28}
            // * Name = Microsoft Office 16.0 Access Database Engine Object Library
            // * Assembly = Microsoft.Office.Interop.Access.Dao, Version=15.0.0.0, Culture=neutral, PublicKeyToken=71E9BCE111E9429C
            // * Path = C:\Program Files\Microsoft Office\root\VFS\ProgramFilesCommonX64\Microsoft Shared\Office16\ACEDAO.DLL
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);

            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            DAO.Recordset rs = db.OpenRecordset(
                m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString()),
                DAO.RecordsetTypeEnum.dbOpenDynaset,
                DAO.RecordsetOptionEnum.dbReadOnly);
            if (!(rs.BOF && rs.EOF))
            {
                // Go through each record in the RecordSet, writing the result to the console window
                int recordsRead = 0;
                Console.WriteLine("\t{0}\t{1}\t{2}",
                    "ProductID", "UnitPrice", "ProductName");

                rs.MoveFirst();
                dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                while (!rs.EOF)
                {
                    recordsRead++;
                    Console.WriteLine("\t{0}\t\t{1:0.0}\t\t{2}",
                        (int)m_utilsDAO.SafeGetFieldValue(rs, "ProductID"),
                        (Decimal)m_utilsDAO.SafeGetFieldValue(rs, "UnitPrice"),
                        m_utilsDAO.SafeGetFieldValue(rs, "ProductName"));
                    rs.MoveNext();
                    dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                }
                rs.Close();
                Console.WriteLine("    ({0} records)", recordsRead);
            }

            db.Close();
            Console.WriteLine();
        }

        protected override void Connect_Write(string strConnection)
        {
            // Use the DAO::DBEngine to open an Access database and write recordsets
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);

            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            string strQuery = m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString());

            Console.Write("Open database read-only: ");
            DAO.Recordset rs = db.OpenRecordset(
                strQuery,
                DAO.RecordsetTypeEnum.dbOpenDynaset,
                DAO.RecordsetOptionEnum.dbReadOnly);
            if (!(rs.BOF && rs.EOF))
            {
                Console.WriteLine(m_utilsDAO.IsRecordUpdateable(rs));
                rs.Close();
            }

            Console.Write("Open database writeable: ");
            rs = db.OpenRecordset(
                strQuery,
                DAO.RecordsetTypeEnum.dbOpenDynaset);
            if (!(rs.BOF && rs.EOF))
            {
                Console.WriteLine(m_utilsDAO.IsRecordUpdateable(rs));
                Console.WriteLine();

                // Now go through all records and check various properties
                int recordsRead = 0;
                Console.WriteLine("  (Using the \"ProductName\" field as an example)");
                Console.WriteLine(
                    "#\tRequired\tValidateOnSet\tValidationRule\tValidationText\tSize\tValue");
                DAO.Field fd;
                rs.MoveFirst();
                while (!rs.EOF)
                {
                    recordsRead++;
                    fd = m_utilsDAO.SafeGetField(rs, "ProductName");
                    if (fd != null)
                    {
                        Console.WriteLine("{0}\t{1}\t\t{2}\t\t{3}\t\t{4}\t\t{5}\t{6}",
                            recordsRead,
                            fd.Required,
                            m_utilsDAO.BoolFieldToString(fd.ValidateOnSet),
                            m_utilsDAO.StringFieldToString(fd.ValidationRule),
                            m_utilsDAO.StringFieldToString(fd.ValidationText),
                            fd.Size,
                            fd.Value);
                    }
                    else
                        Console.WriteLine("{0}(record is null)", recordsRead);

                    rs.MoveNext();
                }
                rs.Close();
            }

            db.Close();
            Console.WriteLine();
        }

        protected override int Connect_PerformanceTest(string strConnection)
        {
            // Version for performance testing
            int recordsRead = 0;
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);

            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            DAO.Recordset rs = db.OpenRecordset(
                m_cfgDatabase.strQuery.Replace("?", m_cfgDatabase.paramValue.ToString()),
                DAO.RecordsetTypeEnum.dbOpenDynaset,
                DAO.RecordsetOptionEnum.dbReadOnly);
            if (!(rs.BOF && rs.EOF))
            {
                // Go through each record in the RecordSet; for this performance version just count
                // the number of records read
                rs.MoveFirst();
                dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                while (!rs.EOF)
                {
                    recordsRead++;
                    rs.MoveNext();
                    dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                }
                rs.Close();
            }

            db.Close();
            return recordsRead;
        }
        #endregion // Abstract methods from the base class

        #region Methods unique to this class
        public void DummyOpenClose()
        {
            Console.WriteLine("    (Dummy open and close databases using DAO)");
            int startTicks = Environment.TickCount;
            string strConnection = string.Empty;
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            DAO.Database db = null;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                {
                    db = dbEngine.OpenDatabase(strConnection, false, false);
                    db.Close();
                }
            }

            int elapsedTicks = (Environment.TickCount - startTicks);
            Console.WriteLine("    (Completed dummy open/close. Took {0}ms.)", elapsedTicks);
        }
        #endregion // Methods unique to this class
    }
}
