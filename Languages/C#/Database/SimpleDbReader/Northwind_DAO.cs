﻿using System;
using System.Collections.Generic;

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
            m_cfgDatabase.dbTech = DatabaseTechnology.eDB_DAO;

            // Set the main database query
            m_cfgDatabase.querySELECT = HelperGetQuerySELECT();
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
            Console.WriteLine("### START: DAO (read, Northwind) ###");

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

        public override void Writeable()
        {
            // DAO (Data Access Objects)
            Console.WriteLine("### START: DAO (writeable, Northwind) ###");

            // See the class constructor for details on databases
            string strConnection = string.Empty;
            foreach (MSAccessDbType dbType in Enum.GetValues(typeof(MSAccessDbType)))
            {
                m_cfgDatabase.dbType = dbType;
                Console.WriteLine("  Testing: {0}", HelperGetAccessName(true));
                if (SetConnectionString(ref strConnection))
                    Connect_Writeable(strConnection);
            }

            Console.WriteLine("### END: DAO (writeable) ###\n");
        }

        public override void Insert()
        {
            // TODO
        }

        public override void Update()
        {
            // TODO
        }

        public override void Delete()
        {
            // TODO
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
                case MSAccessDbType.eMSAccess2000:
                    // 32-bit only
                    if (m_cfgGeneral.b64bit)
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
                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            if (bHaveConnectionString)
                strConnection = (m_cfgGeneral.strDevDataPath + "\\" +
                    m_utilsDAO.GetConnectionDetailsFilename(m_cfgDatabase.dbType));

            // Example (Access 97) = C:\Apps\Data\Northwind 97.mdb
            return bHaveConnectionString;
        }

        protected override void Connect_Stats(string strConnection)
        {
            // Generate some statistics about the selected database
            // Tables and fields
            string dbName = m_utilsDAO.GetDbName(strConnection);
            List<string> tables = m_utilsDAO.GetTables(strConnection, true);
            List<string> fields;
            if (tables.Count > 0)
            {
                Console.WriteLine("    ({0} tables in {1})", tables.Count, dbName);
                foreach (string tb in tables)
                {
                    Console.WriteLine("      {0}", tb);
                    fields = m_utilsDAO.GetFields(strConnection, tb);
                    if (fields.Count > 0)
                    {
                        Console.WriteLine("        ({0} fields in {1})", fields.Count, tb);
                        foreach (string fd in fields)
                        {
                            Console.WriteLine("        {0}", fd);
                        }
                    }
                    else
                        Console.WriteLine("        (no columns in this table)");
                }
            }
            else
                Console.WriteLine("    (not tables in {0}", dbName);

            // To get a list of just the columns (in a specific table)
            //  List<string> columns = m_utilsDAO.GetFields(strConnection, tables[0]);
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
                m_cfgDatabase.querySELECT.Replace("?", m_cfgDatabase.paramValue.ToString()),
                DAO.RecordsetTypeEnum.dbOpenDynaset,
                DAO.RecordsetOptionEnum.dbReadOnly);
            if (!(rs.BOF && rs.EOF))
            {
                // Go through each record in the RecordSet, writing the result to the console window
                int recordsRead = 0;
                Console.WriteLine("\t{0}{1}{2}",
                        Northwind_Products.colProductID.PadRight(Northwind_Products.colProductIDWidth),
                        Northwind_Products.colUnitPrice.PadRight(Northwind_Products.colUnitPriceWidth),
                        Northwind_Products.colProductName);

                rs.MoveFirst();
                dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                while (!rs.EOF)
                {
                    recordsRead++;
                    Console.WriteLine("\t{0}{1}{2}",
                        ((int)m_utilsDAO.SafeGetFieldValue(rs, Northwind_Products.colProductID)).ToString().PadRight(Northwind_Products.colProductIDWidth),
                        ((decimal)m_utilsDAO.SafeGetFieldValue(rs, Northwind_Products.colUnitPrice)).ToString("0.00").PadRight(Northwind_Products.colUnitPriceWidth),
                        (m_utilsDAO.SafeGetFieldValue(rs, Northwind_Products.colProductName)).ToString());
                    rs.MoveNext();
                    dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                }
                rs.Close();
                Console.WriteLine("    ({0} records)", recordsRead);
            }

            db.Close();
            Console.WriteLine();
        }

        protected override void Connect_Writeable(string strConnection)
        {
            // Use the DAO::DBEngine to open an Access database and write recordsets
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);

            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            string strQuery = m_cfgDatabase.querySELECT.Replace("?", m_cfgDatabase.paramValue.ToString());

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

        protected override void Connect_Insert(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Update(string strConnection)
        {
            // TODO
        }

        protected override void Connect_Delete(string strConnection)
        {
            // TODO
        }

        protected override int Connect_PerformanceTest(string strConnection)
        {
            // Version for performance testing
            int recordsRead = 0;
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);

            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            DAO.Recordset rs = db.OpenRecordset(
                m_cfgDatabase.querySELECT.Replace("?", m_cfgDatabase.paramValue.ToString()),
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
