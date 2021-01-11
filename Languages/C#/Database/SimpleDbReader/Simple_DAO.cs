using System;
using System.Collections.ObjectModel;

namespace SimpleDbReader
{
    class Simple_DAO : DatabaseCommon
    {
        // Member variables unique to this class
        private Utilities_DAO m_utilsDAO = new Utilities_DAO();

        // Constructor
        public Simple_DAO(ConfigGeneral cfgGeneral, ConfigDatabase cfgDatabase) :
            base(cfgGeneral, cfgDatabase)
        {
            // This class uses DAO
            m_tech = DatabaseTechnology.eDB_DAO;

            // Set the main database query
            m_cfgDatabase.queryType = QueryType.eQueryStd2;
            m_cfgDatabase.strQuery = HelperGetQueryString();
        }

        #region Abstract methods from the base class
        public override void GetStats()
        {
            // TODO
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
            // TODO
        }

        public override void PerformanceTest(int nLoops)
        {
            // TODO
        }

        protected override void Connect_Stats(string strConnection)
        {
            // TODO
        }

        public override bool SetConnectionString(ref string strConnection)
        {
            // DAO: Set up the name (or connection string) for the simple Access database
            // Note: This simple database was created manually in MDB Viewer Plus
            bool bHaveConnectionString = true;
            switch (m_cfgDatabase.dbType)
            {
                case MSAccessDbType.eMSAccess97:
                case MSAccessDbType.eMSAccess2000:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                        strConnection = (m_cfgGeneral.strDevDataPath + "\\SimpleTest.mdb");
                    else
                        bHaveConnectionString = false;
                    break;

                case MSAccessDbType.eMSAccess2007_2016:
                    // 64-bit only
                    if (!m_cfgGeneral.b64bit)
                        bHaveConnectionString = false;
                    else
                        strConnection = (m_cfgGeneral.strDevDataPath + "\\SimpleTest.mdb");

                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }

            return bHaveConnectionString;
        }

        protected override void Connect_Read(string strConnection)
        {
            /*SimpleMemberReaderSqlServer reader = new SimpleMemberReaderSqlServer();
            SimpleMemberReaderODBC reader = new SimpleMemberReaderODBC();
            Collection<SimpleMember> members = reader.Execute();
            foreach (SimpleMember m in members)
                Console.WriteLine(string.Format("{0}, {1}: {2}",
                    m.MemberID,
                    m.Surname,
                    m.FirstName,
                    m.DOB,
                    m.Fee,
                    m.Accepted,
                    m.Points));
            Console.ReadLine();*/

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
                m_cfgDatabase.strQuery,
                DAO.RecordsetTypeEnum.dbOpenDynaset,
                DAO.RecordsetOptionEnum.dbReadOnly);
            if (!(rs.BOF && rs.EOF))
            {
                // Go through each record in the RecordSet, writing the result to the console window
                int recordsRead = 0;
                Console.WriteLine("\t{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}",
                    CommonSimple.colMemberID,
                    CommonSimple.colSurname,
                    CommonSimple.colFirstName,
                    CommonSimple.colDOB,
                    CommonSimple.colFee,
                    CommonSimple.colAccepted,
                    CommonSimple.colPoints);

                string[] s = new string[10];
                object obj;
                DateTime dt;
                rs.MoveFirst();
                dbEngine.Idle(DAO.IdleEnum.dbFreeLocks);
                while (!rs.EOF)
                {
                    recordsRead++;
                    try
                    {
                        s[0] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colMemberID).ToString();
                        s[1] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colSurname).ToString();
                        s[2] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colFirstName).ToString();
                        s[3] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colDOB).ToString();
                        obj = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colDOB);
                        dt = (DateTime)m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colDOB);
                        s[4] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colFee).ToString();
                        s[5] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colAccepted).ToString();
                        s[6] = m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colPoints).ToString();

                        Console.WriteLine("\t{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}",
                            m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colMemberID),
                            m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colSurname),
                            m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colFirstName),
                            ((DateTime)m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colDOB)).ToString(cszDateISO8601),
                            (Decimal)m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colFee),
                            (bool)m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colAccepted),
                            (int)m_utilsDAO.HelperSafeGetFieldValue(rs, CommonSimple.colPoints));
                    }
                    catch { }
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
            // TODO
        }

        protected override int Connect_PerformanceTest(string strConnection)
        {
            // TODO
            return 0;
        }
        #endregion // Abstract methods from the base class
    }
}
