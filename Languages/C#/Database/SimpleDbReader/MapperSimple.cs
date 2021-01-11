using System;
using System.Collections.ObjectModel;
using System.Data;

namespace SimpleDbReader
{
    // Database structure(s) and mapper(s) for the SimpleTest.mdb database

    #region Member table
    public class SimpleMember
    {
        // Member table

        #region Constants
        // Default values when a nullable entry is blank or the SQL query did not return the value
        public readonly int cDefaultMemberID = -1;
        public readonly string cDefaultSurname = string.Empty;
        public readonly string cDefaultFirstName = string.Empty;
        public readonly DateTime cDefaultDOB = DateTime.MinValue;
        public readonly decimal cDefaultFee = 0.0m;
        public readonly bool cDefaultAccepted = false;
        public readonly int cDefaultPoints = -1;
        #endregion

        #region Fields
        // Fields (columns) in this table
        private int m_memberID;
        private string m_surname;
        private string m_firstName;
        private DateTime m_DOB;
        private decimal m_fee;
        private bool m_accepted;
        private int m_points;
        #endregion // Fields

        // Constructor
        public SimpleMember()
        {
        }

        #region Properties
        public int MemberID
        {
            get { return m_memberID; }
            set { m_memberID = value; }
        }

        public string Surname
        {
            get { return m_surname; }
            set { m_surname = value; }
        }

        public string FirstName
        {
            get { return m_firstName; }
            set { m_firstName = value; }
        }

        public DateTime DOB
        {
            get { return m_DOB; }
            set { m_DOB = value; }
        }

        public decimal Fee
        {
            get { return m_fee; }
            set { m_fee = value; }
        }

        public bool Accepted
        {
            get { return m_accepted; }
            set { m_accepted = value; }
        }

        public int Points
        {
            get { return m_points; }
            set { m_points = value; }
        }
        #endregion // Properties
    }
    #endregion // Member table

    #region Mapper and Reader
    class SimpleMapper_Member : MapperBase<SimpleMember>
    {
        protected override SimpleMember Map(IDataRecord record)
        {
            SimpleMember m = new SimpleMember();
            try
            {
                m.MemberID = (DBNull.Value == record[CommonSimple.colMemberID])
                    ? m.cDefaultMemberID
                    : (int)record[CommonSimple.colMemberID];
            }
            catch { }

            try
            {
                m.Surname = (DBNull.Value == record[CommonSimple.colSurname])
                    ? m.cDefaultSurname
                    : (string)record[CommonSimple.colSurname];
            }
            catch { }

            try
            {
                m.FirstName = (DBNull.Value == record[CommonSimple.colFirstName])
                    ? m.cDefaultFirstName
                    : (string)record[CommonSimple.colFirstName];
            }
            catch { }

            try
            {
                m.DOB = (DBNull.Value == record[CommonSimple.colDOB])
                    ? m.cDefaultDOB
                    : (DateTime)record[CommonSimple.colDOB];
            }
            catch { }

            try
            {
                m.Fee = (DBNull.Value == record[CommonSimple.colFee])
                    ? m.cDefaultFee
                    : (decimal)record[CommonSimple.colFee];
            }
            catch { }

            try
            {
                m.Accepted = (DBNull.Value == record[CommonSimple.colAccepted])
                    ? m.Accepted
                    : (bool)record[CommonSimple.colAccepted];
            }
            catch { }

            try
            {
                m.Points = (DBNull.Value == record[CommonSimple.colPoints])
                    ? m.cDefaultPoints
                    : (int)record[CommonSimple.colPoints];
            }
            catch { }

            return m;
        }
    }

    class SimpleReader_Member : ObjectReaderWithConnection<SimpleMember>
    {
        public override DatabaseTechnology DbTechnology
        {
            get { return m_tech; }
            set { m_tech = value; }
        }

        public override string ConnectionString
        {
            get { return m_connectionString; }
            set { m_connectionString = value; }
        }

        public override string CmdText
        {
            //get { return "SELECT * FROM Members"; }
            //get { return "SELECT MemberID,Surname FROM Members"; }
            get { return m_cmdText; }
            set { m_cmdText = value; }
        }

        public override CommandType CmdType
        {
            // Available command types are:
            // * Text (standard SQL query) = 1,
            // * StoredProcedure
            // * TableDirect (direct table access) [appears to be a shortcut to "SELECT * FROM TABLENAME"]
            get { return m_cmdType; }
            set { m_cmdType = value; }
        }

        protected override Collection<IDataParameter> GetParameters(IDbCommand command)
        {
            Collection<IDataParameter> collection = new Collection<IDataParameter>();
            return collection;

            // If you have parameters:
            //IDataParameter param1 = command.CreateParameter();
            //param1.ParameterName = "paramName 1";     // Put the parameter name here
            //param1.Value = 5;                         // Put the parameter value here
            //collection.Add(param1);
            //return collection;   
        }

        protected override MapperBase<SimpleMember> GetMapper()
        {
            MapperBase<SimpleMember> mapper = new SimpleMapper_Member();
            return mapper;
        }
    }
    #endregion // Classes for SQL Server
}
