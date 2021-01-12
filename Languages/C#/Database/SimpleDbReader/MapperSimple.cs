using System;
using System.Collections.ObjectModel;
using System.Data;

namespace SimpleDbReader
{
    // Database structure(s) and mapper(s) for the SimpleTest.mdb database

    #region Tables

    #region Members table
    public class Simple_Members : IRecordBase
    {
        // Class to hold records from the Members table

        #region Constants
        // Columns (fields)
        public static readonly string colMemberID = "MemberID";
        public static readonly string colSurname = "Surname";
        public static readonly string colFirstName = "FirstName";
        public static readonly string colDOB = "DOB";
        public static readonly string colFee = "Fee";
        public static readonly string colAccepted = "Accepted";
        public static readonly string colPoints = "Points";

        // Display width
        public static readonly int colMemberIDWidth = 10;
        public static readonly int colSurnameWidth = 20;
        public static readonly int colFirstNameWidth = 15;
        public static readonly int colDOBWidth = 15;
        public static readonly int colFeeWidth = 9;
        public static readonly int colAcceptedWidth = 12;
        public static readonly int colPointsWidth = 10;

        // Default values when a nullable entry is blank or the SQL query did not return the value
        public static readonly int cDefaultMemberID = -1;
        public static readonly string cDefaultSurname = string.Empty;
        public static readonly string cDefaultFirstName = string.Empty;
        public static readonly DateTime cDefaultDOB = DateTime.MinValue;
        public static readonly decimal cDefaultFee = 0.0m;
        public static readonly bool cDefaultAccepted = false;
        public static readonly int cDefaultPoints = -1;
        #endregion // Constants

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

        // Constructor
        public Simple_Members() { }

        #region Helper methods
        public void DefaultRecord()
        {
            // Default this record
            this.MemberID = cDefaultMemberID;
            this.Surname = cDefaultSurname;
            this.FirstName = cDefaultFirstName;
            this.DOB = cDefaultDOB;
            this.Fee = cDefaultFee;
            this.Accepted = cDefaultAccepted;
            this.Points = cDefaultPoints;
        }

        public string GetRecordHeader()
        {
            // Helper method to print a table header to the console
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}",
                colMemberID.PadRight(colMemberIDWidth),
                colSurname.PadRight(colSurnameWidth),
                colFirstName.PadRight(colFirstNameWidth),
                colDOB.PadRight(colDOBWidth),
                colFee.PadRight(colFeeWidth),
                colAccepted.PadRight(colAcceptedWidth),
                colPoints);
        }

        public string GetRecordAsString()
        {
            // Helper method to format a record for printing to the console
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}",
                this.MemberID.ToString().PadRight(colMemberIDWidth),
                this.Surname.PadRight(colSurnameWidth),
                this.FirstName.PadRight(colFirstNameWidth),
                this.DOB.ToString(DatabaseCommon.cszDateISO8601).PadRight(colDOBWidth),
                this.Fee.ToString("0.00").PadRight(colFeeWidth),
                this.Accepted.ToString().PadRight(colAcceptedWidth),
                this.Points);
        }
        #endregion // Helper methods
    }
    #endregion // Members table
    #endregion // Tables

    #region Mappers, Readers
    class SimpleMapper_Members : MapperBase<Simple_Members>
    {
        protected override Simple_Members Map(IDataRecord record)
        {
            Simple_Members m = new Simple_Members();
            m.DefaultRecord();
            try
            {
                m.MemberID = (DBNull.Value == record[Simple_Members.colMemberID])
                    ? Simple_Members.cDefaultMemberID
                    : (int)record[Simple_Members.colMemberID];
            }
            catch { }

            try
            {
                m.Surname = (DBNull.Value == record[Simple_Members.colSurname])
                    ? Simple_Members.cDefaultSurname
                    : (string)record[Simple_Members.colSurname];
            }
            catch { }

            try
            {
                m.FirstName = (DBNull.Value == record[Simple_Members.colFirstName])
                    ? Simple_Members.cDefaultFirstName
                    : (string)record[Simple_Members.colFirstName];
            }
            catch { }

            try
            {
                m.DOB = (DBNull.Value == record[Simple_Members.colDOB])
                    ? Simple_Members.cDefaultDOB
                    : (DateTime)record[Simple_Members.colDOB];
            }
            catch { }

            try
            {
                m.Fee = (DBNull.Value == record[Simple_Members.colFee])
                    ? Simple_Members.cDefaultFee
                    : (decimal)record[Simple_Members.colFee];
            }
            catch { }

            try
            {
                m.Accepted = (DBNull.Value == record[Simple_Members.colAccepted])
                    ? Simple_Members.cDefaultAccepted
                    : (bool)record[Simple_Members.colAccepted];
            }
            catch { }

            try
            {
                m.Points = (DBNull.Value == record[Simple_Members.colPoints])
                    ? Simple_Members.cDefaultPoints
                    : (int)record[Simple_Members.colPoints];
            }
            catch { }

            return m;
        }
    }

    class SimpleReader_Members : ObjectReaderWithConnection<Simple_Members>
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

        protected override MapperBase<Simple_Members> GetMapper()
        {
            MapperBase<Simple_Members> mapper = new SimpleMapper_Members();
            return mapper;
        }
    }
    #endregion // Mappers, Readers
}
