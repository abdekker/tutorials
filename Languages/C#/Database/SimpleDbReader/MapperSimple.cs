using System;
using System.Collections.ObjectModel;
using System.Data;

namespace SimpleDbReader
{
    // Database structure and mapper(s) for the SimpleTest.mdb database
    class SimpleMember
    {
        #region Fields
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

    #region Classes for SQL Server
    class SimpleMapperSqlServer : MapperBaseSqlServer<SimpleMember>
    {
        // Mapper for SQL Server
        protected override SimpleMember Map(IDataRecord record)
        {
            SimpleMember m = new SimpleMember();
            try
            {
                m.MemberID = (DBNull.Value == record[CommonSimple.colMemberID]) ?
                    -1 : (int)record[CommonSimple.colMemberID];
                m.Surname = (DBNull.Value == record[CommonSimple.colSurname]) ?
                    string.Empty : (string)record[CommonSimple.colSurname];
                m.FirstName = (DBNull.Value == record[CommonSimple.colFirstName]) ?
                    string.Empty : (string)record[CommonSimple.colFirstName];
                m.DOB = (DBNull.Value == record[CommonSimple.colDOB]) ?
                    DateTime.MinValue : (DateTime)record[CommonSimple.colDOB];
                m.Fee = (DBNull.Value == record[CommonSimple.colFee]) ?
                    0.0m : (decimal)record[CommonSimple.colFee];
                m.Accepted = (DBNull.Value == record[CommonSimple.colAccepted]) ?
                    false : (bool)record[CommonSimple.colAccepted];
                m.Points = (DBNull.Value == record[CommonSimple.colPoints]) ?
                    -1 : (int)record[CommonSimple.colPoints];
            }
            catch
            {
                //throw;
            }

            return m;
        }
    }

    class SimpleMemberReaderSqlServer : ObjectReaderWithConnectionSqlServer<SimpleMember>
    {
        // Reader for SQL Server
        protected override string CommandText
        {
            get { return "SELECT * FROM Members"; }
        }

        protected override CommandType CommandType
        {
            get { return CommandType.Text; }
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

        protected override MapperBaseSqlServer<SimpleMember> GetMapper()
        {
            MapperBaseSqlServer<SimpleMember> mapper = new SimpleMapperSqlServer();
            return mapper;
        }
    }
    #endregion // Classes for SQL Server

    #region Classes for ODBC
    class SimpleMapperODBC : MapperBaseODBC<SimpleMember>
    {
        // Mapper for ODBC
        protected override SimpleMember Map(IDataRecord record)
        {
            SimpleMember m = new SimpleMember();
            try
            {
                m.MemberID = (DBNull.Value == record[CommonSimple.colMemberID]) ?
                    -1 : (int)record[CommonSimple.colMemberID];
                m.Surname = (DBNull.Value == record[CommonSimple.colSurname]) ?
                    string.Empty : (string)record[CommonSimple.colSurname];
                m.FirstName = (DBNull.Value == record[CommonSimple.colFirstName]) ?
                    string.Empty : (string)record[CommonSimple.colFirstName];
                m.DOB = (DBNull.Value == record[CommonSimple.colDOB]) ?
                    DateTime.MinValue : (DateTime)record[CommonSimple.colDOB];
                m.Fee = (DBNull.Value == record[CommonSimple.colFee]) ?
                    0.0m : (decimal)record[CommonSimple.colFee];
                m.Accepted = (DBNull.Value == record[CommonSimple.colAccepted]) ?
                    false : (bool)record[CommonSimple.colAccepted];
                m.Points = (DBNull.Value == record[CommonSimple.colPoints]) ?
                    -1 : (int)record[CommonSimple.colPoints];
            }
            catch
            {
                //throw;
            }

            return m;
        }
    }

    class SimpleMemberReaderODBC : ObjectReaderWithConnectionODBC<SimpleMember>
    {
        // Reader for ODBC
        protected override string CmdText
        {
            get { return "SELECT * FROM Members"; }
        }

        protected override CommandType CmdType
        {
            get { return CommandType.Text; }
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

        protected override MapperBaseODBC<SimpleMember> GetMapper()
        {
            MapperBaseODBC<SimpleMember> mapper = new SimpleMapperODBC();
            return mapper;
        }
    }
    #endregion // Classes for SQL Server
}
