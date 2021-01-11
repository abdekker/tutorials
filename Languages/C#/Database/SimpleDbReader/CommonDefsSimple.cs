using System;

namespace SimpleDbReader
{
    // Definitions for the SimpleTest.mdb database
    public struct CommonSimpleMemberRecord
    {
        // Helper structure for records from the Member table
        public int MemberID;
        public string Surname;
        public string FirstName;
        public DateTime DOB;
        public decimal Fee;
        public bool Accepted;
        public int Points;

        // Constructor
        public static void DefaultRecord(ref CommonSimpleMemberRecord rs)
        {
            // Default this record
            rs.MemberID = -1;
            rs.Surname = "(surname)";
            rs.FirstName = "(first)";
            rs.DOB = DateTime.MinValue;
            rs.Fee = 0.0m;
            rs.Accepted = false;
            rs.Points = 0;
        }

        public static string GetRecordHeader()
        {
            // Helper method to print a table header to the console
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}",
                CommonSimple.colMemberID.PadRight(CommonSimple.colWidthMemberID),
                CommonSimple.colSurname.PadRight(CommonSimple.colWidthSurname),
                CommonSimple.colFirstName.PadRight(CommonSimple.colWidthFirstName),
                CommonSimple.colDOB.PadRight(CommonSimple.colWidthDOB),
                CommonSimple.colFee.PadRight(CommonSimple.colWidthFee),
                CommonSimple.colAccepted.PadRight(CommonSimple.colWidthAccepted),
                CommonSimple.colPoints);
        }

        public static string GetRecordAsString(in CommonSimpleMemberRecord rs)
        {
            // Helper method to format a record for printing to the console
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}",
                rs.MemberID.ToString().PadRight(CommonSimple.colWidthMemberID),
                rs.Surname.PadRight(CommonSimple.colWidthSurname),
                rs.FirstName.PadRight(CommonSimple.colWidthFirstName),
                rs.DOB.ToString(DatabaseCommon.cszDateISO8601).PadRight(CommonSimple.colWidthDOB),
                rs.Fee.ToString("0.00").PadRight(CommonSimple.colWidthFee),
                rs.Accepted.ToString().PadRight(CommonSimple.colWidthAccepted),
                rs.Points);
        }

        public static string GetSimpleMemberAsString(in SimpleMember rs)
        {
            // Helper method to format a row for printing to the console
            // Note: This version actually takes another class (SimpleMember)
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}",
                rs.MemberID.ToString().PadRight(CommonSimple.colWidthMemberID),
                rs.Surname.PadRight(CommonSimple.colWidthSurname),
                rs.FirstName.PadRight(CommonSimple.colWidthFirstName),
                rs.DOB.ToString(DatabaseCommon.cszDateISO8601).PadRight(CommonSimple.colWidthDOB),
                rs.Fee.ToString("0.00").PadRight(CommonSimple.colWidthFee),
                rs.Accepted.ToString().PadRight(CommonSimple.colWidthAccepted),
                rs.Points);
        }
    }

    class CommonSimple
    {
        #region Fields (columns) in the Member table
        public static readonly string colMemberID     = "MemberID";
        public static readonly string colSurname      = "Surname";
        public static readonly string colFirstName    = "FirstName";
        public static readonly string colDOB          = "DOB";
        public static readonly string colFee          = "Fee";
        public static readonly string colAccepted     = "Accepted";
        public static readonly string colPoints       = "Points";

        // Display width
        public static readonly int colWidthMemberID    = 10;
        public static readonly int colWidthSurname     = 20;
        public static readonly int colWidthFirstName   = 15;
        public static readonly int colWidthDOB         = 15;
        public static readonly int colWidthFee         = 9;
        public static readonly int colWidthAccepted    = 12;
        public static readonly int colWidthPoints      = 10;
        #endregion // Fields (columns) in the Member table
    };
}
