using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AccessLoginApp_MDB
{
     // Common definitions
    public static class CommonDefs
    {
        #region Fields in the database
        public static readonly UInt32 fieldEmployeeID   = 0x00000001;
        public static readonly UInt32 fieldFirstName    = 0x00000002;
        public static readonly UInt32 fieldLastName     = 0x00000004;
        public static readonly UInt32 fieldDOB          = 0x00000008;
        public static readonly UInt32 fieldCountry      = 0x00000010;
        public static readonly UInt32 fieldPhone        = 0x00000020;
        public static readonly UInt32 fieldPay          = 0x00000040;
        public static readonly UInt32 fieldUsername     = 0x00000080;
        public static readonly UInt32 fieldPassword     = 0x00000100;

        public static readonly UInt32 fieldsNone        = 0x00000000;
        public static readonly UInt32 fieldsAll         = (
            fieldEmployeeID |
            fieldFirstName |
            fieldLastName |
            fieldDOB |
            fieldCountry |
            fieldPhone |
            fieldPay |
            fieldUsername |
            fieldPassword);

        public static readonly string nameEmployeeID   = "EmployeeID";
        public static readonly string nameFirstName    = "FirstName";
        public static readonly string nameLastName     = "LastName";
        public static readonly string nameDOB          = "DOB";
        public static readonly string nameCountry      = "Country";
        public static readonly string namePhone        = "Phone";
        public static readonly string namePay          = "Pay";
        public static readonly string nameUsername     = "Username";
        public static readonly string namePassword     = "Password";
        #endregion // Fields in the database

        // Enumerations
        public enum FieldError
        {
            // Validation errors on new user entry
            fieldError_None,
            fieldError_Blank,               // Generic
            fieldError_Length,              // Generic (mostly applies to Text/VarChar fields)
            fieldError_Invalid,             // Ill-formed or invalid (eg alphabetic characters in number)
            fieldError_OutOfRange,          // Value too high or too low
            fieldError_IdUnknown,           // Unknown user (for UPDATE and DELETE)
            fieldError_DuplicateName,       // FirstName, LastName already exists
            fieldError_DuplicateUsername    // Username already exists
        };
    };

    public class EmployeeData
    {
        // Properties
        public int EmployeeID { get; set; }
        public string FirstName { get; set; }
        public string LastName { get; set; }
        public DateTime DOB { get; set; }
        public string Country { get; set; }
        public string Phone { get; set; }
        public decimal Pay { get; set; }
        public string Username { get; set; }
        public string Password { get; set; }

        // Class to represent an employee
        public EmployeeData()
        {
            EmployeeID = -1;
            FirstName = string.Empty;
            LastName = string.Empty;
            DOB = DateTime.Now;
            Country = string.Empty;
            Phone = string.Empty;
            Pay = 0.0m;
            Username = string.Empty;
            Password = string.Empty;
        }
    };
}
