using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Data.OleDb;

using System.Windows.Forms;

namespace AccessLoginApp_MDB
{
    public partial class frmUserEntry : Form
    {
        #region Member variables
        private bool m_error = false;
        private UInt32 m_errorID = CommonDefs.fieldNone;
        private CommonDefs.FieldError m_errorCode = CommonDefs.FieldError.fieldError_None;

        private OleDbConnection m_connection = new OleDbConnection();
        private string m_connectionString =
            @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\AccessLogin.mdb;User Id=admin;Password=;";
        #endregion // Member variables

        public frmUserEntry(string connectionString)
        {
            InitializeComponent();
            m_connectionString = connectionString;
        }

        private void btnSaveUser_Click(object sender, EventArgs e)
        {
            // Ensure the entered data is valid before continuing
            if (ValidateData())
            {
                try
                {
                    m_error = false;
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();
                    OleDbCommand command = new OleDbCommand();
                    command.Connection = m_connection;
                    command.CommandText = (
                        "INSERT INTO EmployeeData (FirstName,LastName,Pay) " +
                        "VALUES (" + 
                            "'" + txtFirstName.Text + "'," +
                            "'" + txtLastName.Text + "'," +
                            "'" + txtPay.Text + "')");
                    command.ExecuteNonQuery();

                    m_connection.Close();
                    m_connection.Dispose();
                }
                catch
                {
                    m_error = true;
                    lblStatus.Text = "Exception!";
                }
            }
            else
            {
                MessageBox.Show(GetInvalidMessage(), "Error!");
            }
        }

        private bool ValidateData()
        {
            // Validate user entry
            m_error = false;
            m_errorID = CommonDefs.fieldNone;
            m_errorCode = CommonDefs.FieldError.fieldError_None;

            // Check for blank entries
            if (string.IsNullOrEmpty(txtFirstName.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldFirstName;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }

            if (string.IsNullOrEmpty(txtLastName.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldLastName;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }

            if (string.IsNullOrEmpty(txtPay.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldPay;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }

            if (!m_error)
            {
                // Check for length issues
                if (txtFirstName.Text.Length > 20)
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldFirstName;
                    m_errorCode = CommonDefs.FieldError.fieldError_Length;
                }

                if (txtLastName.Text.Length > 50)
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldLastName;
                    m_errorCode = CommonDefs.FieldError.fieldError_Length;
                }
            }

            if (!m_error)
            {
                // Check for a duplicate combination of (FirstName,  LastName)
                try
                {
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();
                    OleDbCommand command = new OleDbCommand();
                    command.Connection = m_connection;
                    command.CommandText = (
                        "SELECT * FROM EmployeeData WHERE " +
                        "FirstName='" + txtFirstName.Text + "' AND " +
                        "LastName='" + txtLastName.Text + "'");
                    OleDbDataReader reader = command.ExecuteReader();
                    if (reader.HasRows)
                    {
                        m_error = true;
                        m_errorID |= CommonDefs.fieldFirstName;
                        m_errorCode = CommonDefs.FieldError.fieldError_DuplicateName;
                    }

                    m_connection.Close();
                    m_connection.Dispose();
                }
                catch { }
            }

            if (!m_error)
            {
                // Check for valid pay
                Decimal pay = 0.0m;
                if (!Decimal.TryParse(txtPay.Text, out pay))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldPay;
                    m_errorCode = CommonDefs.FieldError.fieldError_InvalidPay;
                }
            }

            return (!m_error);
        }

        private string GetInvalidMessage()
        {
            // Validate user entry
            string errorMsg = string.Empty;
            if ((m_errorID & CommonDefs.fieldFirstName) != 0)
                errorMsg = string.Format("First Name: {0}", GetInvalidDataReason(m_errorCode));

            if ((m_errorID & CommonDefs.fieldLastName) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Last Name: {0}", GetInvalidDataReason(m_errorCode));
            }

            if ((m_errorID & CommonDefs.fieldDOB) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Date of Birth: {0}", GetInvalidDataReason(m_errorCode));
            }

            if ((m_errorID & CommonDefs.fieldCountry) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Country: {0}", GetInvalidDataReason(m_errorCode));
            }

            if ((m_errorID & CommonDefs.fieldPhone) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Phone: {0}", GetInvalidDataReason(m_errorCode));
            }

            if ((m_errorID & CommonDefs.fieldPay) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Pay: {0}", GetInvalidDataReason(m_errorCode));
            }
            
            if ((m_errorID & CommonDefs.fieldUsername) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Username: {0}", GetInvalidDataReason(m_errorCode));
            }

            if ((m_errorID & CommonDefs.fieldPassword) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("Password: {0}", GetInvalidDataReason(m_errorCode));
            }

            return (errorMsg);
        }

        private string GetInvalidDataReason(CommonDefs.FieldError error)
        {
            string reason = string.Empty;
            switch (error)
            {
                case CommonDefs.FieldError.fieldError_Blank:
                    reason = "Blank";
                    break;

                case CommonDefs.FieldError.fieldError_Length:
                    reason = "Length too long";
                    break;

                case CommonDefs.FieldError.fieldError_DuplicateName:
                    reason = "Duplicate First and Last name";
                    break;

                case CommonDefs.FieldError.fieldError_DuplicateUsername:
                    reason = "User already exists";
                    break;

                case CommonDefs.FieldError.fieldError_InvalidDOB:
                case CommonDefs.FieldError.fieldError_InvalidPay:
                    reason = "Invalid format";
                    break;

                default:
                    reason = "Unknown";
                    break;
            }

            return reason;
        }
    }
}
