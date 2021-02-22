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
        #region Constants
        private readonly string actionNewUser = "New";
        private readonly string actionEditUser = "Edit";
        private readonly string actionDeleteUser = "Delete";

        private readonly string buttonTextNewUser = "Add New";
        private readonly string buttonTextEditUser = "Edit Existing";
        private readonly string buttonTextDeleteUser = "Delete Existing";
        #endregion // Constants

        #region Member variables
        private bool m_error = false;
        private UInt32 m_errorID = CommonDefs.fieldsNone;
        private CommonDefs.FieldError m_errorCode = CommonDefs.FieldError.fieldError_None;

        private UInt32 m_editFields = CommonDefs.fieldsAll;

        private OleDbConnection m_connection = new OleDbConnection();
        private string m_connectionString =
            @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\AccessLogin.mdb;User Id=admin;Password=;";
        #endregion // Member variables

        // Constructor
        public frmUserEntry(string connectionString)
        {
            // Initialise form
            InitializeComponent();

            // Set the connection string
            m_connectionString = connectionString;

            // Populate the available actions
            ddlAction.Items.Clear();
            ddlAction.Items.Add(actionNewUser);
            ddlAction.Items.Add(actionEditUser);
            ddlAction.Items.Add(actionDeleteUser);
            ddlAction.SelectedIndex = 0;
        }

        #region User Events
        private void ddlAction_SelectedIndexChanged(object sender, EventArgs e)
        {
            // Update the button text
            if (ddlAction.Text.Equals(actionNewUser))
            {
                btnUpdate.Text = buttonTextNewUser;
                txtEmployeeID.Enabled = false;
                txtEmployeeID.Text = string.Empty;
            }
            else if (ddlAction.Text.Equals(actionEditUser))
            {
                btnUpdate.Text = buttonTextEditUser;
                txtEmployeeID.Enabled = true;
            }
            else if (ddlAction.Text.Equals(actionDeleteUser))
            {
                btnUpdate.Text = buttonTextDeleteUser;
                txtEmployeeID.Enabled = true;
            }
        }

        private void btnUpdate_Click(object sender, EventArgs e)
        {
            // Determine the action to take
            if (ddlAction.Text.Equals(actionNewUser))
                ActionAddNewUser();
            else if (ddlAction.Text.Equals(actionEditUser))
                ActionEditExistingUser();
            else if (ddlAction.Text.Equals(actionDeleteUser))
                ActionDeleteExistingUser();
        }
        #endregion // User Events

        #region Private methods
        private void ActionAddNewUser()
        {
            // Adding a new user. Ensure the entered data is valid before continuing.
            if (ValidateDataNewUser())
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
                MessageBox.Show(GetInvalidMessage(), "Error!");
        }

        private void ActionEditExistingUser()
        {
            // Editing an existing user. Ensure the entered data is valid before continuing.
            if (ValidateDataEditExisting())
            {
                // Data is valid. Update the existing user based on Employee ID.
                try
                {
                    m_error = false;
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();
                    OleDbCommand command = new OleDbCommand();
                    command.Connection = m_connection;
                    command.CommandText = (
                        "UPDATE EmployeeData SET " +
                        "FirstName='" + "'" + txtFirstName.Text + "'," +
                        "LastName='" + "'" + txtFirstName.Text + "'," +

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
                MessageBox.Show(GetInvalidMessage(), "Error!");
        }

        private void ActionDeleteExistingUser()
        {
        }

        private bool ValidateDataNewUser()
        {
            // Validate user entry (for adding a new user)
            m_error = false;
            m_errorID = CommonDefs.fieldsNone;
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
                // Check for a duplicate combination of (FirstName,LastName)
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

        private bool ValidateDataEditExisting()
        {
            // Validate user entry (for editing an existing user)
            m_error = false;
            m_errorID = CommonDefs.fieldsNone;
            m_errorCode = CommonDefs.FieldError.fieldError_None;

            m_editFields = CommonDefs.fieldsAll;

            // Check for blank entries
            if (string.IsNullOrEmpty(txtFirstName.Text))
                m_editFields &= ~(CommonDefs.fieldFirstName);

            if (string.IsNullOrEmpty(txtLastName.Text))
                m_editFields &= ~(CommonDefs.fieldLastName);

            if (string.IsNullOrEmpty(txtPay.Text))
                m_editFields &= ~(CommonDefs.fieldPay);

            if ((string.IsNullOrEmpty(txtFirstName.Text)) &&
                (string.IsNullOrEmpty(txtLastName.Text)) &&
                (string.IsNullOrEmpty(txtPay.Text)))
            {
                m_error = true;
                m_errorID |= (
                    CommonDefs.fieldFirstName &
                    CommonDefs.fieldLastName &
                    CommonDefs.fieldPay);
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }

            if (!m_error)
            {
                // Check for length issues
                if (txtFirstName.Text.Length > 20)
                    m_editFields &= ~(CommonDefs.fieldFirstName);

                if (txtLastName.Text.Length > 50)
                    m_editFields &= ~(CommonDefs.fieldLastName);

                if ((txtFirstName.Text.Length > 20) &&
                    (txtLastName.Text.Length > 50))
                {
                    m_error = true;
                    m_errorID |= (
                        CommonDefs.fieldFirstName &
                        CommonDefs.fieldLastName);
                    m_errorCode = CommonDefs.FieldError.fieldError_Length;
                }
            }

            if (!m_error)
            {
                // Check for a duplicate combination of (FirstName,LastName)
                if ((!string.IsNullOrEmpty(txtFirstName.Text)) &&
                    (!string.IsNullOrEmpty(txtLastName.Text)))
                {
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
        #endregion // Private methods
    }
}
