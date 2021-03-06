﻿using System;
using System.Data;
using System.Data.Common;
#if UseOleDB
    using System.Data.OleDb;
#elif UseODBC
    using System.Data.Odbc;
#endif

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

        private readonly int employeeIdMin = 0;
        private readonly int employeeIdMax = 999999;

        private readonly int firstNameLengthMax = 20;
        private readonly int lastNameLengthMax = 50;

        private readonly decimal payMin = 0.0m;
        private readonly decimal payMax = 999.99m;
        #endregion // Constants

        #region Member variables
        private DatabaseUtils m_utils = new DatabaseUtils();
        private bool m_error = false;
        private UInt32 m_errorID = CommonDefs.fieldsNone;
        private CommonDefs.FieldError m_errorCode = CommonDefs.FieldError.fieldError_None;

        private EmployeeData m_employee = new EmployeeData();
        private UInt32 m_editFields = CommonDefs.fieldsAll;

        private DbConnection m_connection;
        private string m_connectionString;
        #endregion // Member variables

        // Constructor
        public frmUserEntry(string connectionString)
        {
            // Initialise form
            InitializeComponent();

            // Set up the database connection
            m_connection = m_utils.GetDbConnection();
            m_connectionString = connectionString;
        }

        #region Form Events
        private void frmUserEntry_Load(object sender, EventArgs e)
        {
            // Populate the available actions
            ddlAction.Items.Clear();
            ddlAction.Items.Add(actionNewUser);
            ddlAction.Items.Add(actionEditUser);
            ddlAction.Items.Add(actionDeleteUser);
            ddlAction.SelectedIndex = 0;

            // Show a list of the current users
            RefreshUserList();
            RefreshUserGrid();
        }

        private void ddlAction_SelectedIndexChanged(object sender, EventArgs e)
        {
            // Update the button text
            if (ddlAction.Text.Equals(actionNewUser))
            {
                btnUpdate.Text = buttonTextNewUser;
                txtEmployeeID.Enabled = false;
                txtEmployeeID.Text = string.Empty;
                txtFirstName.Enabled = true;
                txtLastName.Enabled = true;
                txtPay.Enabled = true;
            }
            else if (ddlAction.Text.Equals(actionEditUser))
            {
                btnUpdate.Text = buttonTextEditUser;
                txtEmployeeID.Enabled = true;
                txtFirstName.Enabled = true;
                txtLastName.Enabled = true;
                txtPay.Enabled = true;
            }
            else if (ddlAction.Text.Equals(actionDeleteUser))
            {
                btnUpdate.Text = buttonTextDeleteUser;
                txtEmployeeID.Enabled = true;
                txtFirstName.Enabled = false;
                txtLastName.Enabled = false;
                txtPay.Enabled = false;
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

        private void lstUsers_SelectedIndexChanged(object sender, EventArgs e)
        {
            // Refresh the user details when a user is selected in the list
            string selectedText = lstUsers.Items[lstUsers.SelectedIndex].ToString();
            string[] details = selectedText.Split(':');
            int employeeID = -1;
            if (int.TryParse(details[0], out employeeID))
            {
                try
                {
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();
                    DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                    command.Connection = m_connection;
                    string query = (
                        "SELECT * FROM EmployeeData WHERE " +
                        "EmployeeID=" + employeeID.ToString() + ";");
                    command.CommandText = query;

                    DbDataReader reader = command.ExecuteReader(); // OleDbDataReader or OdbcDataReader
                    if (reader.Read())
                    {
                        if (ddlAction.Text.Equals(actionNewUser))
                        {
                            txtFirstName.Text = reader[CommonDefs.nameFirstName].ToString();
                            txtLastName.Text = reader[CommonDefs.nameLastName].ToString();
                            txtPay.Text = ((decimal)reader[CommonDefs.namePay]).ToString("0.00");
                        }
                        else if (ddlAction.Text.Equals(actionEditUser))
                        {
                            txtEmployeeID.Text = reader[CommonDefs.nameEmployeeID].ToString();
                            txtFirstName.Text = reader[CommonDefs.nameFirstName].ToString();
                            txtLastName.Text = reader[CommonDefs.nameLastName].ToString();
                            txtPay.Text = ((decimal)reader[CommonDefs.namePay]).ToString("0.00");
                        }
                        else if (ddlAction.Text.Equals(actionDeleteUser))
                        {
                            txtEmployeeID.Text = reader[CommonDefs.nameEmployeeID].ToString();
                        }
                    }

                    m_connection.Close();
                    m_connection.Dispose();
                }
                catch (Exception ex)
                {
                    lblStatus.Text = string.Format("Exception: {0}", ex.Message);
                }
            }
        }

        private void gridUsers_SelectionChanged(object sender, EventArgs e)
        {
            // Refresh the user details when a user is selected in the grid
            DataGridViewCell cell = null;
            foreach (DataGridViewCell selectedCell in gridUsers.SelectedCells)
            {
                cell = selectedCell;
                break;
            }

            if (cell != null)
            {
                DataGridViewRow row = cell.OwningRow;
                decimal pay = 0.0m;
                if (ddlAction.Text.Equals(actionNewUser))
                {
                    txtFirstName.Text = row.Cells[CommonDefs.nameFirstName].Value.ToString();
                    txtLastName.Text = row.Cells[CommonDefs.nameLastName].Value.ToString();
                    if (decimal.TryParse(row.Cells[CommonDefs.namePay].Value.ToString(), out pay))
                        txtPay.Text = pay.ToString("0.00");
                }
                else if (ddlAction.Text.Equals(actionEditUser))
                {
                    txtEmployeeID.Text = row.Cells[CommonDefs.nameEmployeeID].Value.ToString();
                    txtFirstName.Text = row.Cells[CommonDefs.nameFirstName].Value.ToString();
                    txtLastName.Text = row.Cells[CommonDefs.nameLastName].Value.ToString();
                    if (decimal.TryParse(row.Cells[CommonDefs.namePay].Value.ToString(), out pay))
                        txtPay.Text = pay.ToString("0.00");
                }
                else if (ddlAction.Text.Equals(actionDeleteUser))
                {
                    txtEmployeeID.Text = row.Cells[CommonDefs.nameEmployeeID].Value.ToString();
                }
            }
        }

        private void gridUsers_CellClick(object sender, DataGridViewCellEventArgs e)
        {
            // Alternative to extracting data from the grid using the "CellClick" event. The "SelectionChanged"
            // event is more reliable, though.

            /*// Refresh the user details when a user is selected in the grid
            if (e.RowIndex >= 0)
            {
                DataGridViewRow row = gridUsers.Rows[e.RowIndex];
                decimal pay = 0.0m;
                if (ddlAction.Text.Equals(actionNewUser))
                {
                    txtFirstName.Text = row.Cells[CommonDefs.nameFirstName].Value.ToString();
                    txtLastName.Text = row.Cells[CommonDefs.nameLastName].Value.ToString();
                    if (decimal.TryParse(row.Cells[CommonDefs.namePay].Value.ToString(), out pay))
                        txtPay.Text = pay.ToString("0.00");
                }
                else if (ddlAction.Text.Equals(actionEditUser))
                {
                    txtEmployeeID.Text = row.Cells[CommonDefs.nameEmployeeID].Value.ToString();
                    txtFirstName.Text = row.Cells[CommonDefs.nameFirstName].Value.ToString();
                    txtLastName.Text = row.Cells[CommonDefs.nameLastName].Value.ToString();
                    if (decimal.TryParse(row.Cells[CommonDefs.namePay].Value.ToString(), out pay))
                        txtPay.Text = pay.ToString("0.00");
                }
                else if (ddlAction.Text.Equals(actionDeleteUser))
                {
                    txtEmployeeID.Text = row.Cells[CommonDefs.nameEmployeeID].Value.ToString();
                }
            }*/
        }

        private void btnRefreshList_Click(object sender, EventArgs e)
        {
            // Refresh the list with the current users
            RefreshUserList();
            RefreshUserGrid();
        }
        #endregion // Form Events

        #region Private methods
        private void ActionAddNewUser()
        {
            // Add a new user. Ensure the entered data is valid before continuing.
            // Note: Currently requires First Name, Last Name, and pay to be provided.
            if (ValidateDataNewUser())
            {
                try
                {
                    m_error = false;
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();

                    DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                    command.Connection = m_connection;
                    command.CommandText = (
                        "INSERT INTO EmployeeData (FirstName,LastName,Pay) " +
                        "VALUES (" + 
                            "'" + txtFirstName.Text + "'," +
                            "'" + txtLastName.Text + "'," +
                            "'" + txtPay.Text + "')");
                    int rowsAffected = command.ExecuteNonQuery(); 
                    m_connection.Close();
                    m_connection.Dispose();
                    lblStatus.Text = string.Format("User '{0}' '{1}' added to the database [{2} rows affected]",
                        txtFirstName.Text, txtLastName.Text, rowsAffected);
                }
                catch (Exception ex)
                {
                    m_error = true;
                    lblStatus.Text = string.Format("Exception: {0}", ex.Message);
                }
            }
            else
                MessageBox.Show(GetInvalidMessage(), "Error!");
        }

        private void ActionEditExistingUser()
        {
            // Edit an existing user. Ensure the entered data is valid before continuing.
            if (ValidateDataEditExisting())
            {
                // Data is valid. Update the existing user based on Employee ID.
                try
                {
                    m_error = false;
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();

                    DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                    command.Connection = m_connection;
                    string query = (
                        "UPDATE EmployeeData" +
                        GetEditParameters() +
                        "WHERE EmployeeID=" + m_employee.EmployeeID.ToString() + ";");
                    command.CommandText = query;
                    int rowsAffected = command.ExecuteNonQuery(); 
                    m_connection.Close();
                    m_connection.Dispose();
                    lblStatus.Text = string.Format("Employee ID '{0}' updated in the database [{1} rows affected]",
                        m_employee.EmployeeID, rowsAffected);
                }
                catch (Exception ex)
                {
                    m_error = true;
                    lblStatus.Text = string.Format("Exception: {0}", ex.Message);
                }
            }
            else
                MessageBox.Show(GetInvalidMessage(), "Error!");
        }

        private void ActionDeleteExistingUser()
        {
            // Delete an existing user. Ensure the entered data is valid before continuing.
            if (ValidateDataDeleteExisting())
            {
                // Data is valid. Update the existing user based on Employee ID.
                try
                {
                    m_error = false;
                    m_connection.ConnectionString = m_connectionString;
                    m_connection.Open();

                    DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                    command.Connection = m_connection;
                    string query = (
                        "DELETE FROM EmployeeData WHERE EmployeeID=" + m_employee.EmployeeID.ToString() + ";");
                    command.CommandText = query;
                    int rowsAffected = command.ExecuteNonQuery(); 
                    m_connection.Close();
                    m_connection.Dispose();
                    lblStatus.Text = string.Format("Employee ID '{0}' deleted from the database [{1} rows affected]",
                        m_employee.EmployeeID, rowsAffected);
                }
                catch (Exception ex)
                {
                    m_error = true;
                    lblStatus.Text = string.Format("Exception: {0}", ex.Message);
                }
            }
            else
                MessageBox.Show(GetInvalidMessage(), "Error!");
        }

        private bool ValidateDataNewUser()
        {
            // Validate user entry (for adding a new user)
            m_error = false;
            m_errorID = CommonDefs.fieldsNone;
            m_errorCode = CommonDefs.FieldError.fieldError_None;

            // First name
            if (string.IsNullOrEmpty(txtFirstName.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldFirstName;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }
            else if (txtFirstName.Text.Length > firstNameLengthMax)
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldFirstName;
                m_errorCode = CommonDefs.FieldError.fieldError_Length;
            }

            // Last name
            if (string.IsNullOrEmpty(txtLastName.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldLastName;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }
            else if (txtLastName.Text.Length > lastNameLengthMax)
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldLastName;
                m_errorCode = CommonDefs.FieldError.fieldError_Length;
            }

            // Check for a duplicate combination of (FirstName,LastName)
            if (!m_error)
            {
                string query = (
                    "SELECT * FROM EmployeeData WHERE " +
                    "FirstName='" + txtFirstName.Text + "' AND " +
                    "LastName='" + txtLastName.Text + "';");
                if (DoesQueryReturnRows(query))
                {
                    m_error = true;
                    m_errorID |= (
                        CommonDefs.fieldFirstName |
                        CommonDefs.fieldLastName);
                    m_errorCode = CommonDefs.FieldError.fieldError_DuplicateName;
                }
            }

            // Pay
            decimal pay = 0.0m;
            if (!m_error)
            {
                if (string.IsNullOrEmpty(txtPay.Text))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldPay;
                    m_errorCode = CommonDefs.FieldError.fieldError_Blank;
                }
                else if (!decimal.TryParse(txtPay.Text, out pay))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldPay;
                    m_errorCode = CommonDefs.FieldError.fieldError_Invalid;
                }
                else if ((pay < payMin) || (pay > payMax))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldPay;
                    m_errorCode = CommonDefs.FieldError.fieldError_OutOfRange;
                }
            }

            // If there are no errors, this user can be added to the database!
            if (!m_error)
            {
                m_employee.FirstName = txtFirstName.Text;
                m_employee.LastName = txtLastName.Text;
                m_employee.Pay = pay;
            }
            return (!m_error);
        }

        private bool ValidateDataEditExisting()
        {
            // Validate user entry (for editing an existing user)
            m_error = false;
            m_errorID = CommonDefs.fieldsNone;
            m_errorCode = CommonDefs.FieldError.fieldError_None;

            // Assume all user data has been entered and is valid
            m_editFields = CommonDefs.fieldsAll;

            // Employee ID
            int employeeID = -1;
            if (string.IsNullOrEmpty(txtEmployeeID.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldEmployeeID;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }
            else if (!int.TryParse(txtEmployeeID.Text, out employeeID))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldEmployeeID;
                m_errorCode = CommonDefs.FieldError.fieldError_Invalid;
            }
            else if ((employeeID < employeeIdMin) || (employeeID > employeeIdMax))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldEmployeeID;
                m_errorCode = CommonDefs.FieldError.fieldError_OutOfRange;
            }

            // Confirm this employee exists in the database
            if (!m_error)
            {
                string query = (
                    "SELECT * FROM EmployeeData WHERE " +
                    "EmployeeID=" + employeeID.ToString() + ";");
                if (!DoesQueryReturnRows(query))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldEmployeeID;
                    m_errorCode = CommonDefs.FieldError.fieldError_IdUnknown;
                }
            }

            // Check at least one user detail has been provided (e.g. first name)
            if (!m_error)
            {
                if ((string.IsNullOrEmpty(txtFirstName.Text)) &&
                    (string.IsNullOrEmpty(txtLastName.Text)) &&
                    (string.IsNullOrEmpty(txtPay.Text)))
                {
                    m_error = true;
                    m_errorID |= (
                        CommonDefs.fieldFirstName |
                        CommonDefs.fieldLastName |
                        CommonDefs.fieldPay);
                    m_errorCode = CommonDefs.FieldError.fieldError_Blank;
                    m_editFields &= ~(CommonDefs.fieldFirstName);
                    m_editFields &= ~(CommonDefs.fieldLastName);
                    m_editFields &= ~(CommonDefs.fieldPay);
                }
            }

            // First name
            if (!m_error)
            {
                if ((string.IsNullOrEmpty(txtFirstName.Text)) ||
                    (txtFirstName.Text.Length > firstNameLengthMax))
                    m_editFields &= ~(CommonDefs.fieldFirstName);
            }

            // Last name
            if (!m_error)
            {
                if ((string.IsNullOrEmpty(txtLastName.Text)) ||
                    (txtLastName.Text.Length > lastNameLengthMax))
                    m_editFields &= ~(CommonDefs.fieldLastName);
            }

            // Check for a duplicate combination of (FirstName,LastName)
            if (!m_error)
            {
                if ((!string.IsNullOrEmpty(txtFirstName.Text)) &&
                    (!string.IsNullOrEmpty(txtLastName.Text)))
                {
                    string query = (
                        "SELECT * FROM EmployeeData WHERE " +
                        "FirstName='" + txtFirstName.Text + "' AND " +
                        "LastName='" + txtLastName.Text + "';");
                    if (DoesQueryReturnRows(query))
                    {
                        m_error = true;
                        m_errorID |= (
                            CommonDefs.fieldFirstName |
                            CommonDefs.fieldLastName);
                        m_errorCode = CommonDefs.FieldError.fieldError_DuplicateName;
                        m_editFields &= ~(CommonDefs.fieldFirstName);
                        m_editFields &= ~(CommonDefs.fieldLastName);
                    }
                }
            }

            // Pay
            decimal pay = 0.0m;
            if (!m_error)
            {
                if (string.IsNullOrEmpty(txtPay.Text))
                    m_editFields &= ~(CommonDefs.fieldPay);
                else if (!decimal.TryParse(txtPay.Text, out pay))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldPay;
                    m_errorCode = CommonDefs.FieldError.fieldError_Invalid;
                    m_editFields &= ~(CommonDefs.fieldPay);
                }
                else if ((pay < payMin) || (pay > payMax))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldPay;
                    m_errorCode = CommonDefs.FieldError.fieldError_OutOfRange;
                }
            }

            // If there are no errors, this user can be edited in the database!
            if (!m_error)
            {
                m_employee.EmployeeID = employeeID;
                m_employee.FirstName = txtFirstName.Text;
                m_employee.LastName = txtLastName.Text;
                m_employee.Pay = pay;
            }
            return (!m_error);
        }

        private bool ValidateDataDeleteExisting()
        {
            // Validate user entry (for deleting an existing user)
            m_error = false;
            m_errorID = CommonDefs.fieldsNone;
            m_errorCode = CommonDefs.FieldError.fieldError_None;

            // Note: Only the employee ID is required for deleting a user

            // Employee ID
            int employeeID = -1;
            if (string.IsNullOrEmpty(txtEmployeeID.Text))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldEmployeeID;
                m_errorCode = CommonDefs.FieldError.fieldError_Blank;
            }
            else if (!int.TryParse(txtEmployeeID.Text, out employeeID))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldEmployeeID;
                m_errorCode = CommonDefs.FieldError.fieldError_Invalid;
            }
            else if ((employeeID < employeeIdMin) || (employeeID > employeeIdMax))
            {
                m_error = true;
                m_errorID |= CommonDefs.fieldEmployeeID;
                m_errorCode = CommonDefs.FieldError.fieldError_OutOfRange;
            }

            // Confirm this employee exists in the database
            if (!m_error)
            {
                string query = (
                    "SELECT * FROM EmployeeData WHERE " +
                    "EmployeeID=" + employeeID.ToString() + ";");
                if (!DoesQueryReturnRows(query))
                {
                    m_error = true;
                    m_errorID |= CommonDefs.fieldEmployeeID;
                    m_errorCode = CommonDefs.FieldError.fieldError_IdUnknown;
                }
            }

            // If there are no errors, this user can be edited in the database!
            if (!m_error)
                m_employee.EmployeeID = employeeID;

            return (!m_error);
        }

        private string GetEditParameters()
        {
            // Set up the parameters for the "SET" part of the SQL UPDATE statement
            string clauseSET = " SET ";
            int numParams = 0;
            if ((m_editFields & CommonDefs.fieldFirstName) != 0)
            {
                clauseSET += ("FirstName='" + m_employee.FirstName + "'");
                numParams++;
            }

            if ((m_editFields & CommonDefs.fieldLastName) != 0)
            {
                if (numParams > 0)
                    clauseSET += ",";

                clauseSET += ("LastName='" + m_employee.LastName + "'");
                numParams++;
            }

            if ((m_editFields & CommonDefs.fieldPay) != 0)
            {
                if (numParams > 0)
                    clauseSET += ",";

                clauseSET += ("Pay=" + m_employee.Pay.ToString("0.00") + "");
                numParams++;
            }

            clauseSET += " ";
            return clauseSET;
        }

        private string GetInvalidMessage()
        {
            // Validate user entry
            string errorMsg = string.Empty;
            if ((m_errorID & CommonDefs.fieldEmployeeID) != 0)
                errorMsg = string.Format("Employee ID: {0}", GetInvalidDataReason(m_errorCode));
            
            if ((m_errorID & CommonDefs.fieldFirstName) != 0)
            {
                if (errorMsg.Length > 0)
                    errorMsg += "\n";

                errorMsg += string.Format("First Name: {0}", GetInvalidDataReason(m_errorCode));
            }

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

                case CommonDefs.FieldError.fieldError_Invalid:
                    reason = "Invalid format";
                    break;

                case CommonDefs.FieldError.fieldError_OutOfRange:
                    reason = "Out of range";
                    break;

                case CommonDefs.FieldError.fieldError_IdUnknown:
                    reason = "ID not recognised";
                    break;

                case CommonDefs.FieldError.fieldError_DuplicateName:
                    reason = "Duplicate name";
                    break;

                case CommonDefs.FieldError.fieldError_DuplicateUsername:
                    reason = "User already exists";
                    break;

                default:
                    reason = "Unknown";
                    break;
            }

            return reason;
        }

        private bool DoesQueryReturnRows(string query)
        {
            // Helper method which runs the provided query against the database, and returns true if the query
            // returned one or more rows.
            bool hasRows = false;
            try
            {
                m_connection.ConnectionString = m_connectionString;
                m_connection.Open();

                DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                command.Connection = m_connection;
                command.CommandText = query;

                DbDataReader reader = command.ExecuteReader(); // OleDbDataReader or OdbcDataReader
                hasRows = reader.HasRows;
                m_connection.Close();
                m_connection.Dispose();
            }
            catch { }
            return hasRows;
        }

        private void RefreshUserList()
        {
            // Update the list of users in a ListBOx
            try
            {
                m_connection.ConnectionString = m_connectionString;
                m_connection.Open();

                DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                command.Connection = m_connection;
                command.CommandText = ("SELECT * FROM EmployeeData;");

                DbDataReader reader = command.ExecuteReader(); // OleDbDataReader or OdbcDataReader
                int numUsers = 0;
                lstUsers.BeginUpdate();
                lstUsers.Items.Clear();
                while (reader.Read())
                {
                    numUsers++;
                    lstUsers.Items.Add(string.Format("{0}: {1}, {2}, €{3:0.00}",
                        reader[CommonDefs.nameEmployeeID],
                        reader[CommonDefs.nameLastName],
                        reader[CommonDefs.nameFirstName],
                        (decimal)reader[CommonDefs.namePay]));
                }
                lstUsers.EndUpdate();
                lblNumUsers.Text = string.Format("Users: {0}", numUsers);

                m_connection.Close();
                m_connection.Dispose();
            }
            catch (Exception ex)
            {
                lblStatus.Text = string.Format("Exception: {0}", ex.Message);
            }
        }

        private void RefreshUserGrid()
        {
            // Update the list of users in a DataGridView
            try
            {
                m_connection.ConnectionString = m_connectionString;
                m_connection.Open();
                DbCommand command = m_utils.GetDbCommand(); // OleDbCommand or OdbcCommand
                command.Connection = m_connection;
                // Don't use "SELECT * FROM EmployeeData" because the username and password will be displayed!
                command.CommandText = (
                    "SELECT " +
                    CommonDefs.nameEmployeeID + "," +
                    CommonDefs.nameFirstName + "," +
                    CommonDefs.nameLastName + "," +
                    CommonDefs.nameDOB + "," +
                    CommonDefs.nameCountry + "," +
                    CommonDefs.namePhone + "," +
                    CommonDefs.namePay +
                    " FROM EmployeeData;");

                DbDataAdapter da = m_utils.GetDbDataAdapter(command); // OleDbDataAdapter or OdbcDataAdapter
                DataTable dt = new DataTable();
                da.Fill(dt);
                gridUsers.DataSource = dt;

                m_connection.Close();
                m_connection.Dispose();
            }
            catch (Exception ex)
            {
                lblStatus.Text = string.Format("Exception: {0}", ex.Message);
            }
        }
        #endregion // Private methods
    }
}
