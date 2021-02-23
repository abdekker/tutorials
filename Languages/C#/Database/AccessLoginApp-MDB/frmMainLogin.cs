using System;
using System.Data;
#if UseOleDB
    using System.Data.OleDb;
#elif UseODBC
    using System.Data.Odbc;
#endif
using System.Windows.Forms;

namespace AccessLoginApp_MDB
{
    // Following the tutorial (see below) using:
    // * Microsoft Access (.mdb, pre Access 2007)
    // * OleDB (open project properties and set Build\Conditional compilation symbols to "UseOleDB")
    // * ODBC (as above, but set to "UseODBC")

    // Example database at: abdekker\privDevelopment\Data\Database\AccessLogin.mdb
    // Created in Access 365 and saved as "Access 2002-2003 Database"

    // Tutorial URL = https://www.youtube.com/watch?v=AE-PS6-sL7U (Tutorial 1 of 21)
    // (or search for "C# MS Access Database Tutorial")

    public partial class frmMainLogin : Form
    {
        #region Member variables
        private bool m_error = false;
        #if UseOleDB
            private OleDbConnection m_connection = new OleDbConnection();
            private string m_connectionString =
                @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Apps\Data\AccessLogin.mdb;User Id=admin;Password=;";
        #elif UseODBC
            private OdbcConnection m_connection = new OdbcConnection();
            private string m_connectionString =
                @"Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\Apps\Data\AccessLogin.mdb;Uid=Admin;Pwd=;";
        #endif
        #endregion // Member variables

        public frmMainLogin()
        {
            InitializeComponent();
        }

        private void frmMainLogin_Load(object sender, EventArgs e)
        {
            // Start an update timer for the status of the connection
            tmrStatus.Enabled = true;

            try
            {
                // Set the connection string for the database
                // Connection string for Access 2007+ (OleDB):
                //      @"Provider=Microsoft.ACE.OLEDB.12.0;Data Source=C:\MyDatabase.accdb;Persist Security Info=False;";

                // Connection string for older versions of Access (OleDB):
                //      @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\MyDatabase.mdb;User Id=admin;Password=;";

                // Connection string for ODBC:
                //      @"Driver={Microsoft Access Driver (*.mdb)};Dbq=C:\MyDatabase.mdb;Uid=Admin;Pwd=;";
                m_connection.ConnectionString = m_connectionString;

                // Test open/close the database
                m_connection.Open();
                m_connection.Close();
            }
            catch (Exception ex)
            {
                m_error = true;
                lblStatus.Text = string.Format("Exception: {0}", ex.Message);
            }
        }

        private void btnLogin_Click(object sender, EventArgs e)
        {
            // Validate the username and password
            try
            {
                m_error = false;
                m_connection.ConnectionString = m_connectionString;
                m_connection.Open();
                #if UseOleDB
                    OleDbCommand command = new OleDbCommand();
                #elif UseODBC
                    OdbcCommand command = new OdbcCommand();
                #endif
                command.Connection = m_connection;
                command.CommandText = (
                    "SELECT * FROM EmployeeData WHERE " +
                    "Username='" + txtUsername.Text + "' AND " +     // Change to "OR" to show duplicate users
                    "Password='" + txtPassword.Text + "'");

                #if UseOleDB
                    OleDbDataReader reader = command.ExecuteReader();
                #elif UseODBC
                    OdbcDataReader reader = command.ExecuteReader();
                #endif
                int countUsers = 0;
                while (reader.Read())
                {
                    countUsers++;
                }
                lblDebug.Text = string.Format("Users: {0}", countUsers);

                string msg = string.Empty;
                if (countUsers != 1)
                {
                    m_error = true;
                    if (countUsers < 1)
                        msg = string.Format("User '{0}' not found or password incorrect", txtUsername.Text);
                    else if (countUsers > 1)
                        msg = string.Format("Duplicate users! {0} users found with those credentials.", countUsers);

                    lblStatus.Text = msg;   // Or: MessageBox.Show(msg);
                }

                m_connection.Close();
                m_connection.Dispose();
                if (!m_error)
                {
                    this.Hide();
                    try
                    {
                        frmUserEntry userEntry = new frmUserEntry(m_connectionString);
                        userEntry.ShowDialog();
                    }
                    catch { }

                    this.txtPassword.Clear();
                    this.btnLogin.Focus();
                    this.Show();
                }
            }
            catch (Exception ex)
            {
                m_error = true;
                lblStatus.Text = string.Format("Exception: {0}", ex.Message);
            }
        }

        private void tmrStatus_Tick(object sender, EventArgs e)
        {
            // Update the status for the connection
            tmrStatus.Enabled = false;
            if (!m_error)
            {
                switch (m_connection.State)
                {
                    case ConnectionState.Closed:
                        lblStatus.Text = "Closed";
                        break;

                    case ConnectionState.Open:
                        lblStatus.Text = "Open";
                        break;

                    case ConnectionState.Connecting:
                        lblStatus.Text = "Connecting";
                        break;

                    case ConnectionState.Executing:
                        lblStatus.Text = "Executing";
                        break;

                    case ConnectionState.Fetching:
                        lblStatus.Text = "Fetching";
                        break;

                    case ConnectionState.Broken:
                        lblStatus.Text = "Broken";
                        break;

                    default:
                        lblStatus.Text = "???";
                        break;
                }
            }

            tmrStatus.Enabled = true;
        }
    }
}
