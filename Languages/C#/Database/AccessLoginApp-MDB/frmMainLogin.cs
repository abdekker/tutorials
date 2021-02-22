using System;
using System.Data;
using System.Data.OleDb;

using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Data.OleDb;

using System.Windows.Forms;

namespace AccessLoginApp_MDB
{
    // Following the tutorial (see below) using:
    // * Microsoft Access (.mdb, pre Access 2007)
    // * OleDb

    // Tutorial URLs:
    // 1) C# MS Access Database Tutorial 1 # Getting Started and Access database Connection (https://www.youtube.com/watch?v=AE-PS6-sL7U)

    public partial class frmMainLogin : Form
    {
        public frmMainLogin()
        {
            InitializeComponent();
        }

        private void frmMainLogin_Load(object sender, EventArgs e)
        {
            // Connection string for Access 2007+:
            //      @"Provider=Microsoft.ACE.OLEDB.12.0;Data Source=C:\MyDatabase.accdb;Persist Security Info=False;";

             // Connection string for older versions of Access:
             //      @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\MyDatabase.mdb;User Id=admin;Password=;";
            OleDbConnection connection = new OleDbConnection();
            connection.ConnectionString =
                @"Provider=Microsoft.Jet.OLEDB.4.0;Data Source=AccessLogin.mdb;User Id=admin;Password=;";
            connection.Open();
            if (connection.State == ConnectionState.Open)
                lblStatus.Text = "Status = Open";
            else
                lblStatus.Text = "Status = ?";

            connection.Close();
        }
    }
}
