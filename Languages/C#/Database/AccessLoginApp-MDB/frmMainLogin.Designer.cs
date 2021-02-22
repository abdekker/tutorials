namespace AccessLoginApp_MDB
{
    partial class frmMainLogin
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
			this.groupLogin = new System.Windows.Forms.GroupBox();
			this.lblStatus = new System.Windows.Forms.Label();
			this.SuspendLayout();
			// 
			// groupLogin
			// 
			this.groupLogin.Location = new System.Drawing.Point(10, 15);
			this.groupLogin.Name = "groupLogin";
			this.groupLogin.Size = new System.Drawing.Size(600, 240);
			this.groupLogin.TabIndex = 0;
			this.groupLogin.TabStop = false;
			this.groupLogin.Text = "Login Details";
			// 
			// lblStatus
			// 
			this.lblStatus.AutoSize = true;
			this.lblStatus.Location = new System.Drawing.Point(12, 265);
			this.lblStatus.Name = "lblStatus";
			this.lblStatus.Size = new System.Drawing.Size(55, 13);
			this.lblStatus.TabIndex = 1;
			this.lblStatus.Text = "Status = ?";
			// 
			// frmMainLogin
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(619, 286);
			this.Controls.Add(this.lblStatus);
			this.Controls.Add(this.groupLogin);
			this.Name = "frmMainLogin";
			this.Text = "Main Logic (Access .mdb)";
			this.Load += new System.EventHandler(this.frmMainLogin_Load);
			this.ResumeLayout(false);
			this.PerformLayout();

        }

		#endregion

		private System.Windows.Forms.GroupBox groupLogin;
		private System.Windows.Forms.Label lblStatus;
	}
}

