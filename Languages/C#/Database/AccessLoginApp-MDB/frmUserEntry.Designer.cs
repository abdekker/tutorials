﻿
namespace AccessLoginApp_MDB
{
    partial class frmUserEntry
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
            this.groupUserEntry = new System.Windows.Forms.GroupBox();
            this.lblUserName = new System.Windows.Forms.Label();
            this.ddlAction = new System.Windows.Forms.ComboBox();
            this.txtEmployeeID = new System.Windows.Forms.TextBox();
            this.lblEmployeeID = new System.Windows.Forms.Label();
            this.btnUpdate = new System.Windows.Forms.Button();
            this.txtPay = new System.Windows.Forms.TextBox();
            this.lblPay = new System.Windows.Forms.Label();
            this.txtLastName = new System.Windows.Forms.TextBox();
            this.txtFirstName = new System.Windows.Forms.TextBox();
            this.lblLastName = new System.Windows.Forms.Label();
            this.lblFirstName = new System.Windows.Forms.Label();
            this.lblStatusTitle = new System.Windows.Forms.Label();
            this.lblStatus = new System.Windows.Forms.Label();
            this.groupReview = new System.Windows.Forms.GroupBox();
            this.lstFields = new System.Windows.Forms.ListBox();
            this.btnUpdateList = new System.Windows.Forms.Button();
            this.lblNumUsers = new System.Windows.Forms.Label();
            this.groupUserEntry.SuspendLayout();
            this.groupReview.SuspendLayout();
            this.SuspendLayout();
            // 
            // groupUserEntry
            // 
            this.groupUserEntry.Controls.Add(this.lblUserName);
            this.groupUserEntry.Controls.Add(this.ddlAction);
            this.groupUserEntry.Controls.Add(this.txtEmployeeID);
            this.groupUserEntry.Controls.Add(this.lblEmployeeID);
            this.groupUserEntry.Controls.Add(this.btnUpdate);
            this.groupUserEntry.Controls.Add(this.txtPay);
            this.groupUserEntry.Controls.Add(this.lblPay);
            this.groupUserEntry.Controls.Add(this.txtLastName);
            this.groupUserEntry.Controls.Add(this.txtFirstName);
            this.groupUserEntry.Controls.Add(this.lblLastName);
            this.groupUserEntry.Controls.Add(this.lblFirstName);
            this.groupUserEntry.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.groupUserEntry.Location = new System.Drawing.Point(10, 10);
            this.groupUserEntry.Name = "groupUserEntry";
            this.groupUserEntry.Size = new System.Drawing.Size(365, 280);
            this.groupUserEntry.TabIndex = 0;
            this.groupUserEntry.TabStop = false;
            this.groupUserEntry.Text = "User Entry";
            // 
            // lblUserName
            // 
            this.lblUserName.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.2F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblUserName.Location = new System.Drawing.Point(265, 40);
            this.lblUserName.Name = "lblUserName";
            this.lblUserName.Size = new System.Drawing.Size(80, 28);
            this.lblUserName.TabIndex = 13;
            this.lblUserName.Text = "Mark Tesar";
            this.lblUserName.Visible = false;
            // 
            // ddlAction
            // 
            this.ddlAction.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.ddlAction.FormattingEnabled = true;
            this.ddlAction.Location = new System.Drawing.Point(20, 224);
            this.ddlAction.Name = "ddlAction";
            this.ddlAction.Size = new System.Drawing.Size(110, 33);
            this.ddlAction.TabIndex = 5;
            this.ddlAction.SelectedIndexChanged += new System.EventHandler(this.ddlAction_SelectedIndexChanged);
            // 
            // txtEmployeeID
            // 
            this.txtEmployeeID.Location = new System.Drawing.Point(180, 39);
            this.txtEmployeeID.Margin = new System.Windows.Forms.Padding(2);
            this.txtEmployeeID.Name = "txtEmployeeID";
            this.txtEmployeeID.Size = new System.Drawing.Size(80, 31);
            this.txtEmployeeID.TabIndex = 0;
            // 
            // lblEmployeeID
            // 
            this.lblEmployeeID.AutoSize = true;
            this.lblEmployeeID.Font = new System.Drawing.Font("Microsoft Sans Serif", 16.2F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblEmployeeID.Location = new System.Drawing.Point(15, 40);
            this.lblEmployeeID.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblEmployeeID.Name = "lblEmployeeID";
            this.lblEmployeeID.Size = new System.Drawing.Size(149, 26);
            this.lblEmployeeID.TabIndex = 11;
            this.lblEmployeeID.Text = "Employee ID";
            // 
            // btnUpdate
            // 
            this.btnUpdate.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.btnUpdate.Location = new System.Drawing.Point(145, 220);
            this.btnUpdate.Name = "btnUpdate";
            this.btnUpdate.Size = new System.Drawing.Size(200, 40);
            this.btnUpdate.TabIndex = 4;
            this.btnUpdate.Text = "Update";
            this.btnUpdate.UseVisualStyleBackColor = true;
            this.btnUpdate.Click += new System.EventHandler(this.btnUpdate_Click);
            // 
            // txtPay
            // 
            this.txtPay.Location = new System.Drawing.Point(180, 174);
            this.txtPay.Margin = new System.Windows.Forms.Padding(2);
            this.txtPay.Name = "txtPay";
            this.txtPay.Size = new System.Drawing.Size(165, 31);
            this.txtPay.TabIndex = 3;
            // 
            // lblPay
            // 
            this.lblPay.AutoSize = true;
            this.lblPay.Font = new System.Drawing.Font("Microsoft Sans Serif", 16.2F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblPay.Location = new System.Drawing.Point(15, 175);
            this.lblPay.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblPay.Name = "lblPay";
            this.lblPay.Size = new System.Drawing.Size(89, 26);
            this.lblPay.TabIndex = 8;
            this.lblPay.Text = "Pay (€)";
            // 
            // txtLastName
            // 
            this.txtLastName.Location = new System.Drawing.Point(180, 129);
            this.txtLastName.Margin = new System.Windows.Forms.Padding(2);
            this.txtLastName.Name = "txtLastName";
            this.txtLastName.Size = new System.Drawing.Size(165, 31);
            this.txtLastName.TabIndex = 2;
            // 
            // txtFirstName
            // 
            this.txtFirstName.Location = new System.Drawing.Point(180, 84);
            this.txtFirstName.Margin = new System.Windows.Forms.Padding(2);
            this.txtFirstName.Name = "txtFirstName";
            this.txtFirstName.Size = new System.Drawing.Size(165, 31);
            this.txtFirstName.TabIndex = 1;
            // 
            // lblLastName
            // 
            this.lblLastName.AutoSize = true;
            this.lblLastName.Font = new System.Drawing.Font("Microsoft Sans Serif", 16.2F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblLastName.Location = new System.Drawing.Point(15, 130);
            this.lblLastName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblLastName.Name = "lblLastName";
            this.lblLastName.Size = new System.Drawing.Size(57, 26);
            this.lblLastName.TabIndex = 5;
            this.lblLastName.Text = "Last";
            // 
            // lblFirstName
            // 
            this.lblFirstName.AutoSize = true;
            this.lblFirstName.Font = new System.Drawing.Font("Microsoft Sans Serif", 16.2F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblFirstName.Location = new System.Drawing.Point(15, 85);
            this.lblFirstName.Margin = new System.Windows.Forms.Padding(2, 0, 2, 0);
            this.lblFirstName.Name = "lblFirstName";
            this.lblFirstName.Size = new System.Drawing.Size(59, 26);
            this.lblFirstName.TabIndex = 4;
            this.lblFirstName.Text = "First";
            // 
            // lblStatusTitle
            // 
            this.lblStatusTitle.AutoSize = true;
            this.lblStatusTitle.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.2F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblStatusTitle.Location = new System.Drawing.Point(10, 295);
            this.lblStatusTitle.Name = "lblStatusTitle";
            this.lblStatusTitle.Size = new System.Drawing.Size(37, 13);
            this.lblStatusTitle.TabIndex = 12;
            this.lblStatusTitle.Text = "Status";
            // 
            // lblStatus
            // 
            this.lblStatus.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.2F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblStatus.Location = new System.Drawing.Point(50, 295);
            this.lblStatus.Name = "lblStatus";
            this.lblStatus.Size = new System.Drawing.Size(325, 16);
            this.lblStatus.TabIndex = 11;
            this.lblStatus.Text = "?";
            // 
            // groupReview
            // 
            this.groupReview.Controls.Add(this.lblNumUsers);
            this.groupReview.Controls.Add(this.btnUpdateList);
            this.groupReview.Controls.Add(this.lstFields);
            this.groupReview.Location = new System.Drawing.Point(385, 10);
            this.groupReview.Name = "groupReview";
            this.groupReview.Size = new System.Drawing.Size(265, 280);
            this.groupReview.TabIndex = 13;
            this.groupReview.TabStop = false;
            this.groupReview.Text = "Review";
            // 
            // lstFields
            // 
            this.lstFields.Font = new System.Drawing.Font("Consolas", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lstFields.FormattingEnabled = true;
            this.lstFields.Location = new System.Drawing.Point(5, 20);
            this.lstFields.Name = "lstFields";
            this.lstFields.Size = new System.Drawing.Size(255, 225);
            this.lstFields.TabIndex = 5;
            // 
            // btnUpdateList
            // 
            this.btnUpdateList.Location = new System.Drawing.Point(170, 250);
            this.btnUpdateList.Name = "btnUpdateList";
            this.btnUpdateList.Size = new System.Drawing.Size(85, 23);
            this.btnUpdateList.TabIndex = 6;
            this.btnUpdateList.Text = "Update List";
            this.btnUpdateList.UseVisualStyleBackColor = true;
            this.btnUpdateList.Click += new System.EventHandler(this.btnUpdateList_Click);
            // 
            // lblNumUsers
            // 
            this.lblNumUsers.AutoSize = true;
            this.lblNumUsers.Font = new System.Drawing.Font("Microsoft Sans Serif", 7.2F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblNumUsers.Location = new System.Drawing.Point(10, 250);
            this.lblNumUsers.Name = "lblNumUsers";
            this.lblNumUsers.Size = new System.Drawing.Size(45, 13);
            this.lblNumUsers.TabIndex = 13;
            this.lblNumUsers.Text = "Users: ?";
            // 
            // frmUserEntry
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(661, 316);
            this.Controls.Add(this.groupReview);
            this.Controls.Add(this.lblStatusTitle);
            this.Controls.Add(this.groupUserEntry);
            this.Controls.Add(this.lblStatus);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Name = "frmUserEntry";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.Text = "User Details";
            this.Load += new System.EventHandler(this.frmUserEntry_Load);
            this.groupUserEntry.ResumeLayout(false);
            this.groupUserEntry.PerformLayout();
            this.groupReview.ResumeLayout(false);
            this.groupReview.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.GroupBox groupUserEntry;
        private System.Windows.Forms.TextBox txtLastName;
        private System.Windows.Forms.TextBox txtFirstName;
        private System.Windows.Forms.Label lblLastName;
        private System.Windows.Forms.Label lblFirstName;
        private System.Windows.Forms.Button btnUpdate;
        private System.Windows.Forms.TextBox txtPay;
        private System.Windows.Forms.Label lblPay;
        private System.Windows.Forms.Label lblStatusTitle;
        private System.Windows.Forms.Label lblStatus;
        private System.Windows.Forms.ComboBox ddlAction;
        private System.Windows.Forms.TextBox txtEmployeeID;
        private System.Windows.Forms.Label lblEmployeeID;
        private System.Windows.Forms.Label lblUserName;
        private System.Windows.Forms.GroupBox groupReview;
        private System.Windows.Forms.ListBox lstFields;
        private System.Windows.Forms.Button btnUpdateList;
        private System.Windows.Forms.Label lblNumUsers;
    }
}