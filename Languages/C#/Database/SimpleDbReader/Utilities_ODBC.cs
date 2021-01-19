using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;

using System.Data.Odbc;

namespace SimpleDbReader
{
    class Utilities_ODBC
    {
        // Utilities for using ODBC

        #region Member variables
        #endregion // Member variables

        #region Constants
        // Tables schema
        public readonly string Schema_Tables_ODBC_SystemTable = "SYSTEM TABLE";

        // Columns schema
        public readonly string Schema_Columns_ODBC_Name = "COLUMN_NAME";
        public readonly string Schema_Columns_ODBC_Type = "DATA_TYPE";
        public readonly string Schema_Columns_ODBC_TypeName = "TYPE_NAME";
        public readonly string Schema_Columns_ODBC_Size = "COLUMN_SIZE";
        public readonly string Schema_Columns_ODBC_Nullable = "IS_NULLABLE";
        #endregion // Constants

        public Utilities_ODBC() { }

        #region Public methods
        #endregion // Public methods

        #region Private methods
        #endregion // Private methods
    }
}
