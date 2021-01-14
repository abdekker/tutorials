using System.Collections.Generic;

namespace SimpleDbReader
{
    class Utilities_DAO
    {
        // Utilities for using DAO
        public Utilities_DAO() { }

        #region Public methods
        public string GetDbName(string strConnection)
        {
            string dbName = string.Empty;
            try
            {
                DAO.DBEngine dbEngine = new DAO.DBEngine();
                dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);
                DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
                dbName = db.Name;
                db.Close();
            }
            catch { } 
            return dbName;
        }

        public List<string> GetTables(string strConnection, bool removeSysTables = false)
        {
            // Return a list of the tables in the supplied database
            // Note: Access databases use a number System tables used to manage the database, such as:
            // * MSysAccessObjects
            // * MSysACEs
            // * MSysCmdbars
            // * MSysIMEXColumns
            // * MSysIMEXSpecs
            // * MSysObjects
            // * MSysQueries
            // * MSysRelationships
            List<string> tables = new List<string>();
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);
            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            if (db.TableDefs.Count > 0)
            {
                foreach (DAO.TableDef td in db.TableDefs)
                {
                    if ((!removeSysTables) ||
                        (!td.Name.StartsWith("MSys")))
                        tables.Add(td.Name);
                }
            }
            db.Close();
            return tables;
        }

        public List<string> GetFields(string strConnection, string strTable)
        {
            // Return a list of the columns in the supplied table
            List<string> columns = new List<string>();
            DAO.DBEngine dbEngine = new DAO.DBEngine();
            dbEngine.Idle(DAO.IdleEnum.dbRefreshCache);
            DAO.Database db = dbEngine.OpenDatabase(strConnection, false, false);
            if (db.TableDefs.Count > 0)
            {
                try
                {
                    if (db.TableDefs[strTable].Fields.Count > 0)
                    {
                        foreach (DAO.Field fd in db.TableDefs[strTable].Fields)
                        {
                            columns.Add(fd.Name);
                        }
                    }
                }
                catch { }
            }
            db.Close();
            return columns;

            /*
            Public Function FieldType(intType As Integer) As String
If AdoDao = 1 Then
   Select Case intType
      Case adEmpty          '0
        FieldType = "adEmpty"
      Case adTinyInt        '16
        FieldType = "adTinyInt"
      Case adSmallInt       '2
         FieldType = "adSmallInt"
      Case adInteger        '3
        FieldType = "adInteger"
      Case adBigInt         '20
        FieldType = "adBigInt"
      Case adUnsignedTinyInt '17
         FieldType = "adUnsignedTinyInt"
      Case adUnsignedSmallInt '18
        FieldType = "adUnsignedSmallInt"
      Case adUnsignedInt      '19
        FieldType = "adUnsignedInt"
      Case adUnsignedBigInt     '21
        FieldType = "adUnsignedBigInt"
      Case adSingle             '4
        FieldType = "adSingle"
      Case adDouble             '5
        FieldType = "adDouble"
      Case adCurrency           '6
        FieldType = "adCurrency"
      Case adDecimal            '14
        FieldType = "adDecimal"
      Case adNumeric            '131
        FieldType = "adNumeric"
      Case adBoolean            '11
        FieldType = "adBoolean"
      Case adError              '10
        FieldType = "adError"
      Case adUserDefined        '132
        FieldType = "adUserDefined"
      Case adVariant            '12
        FieldType = "adVariant"
      Case adIDispatch          '9
        FieldType = "adIDispatch"
      Case adIUnknown           '13
        FieldType = "adIUnknown"
      Case adGUID               '72
        FieldType = "adGUID"
      Case adDate               '7
        FieldType = "adDate"
      Case adDBDate             '133
        FieldType = "adDBDate"
      Case adDBTime             '134
        FieldType = "adDBTime"
      Case adDBTimeStamp        '135
        FieldType = "adDBTimeStamp"
      Case adBSTR               '8
        FieldType = "adBSTR"
      Case adChar               '129
         FieldType = "adChar"
      Case adVarChar            '200
         FieldType = "adVarChar"
      Case adLongVarChar        '201
        FieldType = "adLongVarChar"
      Case adWChar              '130
        FieldType = "adWChar"
      Case adVarWChar           '202
        FieldType = "adVarWChar"
      Case adLongVarWChar       '203
        FieldType = "adLongVarWChar"
      Case adBinary             '128
        FieldType = "adBinary"
      Case adVarBinary          '204
        FieldType = "adVarBinary"
      Case adLongVarBinary      '205
        FieldType = "adLongVarBinary"
      Case adChapter            '136
        FieldType = "adChapter"
   End Select
Else
    Select Case intType
        Case dbBoolean
            FieldType = "dbBoolean"
        Case dbByte
            FieldType = "dbByte"
        Case dbInteger
            FieldType = "dbInteger"
        Case dbLong
            FieldType = "dbLong"
        Case dbCurrency
            FieldType = "dbCurrency"
        Case dbSingle
            FieldType = "dbSingle"
        Case dbDouble
            FieldType = "dbDouble"
        Case dbDate
            FieldType = "dbDate"
        Case dbText
            FieldType = "dbText"
        Case dbLongBinary
            FieldType = "dbLongBinary"
        Case dbMemo
            FieldType = "dbMemo"
        Case dbGUID
            FieldType = "dbGUID"
    End Select
            */
        }

        public bool DoesFieldExist(DAO.Database db, string strTable, string strField)
        {
            // DAO: HelperBoolFieldToString function to determine if a field exists
            try
            {
                DAO.Field ThisField = db.TableDefs[strTable].Fields[strField];
                return true;
            }
            catch { }
            return false;
        }

        public object SafeGetFieldValue(DAO.Recordset rs, string strField)
        {
            // DAO: Helper function for recordsets which may contain a null value
            object objResult = null;
            try
            {
                if (rs.Fields[strField].Value != null)
                    objResult = rs.Fields[strField].Value;
            }
            catch { }
            return objResult;
        }

        public DAO.Field SafeGetField(DAO.Recordset rs, string strField)
        {
            // DAO: Helper function for recordset fields
            object objField = null;
            try
            {
                objField = rs.Fields[strField];
            }
            catch { }
            return (DAO.Field)objField;
        }

        public string IsRecordUpdateable(DAO.Recordset rs)
        {
            // DAO: Can the recordset be updated?
            if (rs.Updatable)
                return "recordset is writeable";
            else
                return "recordset is read-only";
        }

        public string BoolFieldToString(bool? prop)
        {
            // DAO: Does the field have a value?
            if (prop.HasValue)
                return ((bool)prop) ? "True" : "False";
            else
                return "(null)";
        }

        public string StringFieldToString(string prop)
        {
            // DAO: Does the field have a value?
            if (string.IsNullOrEmpty(prop))
                return "(empty)";
            else
                return prop;
        }
        #endregion // Public methods

        #region Private methods
        private string GetFieldTypeAsString(int type)
        {
            // TODO
            return string.Empty;
            /*type = "Unknown";
            switch (type)
            {
                case DAO.DataTypeEnum.dbBigInt:
                    // 32-bit only
                    if (!m_cfgGeneral.b64bit)
                    {
                        strDataDriver += "{Microsoft Access Driver (*.mdb)};";
                        strDataSource += "\\Northwind 97.mdb;";
                    }
                    else
                    {
                        bHaveConnectionString = false;
                        Console.WriteLine("    ({0} does not support 64-bit)", HelperGetAccessName(false));
                        // Error is "ERROR [IM002] [Microsoft][ODBC Driver Manager] Data source name not found and no default driver specified"
                    }
                    break;

                default:
                    bHaveConnectionString = false;
                    break;
            }*/
        }
        #endregion // Private methods
    }
}
