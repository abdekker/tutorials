namespace SimpleDbReader
{
    class Utilities_DAO
    {
        // Utilities for using DAO
        public Utilities_DAO() { }

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
    }
}
