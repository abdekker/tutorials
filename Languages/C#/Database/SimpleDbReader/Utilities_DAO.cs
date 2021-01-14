using System;
using System.Collections.Generic;

namespace SimpleDbReader
{
    class Utilities_DAO
    {
        // Utilities for using DAO
        private string m_fieldHeader;

        public Utilities_DAO()
        {
            // Header when displaying field information
            m_fieldHeader = string.Format("{0,-20}{1,-12}{2}", "Name", "Type", "Size");
        }

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
                        columns.Add(m_fieldHeader);
                        foreach (DAO.Field fd in db.TableDefs[strTable].Fields)
                        {
                            columns.Add(string.Format("{0,-20}{1,-12}{2}",
                                fd.Name,
                                GetFieldTypeAsString(fd),
                                fd.Size));
                        }
                    }
                }
                catch { }
            }
            db.Close();
            return columns;
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
        private string GetFieldTypeAsString(DAO.Field fd)
        {
            // Convert DAO.Type to a human-readable string. This can be converted like this:
            //       ((DAO.DataTypeEnum)DAO.Field .Type).ToString()) 
            // but returns something like "dbText", when we want "Text" (or similar).

            // Alternatively, use a giant switch:
            /*  switch (type)
                {
                    case (short)DAO.DataTypeEnum.dbBoolean:
                    strType = "Boolean";
                        break;
                    // etc...
            */
            string strType = "Unknown";
            try
            {
                strType = ((DAO.DataTypeEnum)fd.Type).ToString().Replace("db", "");
            }
            catch { }
            return strType;
        }
    #endregion // Private methods
}
}
