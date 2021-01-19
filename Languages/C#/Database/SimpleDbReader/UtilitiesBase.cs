﻿using System;
using System.Collections.Generic;

namespace SimpleDbReader
{
    abstract class UtilitiesBase
    {
        // Base class for utilities for using DAO, ODBC, OleDB, etc

        // Member variables
        protected DatabaseTechnology m_tech;

        // Constructor
        //public UtilitiesBase(DatabaseTechnology tech)
        //{
        //    DbTechnology = tech;
        //}

        // Property accessors (for member variables)
        public abstract DatabaseTechnology DbTechnology { get; set; }

        // Abstract methods
        public abstract string GetDbName(string strConnection);

        // Methods
        public string GetReadTechnologyAsString(DatabaseReadTechnology eReadTechnology)
        {
            // Return a header for the console which describes the performance test
            string header = string.Empty;
            if (eReadTechnology == DatabaseReadTechnology.eRbRead_DataReader)
                header += "IDataReader";
            else if (eReadTechnology == DatabaseReadTechnology.eRbRead_DataAdapter)
                header += "IDbDataAdapter";

            return header;
        }
    }
}
