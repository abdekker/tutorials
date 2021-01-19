using System;
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

        // Methods
        public abstract string GetDbName(string strConnection);
    }
}
