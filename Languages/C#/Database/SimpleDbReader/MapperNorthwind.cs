﻿using System;
using System.Collections.ObjectModel;
using System.Data;

namespace SimpleDbReader
{
    // Database structure(s) and mapper(s) for the Northwind databases

    #region Tables

    #region Products table
    public class Northwind_Products
    {
        // Products table

        #region Constants
        // Columns (fields)
        public static readonly string colProductID = "ProductID";
        public static readonly string colProductName = "ProductName";
        public static readonly string colSupplierID = "SupplierID";
        public static readonly string colCategoryID = "CategoryID";
        public static readonly string colQuantityPerUnit = "QuantityPerUnit";
        public static readonly string colUnitPrice = "UnitPrice";
        public static readonly string colUnitsInStock = "UnitsInStock";
        public static readonly string colUnitsOnOrder = "UnitsOnOrder";
        public static readonly string colReorderLevel = "ReorderLevel";
        public static readonly string colDiscontinued = "Discontinued";

        // Bitmask for reading a subset of columns
        public static readonly UInt64 colToReadProductID        = 0x0000000000000001;
        public static readonly UInt64 colToReadProductName      = 0x0000000000000002;
        public static readonly UInt64 colToReadSupplierID       = 0x0000000000000004;
        public static readonly UInt64 colToReadCategoryID       = 0x0000000000000008;
        public static readonly UInt64 colToReadQuantityPerUnit  = 0x0000000000000010;
        public static readonly UInt64 colToReadUnitPrice        = 0x0000000000000020;
        public static readonly UInt64 colToReadUnitsInStock     = 0x0000000000000040;
        public static readonly UInt64 colToReadUnitsOnOrder     = 0x0000000000000080;
        public static readonly UInt64 colToReadReorderLevel     = 0x0000000000000100;
        public static readonly UInt64 colToReadDiscontinued     = 0x0000000000000200;

        // Display width
        public static readonly int colProductIDWidth = 12;
        public static readonly int colProductNameWidth = 40;
        public static readonly int colSupplierIDWidth = 12;
        public static readonly int colCategoryIDWidth = 12;
        public static readonly int colQuantityPerUnitWidth = 12;
        public static readonly int colUnitPriceWidth = 12;
        public static readonly int colUnitsInStockWidth = 12;
        public static readonly int colUnitsOnOrderWidth = 12;
        public static readonly int colReorderLevelWidth = 10;
        public static readonly int colDiscontinuedWidth = 12;

        // Default values when a nullable entry is blank or the SQL query did not return the value
        public static readonly int cDefaultProductID = -1;
        public static readonly string cDefaultProductName = string.Empty;
        public static readonly int cDefaultSupplierID = -1;
        public static readonly int cDefaultCategoryID = -1;
        public static readonly string cDefaultQuantityPerUnit = string.Empty;
        public static readonly decimal cDefaultUnitPrice = 0.0m;
        public static readonly int cDefaultUnitsInStock = 0;
        public static readonly int cDefaultUnitsOnOrder = 0;
        public static readonly int cDefaultReorderLevel = 0;
        public static readonly bool cDefaultDiscontinued = false;
        #endregion

        #region Properties
        /* Property accessors normally use a named member variable, for example:
            private int m_someMember;
            public int SomeMember
            {
                get { return m_someMember; }
                set { m_someMember = value; }
            }
        This is especially useful when you need to take action when the value is set or accessed.

        Since C# 7.0 (VS 2017, .NET 4.7) you can write single expression property get/set accessors:
            get => m_cfgGeneral;
            set => m_cfgGeneral = value;
        In my opinion this is more difficult to read!

        Alternatively, if you don't need specialised get/set functionality, provide a generic property
        accessor and leave the rest to the compiler:
            public int SomeMember { get; set; } */
        public int ProductID { get; set; }
        public string ProductName { get; set; }
        public int SupplierID { get; set; }
        public int CategoryID { get; set; }
        public string QuantityPerUnit { get; set; }
        public decimal UnitPrice { get; set; }
        public int UnitsInStock { get; set; }
        public int UnitsOnOrder { get; set; }
        public int ReorderLevel { get; set; }
        public bool Discontinued { get; set; }
        #endregion // Properties

        // Constructor
        public Northwind_Products() { }

        #region Helper methods
        public void DefaultRecord()
        {
            // Default this record
            this.ProductID = cDefaultProductID;
            this.ProductName = cDefaultProductName;
            this.SupplierID = cDefaultSupplierID;
            this.CategoryID = cDefaultCategoryID;
            this.QuantityPerUnit = cDefaultQuantityPerUnit;
            this.UnitPrice = cDefaultUnitPrice;
            this.UnitsInStock = cDefaultUnitsInStock;
            this.UnitsOnOrder = cDefaultUnitsOnOrder;
            this.ReorderLevel = cDefaultReorderLevel;
            this.Discontinued = cDefaultDiscontinued;
        }

        public static string GetRecordHeader()
        {
            // Helper method to print a table header to the console
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}{7}{8}{9}",
                colProductID.PadRight(colProductIDWidth),
                colProductName.PadRight(colProductNameWidth),
                colSupplierID.PadRight(colSupplierIDWidth),
                colCategoryID.PadRight(colCategoryIDWidth),
                colQuantityPerUnit.PadRight(colQuantityPerUnitWidth),
                colUnitPrice.PadRight(colUnitPriceWidth),
                colUnitsInStock.PadRight(colUnitsInStockWidth),
                colUnitsOnOrder.PadRight(colUnitsOnOrderWidth),
                colReorderLevel.PadRight(colReorderLevelWidth),
                colDiscontinued);
        }

        public string GetRecordAsString()
        {
            // Helper method to format a record for printing to the console
            return string.Format("\t{0}{1}{2}{3}{4}{5}{6}{7}{8}{9}",
                this.ProductID.ToString().PadRight(colProductIDWidth),
                this.ProductName.PadRight(colProductNameWidth),
                this.SupplierID.ToString().PadRight(colSupplierIDWidth),
                this.CategoryID.ToString().PadRight(colCategoryIDWidth),
                this.QuantityPerUnit.PadRight(colQuantityPerUnitWidth),
                this.UnitPrice.ToString("0.00").PadRight(colUnitPriceWidth),
                this.UnitsInStock.ToString().PadRight(colUnitsInStockWidth),
                this.UnitsOnOrder.ToString().PadRight(colUnitsOnOrderWidth),
                this.ReorderLevel.ToString().PadRight(colReorderLevelWidth),
                this.Discontinued);
        }
        #endregion // Helper methods
    }
    #endregion // Products table
    #endregion // Tables

    #region Mappers, Readers
    class NorthwindMapperReader_Products : MapperReaderBase<Northwind_Products>
    {
        protected override Northwind_Products Map(IDataRecord record, UInt64 uRecordsToRead)
        {
            // Mapper working with IDataRecord objects
            string error = string.Empty;
            Northwind_Products p = new Northwind_Products();
            p.DefaultRecord();
            if ((uRecordsToRead & Northwind_Products.colToReadProductID) != 0)
            {
                try
                {
                    p.ProductID = (DBNull.Value == record[Northwind_Products.colProductID])
                        ? Northwind_Products.cDefaultProductID
                        : (int)record[Northwind_Products.colProductID];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadProductName) != 0)
            {
                try
                {
                    p.ProductName = (DBNull.Value == record[Northwind_Products.colProductName])
                        ? Northwind_Products.cDefaultProductName
                        : (string)record[Northwind_Products.colProductName];
                }
                catch (Exception ex) { error = ex.Message; }
            }
                
            if ((uRecordsToRead & Northwind_Products.colToReadSupplierID) != 0)
            {
                try
                {
                    p.SupplierID = (DBNull.Value == record[Northwind_Products.colSupplierID])
                        ? Northwind_Products.cDefaultSupplierID
                        : (int)record[Northwind_Products.colSupplierID];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadCategoryID) != 0)
            {
                try
                {
                    p.CategoryID = (DBNull.Value == record[Northwind_Products.colCategoryID])
                        ? Northwind_Products.cDefaultCategoryID
                        : (int)record[Northwind_Products.colCategoryID];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadQuantityPerUnit) != 0)
            {
                try
                {
                    p.QuantityPerUnit = (DBNull.Value == record[Northwind_Products.colQuantityPerUnit])
                        ? Northwind_Products.cDefaultQuantityPerUnit
                        : (string)record[Northwind_Products.colQuantityPerUnit];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadUnitPrice) != 0)
            {
                try
                {
                    p.UnitPrice = (DBNull.Value == record[Northwind_Products.colUnitPrice])
                        ? Northwind_Products.cDefaultUnitPrice
                        : (decimal)record[Northwind_Products.colUnitPrice];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadUnitsInStock) != 0)
            {
                try
                {
                    p.UnitsInStock = (DBNull.Value == record[Northwind_Products.colUnitsInStock])
                        ? Northwind_Products.cDefaultUnitsInStock
                        : (int)record[Northwind_Products.colUnitsInStock];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadUnitsOnOrder) != 0)
            {
                try
                {
                    p.UnitsOnOrder = (DBNull.Value == record[Northwind_Products.colUnitsOnOrder])
                        ? Northwind_Products.cDefaultUnitsOnOrder
                        : (int)record[Northwind_Products.colUnitsOnOrder];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadReorderLevel) != 0)
            {
                try
                {
                    p.ReorderLevel = (DBNull.Value == record[Northwind_Products.colReorderLevel])
                        ? Northwind_Products.cDefaultReorderLevel
                        : (int)record[Northwind_Products.colReorderLevel];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadDiscontinued) != 0)
            {
                try
                {
                    p.Discontinued = (DBNull.Value == record[Northwind_Products.colDiscontinued])
                        ? Northwind_Products.cDefaultDiscontinued
                        : (bool)record[Northwind_Products.colDiscontinued];
                }
                catch (Exception ex) { error = ex.Message; }
            }
            
            if (!string.IsNullOrEmpty(error))
                  Console.WriteLine(UtilitiesGeneral.FormatException(
                       this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, error));

            return p;
        }
    }

    class NorthwindMapperAdapter_Products : MapperAdapterBase<Northwind_Products>
    {
        protected override Northwind_Products Map(DataRow row, UInt64 uRecordsToRead)
        {
            // Mapper working with DataRow objects
            string error = string.Empty;
            Northwind_Products p = new Northwind_Products();
            p.DefaultRecord();
            if ((uRecordsToRead & Northwind_Products.colToReadProductID) != 0)
            {
                try
                {
                    p.ProductID = (DBNull.Value == row[Northwind_Products.colProductID])
                        ? Northwind_Products.cDefaultProductID
                        : (int)row[Northwind_Products.colProductID];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadProductName) != 0)
            {
                try
                {
                    p.ProductName = (DBNull.Value == row[Northwind_Products.colProductName])
                        ? Northwind_Products.cDefaultProductName
                        : (string)row[Northwind_Products.colProductName];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadSupplierID) != 0)
            {
                try
                {
                    p.SupplierID = (DBNull.Value == row[Northwind_Products.colSupplierID])
                        ? Northwind_Products.cDefaultSupplierID
                        : (int)row[Northwind_Products.colSupplierID];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadCategoryID) != 0)
            {
                try
                {
                    p.CategoryID = (DBNull.Value == row[Northwind_Products.colCategoryID])
                        ? Northwind_Products.cDefaultCategoryID
                        : (int)row[Northwind_Products.colCategoryID];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadQuantityPerUnit) != 0)
            {
                try
                {
                    p.QuantityPerUnit = (DBNull.Value == row[Northwind_Products.colQuantityPerUnit])
                        ? Northwind_Products.cDefaultQuantityPerUnit
                        : (string)row[Northwind_Products.colQuantityPerUnit];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadUnitPrice) != 0)
            {
                try
                {
                    p.UnitPrice = (DBNull.Value == row[Northwind_Products.colUnitPrice])
                        ? Northwind_Products.cDefaultUnitPrice
                        : (decimal)row[Northwind_Products.colUnitPrice];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadUnitsInStock) != 0)
            {
                try
                {
                    p.UnitsInStock = (DBNull.Value == row[Northwind_Products.colUnitsInStock])
                        ? Northwind_Products.cDefaultUnitsInStock
                        : (int)row[Northwind_Products.colUnitsInStock];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadUnitsOnOrder) != 0)
            {
                try
                {
                    p.UnitsOnOrder = (DBNull.Value == row[Northwind_Products.colUnitsOnOrder])
                        ? Northwind_Products.cDefaultUnitsOnOrder
                        : (int)row[Northwind_Products.colUnitsOnOrder];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadReorderLevel) != 0)
            {
                try
                {
                    p.ReorderLevel = (DBNull.Value == row[Northwind_Products.colReorderLevel])
                        ? Northwind_Products.cDefaultReorderLevel
                        : (int)row[Northwind_Products.colReorderLevel];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if ((uRecordsToRead & Northwind_Products.colToReadDiscontinued) != 0)
            {
                try
                {
                    p.Discontinued = (DBNull.Value == row[Northwind_Products.colDiscontinued])
                        ? Northwind_Products.cDefaultDiscontinued
                        : (bool)row[Northwind_Products.colDiscontinued];
                }
                catch (Exception ex) { error = ex.Message; }
            }

            if (!string.IsNullOrEmpty(error))
                  Console.WriteLine(UtilitiesGeneral.FormatException(
                       this.ToString(), System.Reflection.MethodBase.GetCurrentMethod().Name, error));

            return p;
        }
    }

    class NorthwindReader_Products : ObjectReaderBase<Northwind_Products>, IDisposable
    {
        #region Properties and methods from ObjectAdapterBase
        public override DatabaseTechnology DbTechnology { get; set; }
        public override string ConnectionString { get; set; }
        public override string CmdText { get; set; }
        public override CommandType CmdType { get; set; }
        public override UInt64 RecordsToRead { get; set; }

        protected override Collection<IDataParameter> GetParameters(IDbCommand command)
        {
            Collection<IDataParameter> collection = new Collection<IDataParameter>();
            return collection;

            // If you have parameters:
            //IDataParameter param1 = command.CreateParameter();
            //param1.ParameterName = "paramName 1";     // Put the parameter name here
            //param1.Value = 5;                         // Put the parameter value here
            //collection.Add(param1);
            //return collection;   
        }

        protected override MapperReaderBase<Northwind_Products> GetMapperReader()
        {
            MapperReaderBase<Northwind_Products> mapper = new NorthwindMapperReader_Products();
            return mapper;
        }
        #endregion // Properties and methods from ObjectAdapterBase

        #region Methods from IDisposable
        public void Dispose() { }
        #endregion // Methods from IDisposable
    }

    class NorthwindAdapter_Products : ObjectAdapterBase<Northwind_Products>, IDisposable
    {
        #region Properties and methods from ObjectAdapterBase
        public override DatabaseTechnology DbTechnology { get; set; }
        public override string ConnectionString { get; set; }
        public override string CmdText { get; set; }
        public override CommandType CmdType { get; set; }
        public override UInt64 RecordsToRead { get; set; }

        protected override Collection<IDataParameter> GetParameters(IDbCommand command)
        {
            Collection<IDataParameter> collection = new Collection<IDataParameter>();
            return collection;

            // If you have parameters:
            //IDataParameter param1 = command.CreateParameter();
            //param1.ParameterName = "paramName 1";     // Put the parameter name here
            //param1.Value = 5;                         // Put the parameter value here
            //collection.Add(param1);
            //return collection;   
        }

        protected override MapperAdapterBase<Northwind_Products> GetMapperAdapter()
        {
            MapperAdapterBase<Northwind_Products> mapper = new NorthwindMapperAdapter_Products();
            return mapper;
        }
        #endregion // Properties and methods from ObjectAdapterBase

        #region Methods from IDisposable
        public void Dispose() { }
        #endregion // Methods from IDisposable
    }
    #endregion // Mappers, Readers
}
