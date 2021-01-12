using System;
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

        // Display width
        public static readonly int colProductIDWidth = 10;
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

        #region Fields
        // Fields (columns) in this table
        private int m_productID;
        private string m_productName;
        private int m_supplierID;
        private int m_categoryID;
        private string m_quantityPerUnit;
        private decimal m_unitPrice;
        private int m_unitsInStock;
        private int m_unitsOnOrder;
        private int m_reorderLevel;
        private bool m_discontinued;
        #endregion // Fields

        #region Properties
        public int ProductID
        {
            get { return m_productID; }
            set { m_productID = value; }
        }

        public string ProductName
        {
            get { return m_productName; }
            set { m_productName = value; }
        }

        public int SupplierID
        {
            get { return m_supplierID; }
            set { m_supplierID = value; }
        }

        public int CategoryID
        {
            get { return m_categoryID; }
            set { m_categoryID = value; }
        }

        public string QuantityPerUnit
        {
            get { return m_quantityPerUnit; }
            set { m_quantityPerUnit = value; }
        }

        public decimal UnitPrice
        {
            get { return m_unitPrice; }
            set { m_unitPrice = value; }
        }

        public int UnitsInStock
        {
            get { return m_unitsInStock; }
            set { m_unitsInStock = value; }
        }

        public int UnitsOnOrder
        {
            get { return m_unitsOnOrder; }
            set { m_unitsOnOrder = value; }
        }

        public int ReorderLevel
        {
            get { return m_reorderLevel; }
            set { m_reorderLevel = value; }
        }

        public bool Discontinued
        {
            get { return m_discontinued; }
            set { m_discontinued = value; }
        }
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
    class NorthwindMapper_Products : MapperBase<Northwind_Products>
    {
        protected override Northwind_Products Map(IDataRecord record)
        {
            Northwind_Products p = new Northwind_Products();
            p.DefaultRecord();
            try
            {
                p.ProductID = (DBNull.Value == record[Northwind_Products.colProductID])
                    ? Northwind_Products.cDefaultProductID
                    : (int)record[Northwind_Products.colProductID];
            }
            catch { }

            try
            {
                p.ProductName = (DBNull.Value == record[Northwind_Products.colProductName])
                    ? Northwind_Products.cDefaultProductName
                    : (string)record[Northwind_Products.colProductName];
            }
            catch { }

            try
            {
                p.SupplierID = (DBNull.Value == record[Northwind_Products.colSupplierID])
                    ? Northwind_Products.cDefaultSupplierID
                    : (int)record[Northwind_Products.colSupplierID];
            }
            catch { }

            try
            {
                p.CategoryID = (DBNull.Value == record[Northwind_Products.colCategoryID])
                    ? Northwind_Products.cDefaultCategoryID
                    : (int)record[Northwind_Products.colCategoryID];
            }
            catch { }
            
            try
            {
                p.QuantityPerUnit = (DBNull.Value == record[Northwind_Products.colQuantityPerUnit])
                    ? Northwind_Products.cDefaultQuantityPerUnit
                    : (string)record[Northwind_Products.colQuantityPerUnit];
            }
            catch { }
            
            try
            {
                p.UnitPrice = (DBNull.Value == record[Northwind_Products.colUnitPrice])
                    ? Northwind_Products.cDefaultUnitPrice
                    : (decimal)record[Northwind_Products.colUnitPrice];
            }
            catch { }
            
            try
            {
                p.UnitsInStock = (DBNull.Value == record[Northwind_Products.colUnitsInStock])
                    ? Northwind_Products.cDefaultUnitsInStock
                    : (int)record[Northwind_Products.colUnitsInStock];
            }
            catch { }

            try
            {
                p.UnitsOnOrder = (DBNull.Value == record[Northwind_Products.colUnitsOnOrder])
                    ? Northwind_Products.cDefaultUnitsOnOrder
                    : (int)record[Northwind_Products.colUnitsOnOrder];
            }
            catch { }

            try
            {
                p.ReorderLevel = (DBNull.Value == record[Northwind_Products.colReorderLevel])
                    ? Northwind_Products.cDefaultReorderLevel
                    : (int)record[Northwind_Products.colReorderLevel];
            }
            catch { }

            try
            {
                p.Discontinued = (DBNull.Value == record[Northwind_Products.colDiscontinued])
                    ? Northwind_Products.cDefaultDiscontinued
                    : (bool)record[Northwind_Products.colDiscontinued];
            }
            catch { }

            return p;
        }
    }

    class NorthwindReader_Products : ObjectReaderWithConnection<Northwind_Products>
    {
        public override DatabaseTechnology DbTechnology
        {
            get { return m_tech; }
            set { m_tech = value; }
        }

        public override string ConnectionString
        {
            get { return m_connectionString; }
            set { m_connectionString = value; }
        }

        public override string CmdText
        {
            //sqlQuery = (
            //    "SELECT ProductID, UnitPrice, ProductName FROM Products " +
            //    "WHERE UnitPrice > ? " +
            //    "ORDER BY UnitPrice DESC;");
            get { return m_cmdText; }
            set { m_cmdText = value; }
        }

        public override CommandType CmdType
        {
            // Available command types are:
            // * Text (standard SQL query) = 1,
            // * StoredProcedure
            // * TableDirect (direct table access) [appears to be a shortcut to "SELECT * FROM TABLENAME"]
            get { return m_cmdType; }
            set { m_cmdType = value; }
        }

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

        protected override MapperBase<Northwind_Products> GetMapper()
        {
            MapperBase<Northwind_Products> mapper = new NorthwindMapper_Products();
            return mapper;
        }
    }
    #endregion // Mappers, Readers
}
