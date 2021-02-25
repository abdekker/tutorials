using System;
using System.Data;

namespace demoDataTable
{
    class Program
    {
        // Constants
        private static readonly string colFormatter = "  {0,-5}{1,-10}{2}";
        private static readonly string dataFormatterTitle = "  {0,-5}{1,-12}{2,-12}{3}";
        private static readonly string dataFormatter = "  {0,-5}{1,-12}{2,-12:0.00}{3}";
        private static readonly string ISO8601_Date = "yyyy-MM-dd";

        private static readonly string colID = "ID";
        private static readonly string colName = "Name";
        private static readonly string colSalary = "Salary";
        private static readonly string colHireDate = "HireDate";

        private static void HelperPrintEmployeesTable(DataTable dt)
        {
            // Helper function to print a DataTable to the console
            Console.WriteLine(dataFormatterTitle, colID, colName, colSalary, colHireDate);
            Console.WriteLine("  =======================================");
            foreach (DataRow row in dt.Rows)
            {
                Console.WriteLine(dataFormatter, 
                    row[colID],
                    row[colName],
                    row[colSalary],
                    ((DateTime)row[colHireDate]).ToString(ISO8601_Date));
            }
        }

        private static int HelperSearchByName(DataTable dt, DataRow row)
        {
            for (int index=0; index < dt.Rows.Count; index++)
            {
                if (dt.Rows[index][colName] == row[colName])  // Or "if (dt.Rows[index][colName].Equals(row[colName]))"
                    return index;
            }

            return -1;
        }

        private static int HelperSearchGeneric(DataTable dt, DataRow row, string[] columns)
        {
            bool columnMatch;
            for (int index=0; index < dt.Rows.Count; index++)
            {
                columnMatch = true;
                foreach (string col in columns)
                {
                    if (!dt.Rows[index][col].Equals(row[col]))
                    {
                        columnMatch = false;
                        break;
                    }
                }

                if (columnMatch)
                    return index;
            }

            return -1;
        }

        private static void HelperPrintSelectQuery(DataTable dt, string query, string sortOrder)
        {
            // Run a select query against the DataTable
            Console.Write("  Search: {0}, ", query);
            if (!string.IsNullOrEmpty(sortOrder))
                Console.Write("Sorting Order: {0}, ", sortOrder);

            DataRow[] rows = (string.IsNullOrEmpty(sortOrder))
                ? dt.Select(query)
                : dt.Select(query, sortOrder);

            if (rows != null && rows.Length > 0)
            {
                Console.WriteLine("Rows found: " + rows.Length);
                foreach (DataRow rw in rows)
                    Console.WriteLine("    " + rw.Field<string>(colName));
            }
            else
                Console.WriteLine("No rows found");

            Console.WriteLine();
        }

        private static void HelperPopulateDataTable(ref DataTable dt)
        {
            // Create a test table which can be used in any method for this demonstration program

            // Add columns to the table
            dt.Columns.Add(colID, typeof(int));
            dt.Columns.Add(colName, typeof(string));
            dt.Columns.Add(colSalary, typeof(double));
            dt.Columns.Add(colHireDate, typeof(DateTime));

            // Add rows to the table
            dt.Rows.Add(52, "Sally", 29000.0, "1988-12-15");
            dt.Rows.Add(63, "Harry", 22000.5, "2011-02-17");
            dt.Rows.Add(72, "Audrey", 23000.0, "2011-02-17");   // Duplicate hire date
            dt.Rows.Add(72, "Alain", 22000.5, "2006-07-29");    // Duplicate ID and Salary

            // Add row using DataTable::NewRow (creates a new DataRow with the same schema as the table)
            DataRow employee = dt.NewRow();
            employee[colID] = 110;
            employee[colName] = "Pete";
            employee[colSalary] = 24500.0;
            employee[colHireDate] = DateTime.Now.ToString(ISO8601_Date);
            dt.Rows.Add(employee);
        }

        private static void DataTable_Basic()
        {
            Console.WriteLine("# Basic usage of DataTable #");
            Console.WriteLine("  DataTable represents a collection of fields (DataColumn) and data (DataRow)");
            Console.WriteLine();

            // Manually create a new DataTable
            DataTable dtEmployees = new DataTable("Employee");
            HelperPopulateDataTable(ref dtEmployees);

            // Display data about the table
            Console.WriteLine("(basic statistics)");
            Console.WriteLine("  {0,-20}: {1} [using DataTable::Columns::Count]", "Number of columns", dtEmployees.Columns.Count);
            Console.WriteLine("  {0,-20}: {1} [using DataTable::Rows::Count]", "Number of rows", dtEmployees.Rows.Count);

            Console.WriteLine();
            Console.WriteLine("(field / column details)");
            Console.WriteLine(colFormatter, "#", "Name", "Data type");
            Console.WriteLine("  ==============================");
            int index = 1;
            foreach (DataColumn col in dtEmployees.Columns)
            {
                Console.WriteLine(colFormatter, index++, col.ColumnName, col.DataType);
            }
            Console.WriteLine();

            Console.WriteLine("(current rows in DataTable)");
            HelperPrintEmployeesTable(dtEmployees);
            Console.WriteLine();

            Console.WriteLine("(using DataTable.Compute)");
            Console.WriteLine("  {0,-20}: {1}", "Employee Count", dtEmployees.Compute("Count(ID)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Maximum Salary", dtEmployees.Compute("Max(Salary)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Minimum Salary", dtEmployees.Compute("Min(Salary)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Average Salary", dtEmployees.Compute("Avg(Salary)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Sum Salary", dtEmployees.Compute("Sum(Salary)", string.Empty));
            Console.WriteLine();

            Console.WriteLine("(edit one of the rows - let's give {0} a $5000 raise)", dtEmployees.Rows[1][colName]);
            dtEmployees.Rows[1].BeginEdit();
            dtEmployees.Rows[1][colSalary] = ((double)dtEmployees.Rows[1][colSalary] + 5000.0);
            dtEmployees.Rows[1].EndEdit();
            HelperPrintEmployeesTable(dtEmployees);
            Console.WriteLine("#\n");
        }

        private static void DataTable_Search_Iterate()
        {
            Console.WriteLine("# Search for records by iterating manually #");
            Console.WriteLine();

            // Manually create a new DataTable
            DataTable dtEmployees = new DataTable("Employee");
            HelperPopulateDataTable(ref dtEmployees);
            DataRow employee;

            Console.WriteLine("(search for DataRow by name - 1st record found only)");
            int index;
            string[] testRowByName = new string[3];
            testRowByName[0] = "Zorro";
            testRowByName[1] = (string)dtEmployees.Rows[1][colName];
            testRowByName[2] = ((string)dtEmployees.Rows[2][colName]).ToLower();
            foreach (string name in testRowByName)
            {
                Console.Write("  " + name.PadRight(12));
                employee = dtEmployees.NewRow();
                employee[colName] = name;
                index = HelperSearchByName(dtEmployees, employee);
                if (index != -1)
                    Console.WriteLine(": Employee found at index {0}", index);
                else
                    Console.WriteLine(": Employee not found...");
            }
            Console.WriteLine();

            Console.WriteLine("(generic search for DataRow - 1st record found only)");
            Console.WriteLine("(Note: This iterates through all records in the DataTable");
            string[] testRowGenericColums = { colID, colSalary };
            int[] testRowGenericIDs = {
                99,
                (int)dtEmployees.Rows[2][colID],
                (int)dtEmployees.Rows[2][colID] };
            double[] testRowGenericSalaries = {
                12345.67,
                (double)dtEmployees.Rows[2][colSalary],
                987.6 };

            for (int tmp = 0; tmp < testRowGenericIDs.Length; tmp++)
            {
                Console.Write("  ID={0}, Salary={1,-8:0.00} ", testRowGenericIDs[tmp], testRowGenericSalaries[tmp]);
                employee = dtEmployees.NewRow();
                employee[colID] = testRowGenericIDs[tmp];
                employee[colSalary] = testRowGenericSalaries[tmp];
                index = HelperSearchGeneric(dtEmployees, employee, testRowGenericColums);
                if (index != -1)
                    Console.WriteLine(": Employee found at index {0}", index);
                else
                    Console.WriteLine(": Employee not found...");
            }
            Console.WriteLine("#\n");
        }

        private static void DataTable_Search_Select()
        {
            Console.WriteLine("# Searching for records using DataTable::Select #");
            Console.WriteLine("(Note: DataTable::DataRowsCollection::IndexOf only finds rows that are already in the collection)");
            Console.WriteLine();

            // Manually create a new DataTable
            DataTable dtEmployees = new DataTable("Employee");
            HelperPopulateDataTable(ref dtEmployees);

            Console.WriteLine("(using DataTable::Select)");
            Console.WriteLine("(simply query for a number => just use the number as is)");
            string query = string.Format("{0} = 72", colID);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(run the same query, but apply a sorting filter");
            string sortOrder = string.Format("{0} ASC", colSalary);
            HelperPrintSelectQuery(dtEmployees, query, sortOrder);

            Console.WriteLine("(search by date => enclose date with '#' symbols or use a string literal)");
            query = string.Format("{0} < #01/01/2000#", colHireDate);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            query = string.Format("{0} < '01/01/2000'", colHireDate);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search for two criteria with 'Or')");
            query = string.Format("{0} = 72 OR {1} < #01/01/2000#", colID, colHireDate);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search for two criteria with 'And')");
            query = string.Format("{0} = 72 AND {1} < #01/01/2000#", colID, colHireDate);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search using the NOT EQUAL '<>' operator)");
            query = string.Format("{0} <> '{1}'", colName, (string)dtEmployees.Rows[3][colName]);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search using the IN operator => select from a range of values)");
            query = string.Format("{0} IN (52, 110)", colID);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            query = string.Format("{0} IN ('{1}', 'Dracula', '{2}')",
                colName,
                (string)dtEmployees.Rows[0][colName],
                (string)dtEmployees.Rows[2][colName]);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            query = query.Replace("IN", "NOT IN");
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search using the LIKE operator)");
            Console.WriteLine("  (value starts with \"A\")");
            query = string.Format("{0} LIKE 'A*'", colName);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            Console.WriteLine("  (Value does not starts with \"A\")");
            query = query.Replace("LIKE", "NOT LIKE");
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            Console.WriteLine("  (Value contains \"y\")");
            query = string.Format("{0} LIKE '%y%'", colName);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search using arithmetic operators)");
            Console.WriteLine("  (anyone with a salary that will go over 30k if we give them a 10% raise)");
            query = string.Format("{0} * 1.1 > 30000.0", colSalary);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            Console.WriteLine("  (anyone with an odd ID)");
            query = string.Format("{0} % 2 <> 0", colID);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);

            Console.WriteLine("(search using aggregate functions: SUM, COUNT, MIN, MAX, AVG, STDEV and VAR)");
            Console.WriteLine("  (value with below average salary)");
            query = string.Format("{0} < AVG({0})", colSalary);
            HelperPrintSelectQuery(dtEmployees, query, string.Empty);
            Console.WriteLine("#\n");
        }

        static void Main()
        {
            // Create a DbTester class
            Console.WriteLine("### DataTable and DataSet ###");
            Console.WriteLine();

            DataTable_Basic();
            DataTable_Search_Iterate();
            DataTable_Search_Select();

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }
    }
}
