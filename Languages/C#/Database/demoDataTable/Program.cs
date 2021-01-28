using System;
using System.Data;

namespace demoDataTable
{
    class Program
    {
        private static void HelperPrintEmployees(DataTable dt)
        {
            // Helper function to print a DataTable to the console
            Console.WriteLine("  {0,-5}{1,-12}{2}", "ID", "Name", "Salary");
            Console.WriteLine("  =========================");
            foreach (DataRow row in dt.Rows)
            {
                Console.WriteLine("  {0,-5}{1,-12}{2:0.00}", row["ID"], row["Name"], row ["Salary"]);
            }
        }

        private static bool HelperContainsName(DataTable dt, DataRow row)
        {
            for (int index=0; index < dt.Rows.Count; index++)
            {
                if (dt.Rows[index]["Name"] == row["Name"])  // Or "if (dt.Rows[index]["Name"].Equals(row["Name"]))"
                    return true;
            }

            return false;
        }

        private static void BasicDataTable()
        {
            Console.WriteLine("# Basic usage of DataTable #");
            Console.WriteLine("  DataTable represents a collection of fields (DataColumn) and data (DataRow)");
            Console.WriteLine();

            // Manually create a new DataTable
            DataTable dtEmployees = new DataTable("Employee");
            DataRow employee;

            // Adding columns
            dtEmployees.Columns.Add("ID", typeof(int));
            dtEmployees.Columns.Add("Name", typeof(string));
            dtEmployees.Columns.Add("Salary", typeof(double));

            // Adding rows to the table
            dtEmployees.Rows.Add(52, "Sally", 29000.0);
            dtEmployees.Rows.Add(63, "Harry", 22000.5);
            dtEmployees.Rows.Add(72, "Alain", 23000.0);

            // Add row using DataTable::NewRow (creates a new DataRow with the same schema as the table)
            employee = dtEmployees.NewRow();
            employee["ID"] = 110;
            employee["Name"] = "Pete";
            employee["Salary"] = 24500.0;
            dtEmployees.Rows.Add(employee);

            // Display data about the table
            Console.WriteLine("(basic statistics)");
            Console.WriteLine("  {0,-20}: {1} [using DataTable::Columns::Count]", "Number of columns", dtEmployees.Columns.Count);
            Console.WriteLine("  {0,-20}: {1} [using DataTable::Rows::Count]", "Number of rows", dtEmployees.Rows.Count);

            Console.WriteLine();
            Console.WriteLine("(field / column details)");
            Console.WriteLine("  {0,-5}{1,-10}{2}", "#", "Name", "Data type");
            Console.WriteLine("  ============================");
            int index = 1;
            foreach (DataColumn col in dtEmployees.Columns)
            {
                Console.WriteLine("  {0,-5}{1,-10}{2}", index++, col.ColumnName, col.DataType);
            }
            Console.WriteLine();

            Console.WriteLine("(current rows in DataTable)");
            HelperPrintEmployees(dtEmployees);
            Console.WriteLine();

            Console.WriteLine("(edit one of the rows - let's give {0} a $5000 raise)", dtEmployees.Rows[1]["Name"]);
            dtEmployees.Rows[1].BeginEdit();
            dtEmployees.Rows[1]["Salary"] = ((double)dtEmployees.Rows[1]["Salary"] + 5000.0);
            dtEmployees.Rows[1].EndEdit();
            HelperPrintEmployees(dtEmployees);
            Console.WriteLine();

            Console.WriteLine("(search by DataRow)");
            string[] testNames = new string[3];
            testNames[0] = "Zorro";
            testNames[1] = (string)dtEmployees.Rows[1]["Name"];
            testNames[2] = ((string)dtEmployees.Rows[2]["Name"]).ToLower();
            foreach (string name in testNames)
            {
                Console.Write("  " + name.PadRight(12));
                employee = dtEmployees.NewRow();
                employee["Name"] = name;
                if (HelperContainsName(dtEmployees, employee))
                    Console.WriteLine(": Employee found!");
                else
                    Console.WriteLine(": Employee missing...");
            }
            Console.WriteLine();

            index = 2;
            Console.WriteLine("(index of DataRow({0}) should return {0})", index);
            employee = dtEmployees.NewRow();
            employee["ID"] = dtEmployees.Rows[2]["ID"];
            employee["Name"] = dtEmployees.Rows[2]["Name"];
            employee["Salary"] = dtEmployees.Rows[2]["Salary"];
            int indexFound = dtEmployees.Rows.IndexOf(employee);
            if (indexFound != -1)
                Console.WriteLine("  Employee has index {0}", indexFound);
            else
                Console.WriteLine("  Employee not found in table...to be investigated");

            Console.WriteLine();

            Console.WriteLine("(using DataTable.Compute)");
            Console.WriteLine("  {0,-20}: {1}", "Employee Count", dtEmployees.Compute("Count(ID)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Maximum Salary", dtEmployees.Compute("Max(Salary)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Minimum Salary", dtEmployees.Compute("Min(Salary)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Average Salary", dtEmployees.Compute("Avg(Salary)", string.Empty));
            Console.WriteLine("  {0,-20}: {1:0.00}", "Sum Salary", dtEmployees.Compute("Sum(Salary)", string.Empty));
            Console.WriteLine("#\n");
        }

        static void Main()
        {
            // Create a DbTester class
            Console.WriteLine("### DataTable and DataSet ###");
            Console.WriteLine();

            BasicDataTable();

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }
    }
}
