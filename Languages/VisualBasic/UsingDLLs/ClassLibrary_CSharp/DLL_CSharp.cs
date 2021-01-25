using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ClassLibrary_CSharp
{
    public class DLL_CSharp
    {
        // Automatically implemented property
        public int MyID { get; set; }

        // Constructor / destructor
        public DLL_CSharp()
        {
            // Constructor which takes no parameters
            Console.WriteLine("    (DLL_CSharp constructor; no parameters)");
            SetMyID();
        }

        public DLL_CSharp(ref string param1, ref int param2)
        {
            // Constructor which takes two parameters
            Console.WriteLine("    (DLL_CSharp constructor with parameters; param1 = '{0}', param2 = {1})", param1, param2);
            SetMyID();
        }

        ~DLL_CSharp()
        {
            Console.WriteLine("    (DLL_CSharp destructor ({0}))", MyID);
        }

        // Test methods
        public void TestMethod1()
        {
            Console.WriteLine("    (DLL_CSharp::TestMethod1; no parameters)");
        }

        public void TestMethod2(ref string param1, ref int param2)
        {
            Console.WriteLine("    (DLL_CSharp::TestMethod2; param1 = '{0}', param2 = {1})", param1, param2);
        }

        // Helper method
        private void SetMyID()
        {
            // Set an unique ID to identify this DLL when it is destroyed
            Random randm = new Random(Environment.TickCount);
            MyID = randm.Next(1000,9999);
        }
    }
}
