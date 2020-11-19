using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;

using Microsoft.Win32;

// By default, the "Console App (.NET Framework)" template adds "using" classes for:
//      System.Collections.Generic
//      System.Text
//      System.Threading.Tasks

// Console application written in Visual Studio 2019 and C# targetting .NET 4.7.2
namespace SampleConsole
{
    class Program
    {
        // Property accessors
        private static string ExecutablePath
        {
            get;
            set;
        }

        private static string GetAssemblyPath
        {
            set { }
            get
            {
                var assemblyPath = Assembly.GetEntryAssembly().Location;
                return assemblyPath.ToString();
            }
        }

        private static string GetAssemblyDirectory
        {
            set { }
            get
            {
                var assemblyPath = Assembly.GetEntryAssembly().Location;
                var assemblyDirectory = Path.GetDirectoryName(assemblyPath);
                return assemblyDirectory.ToString();
            }
        }

        // Methods
        // Note: The code to check for .NET installed versions has been adapted from:
        // https://docs.microsoft.com/en-us/dotnet/framework/migration-guide/how-to-determine-which-versions-are-installed
        private static string GetDotNetInstalledOlder()
        {
            // Check for v4.0 and older .NET versions
            string dotNetInstalled = string.Empty;

            RegistryKey ndpKey =
                RegistryKey.OpenBaseKey(RegistryHive.LocalMachine, RegistryView.Registry32).
                OpenSubKey(@"SOFTWARE\Microsoft\NET Framework Setup\NDP\");
            foreach (var versionKeyName in ndpKey.GetSubKeyNames())
            {
                // Skip .NET Framework 4.5 version information
                if (versionKeyName == "v4")
                    continue;

                if (versionKeyName.StartsWith("v"))
                {
                    RegistryKey versionKey = ndpKey.OpenSubKey(versionKeyName);

                    // Get the .NET Framework version value and service pack (SP) number
                    var name = (string)versionKey.GetValue("Version", "");
                    var sp = versionKey.GetValue("SP", "").ToString();

                    // Get the installation flag
                    var install = versionKey.GetValue("Install", "").ToString();
                    if (string.IsNullOrEmpty(install))
                    {
                        // No install info; it must be in a child subkey
                        dotNetInstalled += ($"{versionKeyName}  {name}" + Environment.NewLine);
                    }
                    else if (install == "1")
                    {
                        // Install = 1 means the version is installed
                        if (!string.IsNullOrEmpty(sp))
                            dotNetInstalled += ($"{versionKeyName}  {name}  SP{sp}" + Environment.NewLine);
                        else
                            dotNetInstalled += ($"{versionKeyName}  {name}" + Environment.NewLine);
                    }

                    if (!string.IsNullOrEmpty(name))
                    {
                        continue;
                    }

                    // Iterate through the subkeys of the version subkey
                    foreach (var subKeyName in versionKey.GetSubKeyNames())
                    {
                        RegistryKey subKey = versionKey.OpenSubKey(subKeyName);
                        name = (string)subKey.GetValue("Version", "");
                        if (!string.IsNullOrEmpty(name))
                            sp = subKey.GetValue("SP", "").ToString();

                        install = subKey.GetValue("Install", "").ToString();
                        if (string.IsNullOrEmpty(install))
                        {
                            // No install info
                            dotNetInstalled += ($"{versionKeyName}  {name}" + Environment.NewLine);
                        }
                        else if (install == "1")
                        {
                            // Install = 1 means the version is installed
                            if (!string.IsNullOrEmpty(sp))
                                dotNetInstalled += ($"{subKeyName}  {name}  SP{sp}" + Environment.NewLine);
                            else
                                dotNetInstalled += ($"  {subKeyName}  {name}" + Environment.NewLine);
                        }
                    }
                }
            }

            return dotNetInstalled;
        }

        private static string GetDotNetInstalledNewer()
        {
            // Check for v4.5 and newer .NET versions
            string dotNetInstalled = string.Empty;

            const string subkey = @"SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full\";
            using (var ndpKey = RegistryKey.OpenBaseKey(
                RegistryHive.LocalMachine, RegistryView.Registry32).OpenSubKey(subkey))
            {
                if (ndpKey != null && ndpKey.GetValue("Release") != null)
                {
                    dotNetInstalled += (
                        $".NET Framework Version: {CheckFor45PlusVersion((int)ndpKey.GetValue("Release"))}" +
                        Environment.NewLine);
                }
                else
                {
                    dotNetInstalled += (".NET Framework Version 4.5 or later is not detected." + Environment.NewLine);
                }
            }

            return dotNetInstalled;
        }

        private static string CheckFor45PlusVersion(int releaseKey)
        {
            // Helper function for "GetDotNetInstalledNewer"
            if (releaseKey >= 528040)
                return "4.8 or later";
            if (releaseKey >= 461808)
                return "4.7.2";
            if (releaseKey >= 461308)
                return "4.7.1";
            if (releaseKey >= 460798)
                return "4.7";
            if (releaseKey >= 394802)
                return "4.6.2";
            if (releaseKey >= 394254)
                return "4.6.1";
            if (releaseKey >= 393295)
                return "4.6";
            if (releaseKey >= 379893)
                return "4.5.2";
            if (releaseKey >= 378675)
                return "4.5.1";
            if (releaseKey >= 378389)
                return "4.5";

            // This code should never execute. A non-null release key should mean that 4.5 or later is installed.
            return "No 4.5 or later version detected";
        }

        // Main entry point for the console application
        static void Main(string[] args)
        {
            // Welcome message about how this application was created
            System.Runtime.Versioning.TargetFrameworkAttribute targetFramework =
                (System.Runtime.Versioning.TargetFrameworkAttribute)Assembly.GetExecutingAssembly().GetCustomAttributes(
                    typeof(System.Runtime.Versioning.TargetFrameworkAttribute), false).SingleOrDefault();
            string msgWelcome = String.Format("### C# console application, targetting {0} ###", targetFramework.FrameworkDisplayName);
            Console.WriteLine(msgWelcome);
            Console.WriteLine($".NET environment version: {Environment.Version}");
            Console.WriteLine("");
            Console.WriteLine("Older .NET frameworks installed (v1-v4):");
            Console.WriteLine("  " + GetDotNetInstalledOlder());
            Console.WriteLine("More recent .NET frameworks installed (v4.5 and later):");
            Console.WriteLine("  " + GetDotNetInstalledNewer());

            // In C/C++, there are pre-defined preprocessor macros such as "_MSC_VER" which give you the version of the
            // compiler at compile-time. These are not available in C# (because there is no preprocessor).

            // Show some information about this assembly
            Console.WriteLine(String.Format("  Assembly full path: {0}", GetAssemblyPath));
            Console.WriteLine(String.Format("  Assembly directory: {0}", GetAssemblyDirectory));
            Console.WriteLine("");

            // Write a message over multiple lines
            string msgMultiLine = ("This message will..." + Environment.NewLine + "...span multiple lines" + Environment.NewLine);
            Console.WriteLine(msgMultiLine);

            // Check whether we have a debugger attached
            if (Debugger.IsAttached)
            {
                // Since there is a debugger attached, assume we are running from the IDE
                Console.WriteLine("Debugger is attached! Press any key to exit...");
            }
            else
            {
                // Assume we aren't running from the IDE
                Console.WriteLine("Debugger is NOT attached!");
            }

            //Console.WriteLine("Press any key to exit...");
            Console.ReadKey(false);
        }
    }
}
