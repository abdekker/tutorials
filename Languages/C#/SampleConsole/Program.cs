using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;

using Microsoft.Win32;
using systemHelperLibrary;

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
        private static string GetAssemblyPath
        {
            get
            {
                var assemblyPath = Assembly.GetEntryAssembly().Location;
                return assemblyPath.ToString();
            }
        }

        private static string GetAssemblyDirectory
        {
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
                        dotNetInstalled += ($"    {versionKeyName}  {name}" + Environment.NewLine);
                    }
                    else if (install == "1")
                    {
                        // Install = 1 means the version is installed
                        if (!string.IsNullOrEmpty(sp))
                            dotNetInstalled += ($"    {versionKeyName}  {name}  SP{sp}" + Environment.NewLine);
                        else
                            dotNetInstalled += ($"    {versionKeyName}  {name}" + Environment.NewLine);
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
                            dotNetInstalled += ($"    {versionKeyName}  {name}" + Environment.NewLine);
                        }
                        else if (install == "1")
                        {
                            // Install = 1 means the version is installed
                            if (!string.IsNullOrEmpty(sp))
                                dotNetInstalled += ($"    {subKeyName}  {name}  SP{sp}" + Environment.NewLine);
                            else
                                dotNetInstalled += ($"    {subKeyName}  {name}" + Environment.NewLine);
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
                        $"    .NET Framework Version: {CheckFor45PlusVersion((int)ndpKey.GetValue("Release"))}" +
                        Environment.NewLine);
                }
                else
                {
                    dotNetInstalled += ("    .NET Framework Version 4.5 or later is not detected." + Environment.NewLine);
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

        private static string GetDotNetByConditionalCompilation()
        {
            // This method was adapted from:
            // https://stackoverflow.com/questions/42754123
            // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/preprocessor-directives/preprocessor-if
            // Note: According to the latter reference:
            //      "For traditional, non-SDK-style projects, you have to manually configure the
            //      conditional compilation symbols for the different target frameworks in Visual
            //      Studio via the project's properties pages."
            // In other words, these symbols may not be defined!
            string strDotNetFramework = "(unknown)";
            string strDotNetVersion = string.Empty;

            #region List of .NET conditional compilation flags
            /* 
            .NET Framework
                FRAMEWORK/VERSION               SYMBOL
                --------------------------------------
                .NET Framework (any version)    NETFRAMEWORK
                .NET Framework 2.0              NET20
                .NET Framework 3.5              NET35
                .NET Framework 4.0              NET40
                .NET Framework 4.5              NET45
                .NET Framework 4.5.1            NET451
                .NET Framework 4.5.2            NET452
                .NET Framework 4.6              NET46
                .NET Framework 4.6.1            NET461
                .NET Framework 4.6.2            NET462
                .NET Framework 4.7              NET47
                .NET Framework 4.7.             NET471
                .NET Framework 4.7.2            NET472
                .NET Framework 4.8              NET48

            .NET Standard
                FRAMEWORK/VERSION               SYMBOL
                --------------------------------------
                .NET Standard (any version)     NETSTANDARD
                .NET Standard 1.0               NETSTANDARD1_0
                .NET Standard 1.1               NETSTANDARD1_1
                .NET Standard 1.2               NETSTANDARD1_2
                .NET Standard 1.3               NETSTANDARD1_3
                .NET Standard 1.4               NETSTANDARD1_4
                .NET Standard 1.5               NETSTANDARD1_5
                .NET Standard 1.6               NETSTANDARD1_6
                .NET Standard 2.0               NETSTANDARD2_0
                .NET Standard 2.1               NETSTANDARD2_1

            .NET Core
                FRAMEWORK/VERSION               SYMBOL
                --------------------------------------
                .NET [Core] (any version)       NETCOREAPP
                .NET Core 1.0                   NETCOREAPP1_0
                .NET Core 1.1                   NETCOREAPP1_1
                .NET Core 2.0                   NETCOREAPP2_0
                .NET Core 2.1                   NETCOREAPP2_1
                .NET Core 2.2                   NETCOREAPP2_2
                .NET Core 3.0                   NETCOREAPP3_0
                .NET Core 3.1                   NETCOREAPP3_1

            .NET 5+
                FRAMEWORK/VERSION               SYMBOL
                --------------------------------------
                .NET 5.0                        NET5_0
            */
             #endregion // List of .NET conditional compilation flags

            // .NET Framework
            #if NETFRAMEWORK
                strDotNetFramework = ".NET Framework"); 
            #endif
            #if NET20
                strDotNetVersion = "2.0"); 
            #endif
            #if NET35
                strDotNetVersion = "3.5"); 
            #endif
            #if NET40
                strDotNetVersion = "4.0"); 
            #endif
            #if NET45
                strDotNetVersion = "4.5"); 
            #endif
            #if NET451
                strDotNetVersion = "4.5.1"); 
            #endif
            #if NET452
                strDotNetVersion = "4.5.2"); 
            #endif
            #if NET46
                strDotNetVersion = "4.6"); 
            #endif
            #if NET461
                strDotNetVersion = "4.6.1"); 
            #endif
            #if NET462
                strDotNetVersion = "4.6.2"); 
            #endif
            #if NET47
                strDotNetVersion = "4.7"); 
            #endif
            #if NET471
                strDotNetVersion = "4.7.1"); 
            #endif
            #if NET472
                strDotNetVersion = "4.7.2"); 
            #endif
            #if NET48
                strDotNetVersion = "4.8"); 
            #endif

            #if NETSTANDARD
                strDotNetFramework = ".NET Standard"); 
            #endif
            #if NETSTANDARD1_0
                strDotNetVersion = "1.0"); 
            #endif
            #if NETSTANDARD1_1
                strDotNetVersion = "1.1"); 
            #endif
            #if NETSTANDARD1_2
                strDotNetVersion = "1.2"); 
            #endif
            #if NETSTANDARD1_3
                strDotNetVersion = "1.3"); 
            #endif
            #if NETSTANDARD1_4
                strDotNetVersion = "1.4"); 
            #endif
            #if NETSTANDARD1_5
                strDotNetVersion = "1.5"); 
            #endif
            #if NETSTANDARD1_6
                strDotNetVersion = "1.6"); 
            #endif
            #if NETSTANDARD2_0
                strDotNetVersion = "2.0"); 
            #endif
            #if NETSTANDARD2_1
                strDotNetVersion = "2.1"); 
            #endif

            #if NETCOREAPP
                strDotNetFramework = ".NET Core"); 
            #endif
            #if NETCOREAPP1_0
                strDotNetVersion = "1.0"); 
            #endif
            #if NETCOREAPP1_1
                strDotNetVersion = "1.1"); 
            #endif
            #if NETCOREAPP2_0
                strDotNetVersion = "2.0"); 
            #endif
            #if NETCOREAPP2_1
                strDotNetVersion = "2.1"); 
            #endif
            #if NETCOREAPP2_2
                strDotNetVersion = "2.2"); 
            #endif
            #if NETCOREAPP3_0
                strDotNetVersion = "3.0"); 
            #endif
            #if NETCOREAPP3_1
                strDotNetVersion = "3.1"); 
            #endif

            return ("    .NET Framework and version = " + strDotNetFramework + " " + strDotNetVersion);
        }

        private static void DisplayAssemblyInfo()
        {
            Console.WriteLine("# Assembly Information #");
            Console.WriteLine(String.Format("  Assembly full path: {0}", GetAssemblyPath));
            Console.WriteLine(String.Format("  Assembly directory: {0}", GetAssemblyDirectory));
            #if DEBUG
                Console.WriteLine("  Assembly built in DEBUG mode");
            #else
                Console.WriteLine("  Assembly built in RELEASE mode");
            #endif
            Console.WriteLine();
        }

        private static void DisplayDotNetInfo()
        {
            Console.WriteLine("# .NET Version Information #");
            Console.WriteLine($"  .NET environment version: {Environment.Version}");
            Console.WriteLine("");
            Console.WriteLine("  Older .NET frameworks installed (v1-v4):");
            Console.WriteLine(GetDotNetInstalledOlder());
            Console.WriteLine("  Recent .NET frameworks installed (v4.5 and later):");
            Console.WriteLine(GetDotNetInstalledNewer());
            Console.WriteLine("  Conditional compilation to detect the .NET version being targetted:");
            Console.WriteLine(GetDotNetByConditionalCompilation());

            // In C/C++, there are pre-defined preprocessor macros such as "_MSC_VER" which give you the version of the
            // compiler at compile-time. These are not available in C# (because there is no preprocessor).
            Console.WriteLine();
        }

        private static void DisplaySystemInformation()
        {
            Console.WriteLine("# System Information #");
            Console.WriteLine("  OS Description\t\t{0} ({1})", RuntimeInformation.OSDescription, SystemLibrary.GetOSName());
            Console.WriteLine("  OS Architecture\t\t{0}", RuntimeInformation.OSArchitecture.ToString());
            Console.WriteLine("  Process Architecture\t\t{0}", RuntimeInformation.ProcessArchitecture.ToString());
            Console.WriteLine("  Framework Description\t\t{0}", RuntimeInformation.FrameworkDescription);
            Console.WriteLine();
        }

        // Main entry point for the console application
        static void Main(string[] args)
        {
            // Sample console application in C#
            System.Runtime.Versioning.TargetFrameworkAttribute targetFramework =
                (System.Runtime.Versioning.TargetFrameworkAttribute)Assembly.GetExecutingAssembly().GetCustomAttributes(
                    typeof(System.Runtime.Versioning.TargetFrameworkAttribute), false).SingleOrDefault();
            string msgWelcome = String.Format("### C# console application, targetting {0} ###", targetFramework.FrameworkDisplayName);
            Console.WriteLine(msgWelcome);
            Console.WriteLine();

            // Display some system information
            DisplaySystemInformation();

            // Display information about .NET versions installed
            DisplayDotNetInfo();

            // Show some information about this assembly
            DisplayAssemblyInfo();

            // Show the arguments passed to this console application
            Console.WriteLine("# Check for arguments #");
            if (args.Length == 0)
                Console.WriteLine("  No arguments supplied...");
            else
            {
                // At least one argument supplied...
                Console.WriteLine("  Number of arguments passed: {0}", args.Length);

                // Attempt to convert the first parameter to an integer
                int firstArgAsInt;
                if (int.TryParse(args[0], out firstArgAsInt))
                    Console.WriteLine("  First argument is the integer \"{0}\"", firstArgAsInt);
                else
                    Console.WriteLine("  First argument is NOT an integer");

                // Now try to convert the first parameter as a floating point number
                float firstArgAsFloat;
                if (float.TryParse(args[0], out firstArgAsFloat))
                    Console.WriteLine("  First argument is the float \"{0:0.000}\"", firstArgAsFloat);
                else
                    Console.WriteLine("  First argument is NOT a float");
            }
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
