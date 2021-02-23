// Uncomment this to show a different value in "LocalDefine"
// Note: In C#, pre-processor symbols must be defined / undefined as the first tokens in a file
#define SOMEVALUE

// Comment/uncomment these lines to undefine a pre-processor symbol
#if SOMEVALUE
    #undef SOMEVALUE
#endif

using System;

namespace ConditionalCompilation
{
    class Program
    {
        private static void DebugOrRelease()
        {
            Console.WriteLine("### DEBUG or not? ###");
            string mode;

            #if DEBUG
                mode = "DEBUG";
            #else
                mode = "RELEASE";
            #endif
            Console.WriteLine("  Assembly built in {0,-9} mode [check with '#if DEBUG']", mode);

            #if !DEBUG
                mode = "RELEASE";
            #else
                mode = "DEBUG";
            #endif
            Console.WriteLine("  Assembly built in {0,-9} mode [check with '#if !DEBUG']", mode);
            Console.WriteLine("#\n");
        }

        private static void DotNetImplementation()
        {
            Console.WriteLine("### .NET implementation details ###");
            string dotNet;

            // https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/preprocessor-directives/preprocessor-if
            // Note: According to https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/preprocessor-directives/preprocessor-if:
            //      "For traditional, non-SDK-style projects, you have to manually configure the conditional compilation
            //      symbols for the different target frameworks in Visual Studio via the project's properties pages."
            // In other words, these symbols may not be defined!
            #if NETFRAMEWORK
                dotNet = ".NET Framework";
            #elif NETSTANDARD
                dotNet = ".NET Standard";
            #elif NETCOREAPP
                dotNet = ".NET Core";
            #else
                dotNet = ".NET Unknown";
            #endif
            Console.WriteLine("  .NET implementation is '{0}'", dotNet);
            Console.WriteLine("#\n");
        }

        private static void LocalDefine()
        {
            Console.WriteLine("### Local symbol (SOMEVALUE) ###");
            #if SOMEVALUE
                Console.WriteLine("  Symbol SOMEVALUE is defined! :)");
            #else
                Console.WriteLine("  Symbol SOMEVALUE is not defined :(");
            #endif
            Console.WriteLine("#\n");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("### Exploring conditional compilation in C# ###");
            Console.WriteLine();

            DebugOrRelease();
            DotNetImplementation();
            LocalDefine();

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }
    }
}
