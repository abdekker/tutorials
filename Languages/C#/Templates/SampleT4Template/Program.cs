using System;

namespace SampleT4Template
{
    // T4 templates allow the developer to generate program code and other files in Visual Studio.
    // This tutorial follows:
    // * https://docs.microsoft.com/en-us/visualstudio/modeling/design-time-code-generation-by-using-t4-text-templates
    // * https://docs.microsoft.com/en-us/visualstudio/modeling/run-time-text-generation-with-t4-text-templates
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("### T4 templates in C# ###");
            Console.WriteLine("  T4 templates generate output at design-time or run-time");
            Console.WriteLine();
            Console.WriteLine("Design-time");
            Console.WriteLine("  1) In Solution Explorer, right-click Add New Item > Text Template");
            Console.WriteLine("  2) Rename as appropriate");
            Console.WriteLine("  3) CustomTool property is \"TextTemplatingFileGenerator\"");
            Console.WriteLine("  4) Design-time template runs when saved, edited, or manually invoked");
            Console.WriteLine();
            Console.WriteLine("Run-time");
            Console.WriteLine("  1) In Solution Explorer, right-click Add New Item > Runtime Text Template");
            Console.WriteLine("  2) Rename as appropriate");
            Console.WriteLine("  3) CustomTool property is \"TextTemplatingFilePreprocessor\"");
            Console.WriteLine("  4) Design-time template runs when saved, edited, or manually invoked");
            Console.WriteLine();
            Console.WriteLine("Convert an existing file to a runtime template (example is .html)");
            Console.WriteLine("  1) Create plain HTML file and confirm it works as expected");
            Console.WriteLine("  2) Include item in project with Solution Explorer > Add > Existing Item");
            Console.WriteLine("  3) Set the CustomTool property to \"TextTemplatingFilePreprocessor\"");
            Console.WriteLine("  4) Rename extension to .tt (this is optional but is the convention)");
            Console.WriteLine("Note: A .cs file is auto-generated, which is used to create the content and is required");
            Console.WriteLine();

            Console.WriteLine("Generating the web page...find it in $(OutDir)");
            TemplateRunTime page = new TemplateRunTime();
            String pageContent = page.TransformText();
            System.IO.File.WriteAllText("outputPage.html", pageContent);
            Console.WriteLine();

            Console.WriteLine("All done!");
        }
    }
}
