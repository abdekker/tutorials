Imports ClassLibrary_VB
Imports ClassLibrary_CSharp

Module ConsoleUsingDLLs

    ' Automatically implemented property
    Private Property IntParam As Integer

    #Region "Methods"
    Private Sub InternalMethod1()
        Console.WriteLine("### Internal method 1 ###")
        Console.WriteLine("  (no parameters)")
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub InternalMethod2(param1 As String, param2 As Integer)
        Console.WriteLine("### Internal method 2 ###")
        Console.WriteLine("  (param1 = '{0}', param2 = {1})", param1, param2)
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub ExternalMethod_VB()
        Console.WriteLine("### External method 1 - Calling into a Visual Basic DLL ###")

        System.Threading.Thread.Sleep(50)
        Console.WriteLine("  Use external constructor without any parameters")
        Dim dll_VBa As DLL_VB = New DLL_VB()
        dll_VBa.TestMethod1()
        dll_VBa.TestMethod2("external" & IntParam, IntParam)
        IntParam = IntParam + 1
        Console.WriteLine()

        System.Threading.Thread.Sleep(50)
        Console.WriteLine("  Use external constructor with parameters")
        Dim dll_VBb As DLL_VB = New DLL_VB("external" & IntParam, IntParam)
        IntParam = IntParam + 1
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub ExternalMethod_CSharp()
        Console.WriteLine("### External method 2 - Calling into a C# DLL ###")

        System.Threading.Thread.Sleep(50)
        Console.WriteLine("  Use external constructor without any parameters")
        Dim dll_CSharpa As DLL_CSharp = New DLL_CSharp()
        dll_CSharpa.TestMethod1()
        dll_CSharpa.TestMethod2("external" & IntParam, IntParam)
        IntParam = IntParam + 1
        Console.WriteLine()

        System.Threading.Thread.Sleep(50)
        Console.WriteLine("  Use external constructor with parameters")
        Dim dll_CSharpb As DLL_CSharp = New DLL_CSharp("external" & IntParam, IntParam)
        IntParam = IntParam + 1
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub
    #End Region ' Methods

    Sub Main()
        ' This application is a Console App (.NET Framework) (in Visual Basic)
        ' To add new projects to this solution:
        ' * Right-click the solution in Solution Explorer and select Add > New Project...
        ' * Select "Class Library (.NET Framework) (in required language)

        ' * Visual Basic Class Library:
        '   - Right-click the DLL project in Solution Explorer and select properties
        '   - Application: Note that the Application Type is set to Class Library
        '   - Compile: Leave the Build output folder set to "bin\Debug\" or "bin\Release\"
        '   - Compile > Build Events (lower right): Set the post-build event to something like "copy $(TargetDir)$(TargetFileName) ..\..\..\"

        ' * C# Class Library:
        '   - Right-click the DLL project in Solution Explorer and select properties
        '   - Application: Note that the Application Type is set to Class Library
        '   - Build: Leave the Output path set to "bin\Debug\" or "bin\Release\"
        '   - Build Events: Set the post-build event to something like "copy $(TargetDir)$(TargetFileName) ..\..\..\"

        IntParam = 1
        Console.WriteLine("Using DLLs in Visual Basic")
        Console.WriteLine("DLLs are called 'Class libraries' in .NET")
        Console.WriteLine()

        ' Internal methods
        InternalMethod1()
        InternalMethod2("internal" & IntParam, IntParam)
        IntParam = IntParam + 1

        ' External methods
        ExternalMethod_VB()         ' Visual Basic DLL
        ExternalMethod_CSharp()     ' C# DLL

        ' All done
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub

End Module
