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
    #End Region

    Sub Main()
        #Region "About use DLLs in Visual Basic"
        ' This application is a Console App (.NET Framework) (in Visual Basic)

        ' To add new projects to this solution:
        ' * Right-click the solution in Solution Explorer and select Add > New Project...
        ' * Select "Class Library (.NET Framework) (in required language)

        ' To include the external DLLs as project references, there are two basic methods:

        ' ### METHOD 1 ###
        ' Use a post-build event to copy the output DLL to a common folder; add a reference to the DLL in the common folder
        '   * Visual Basic Class Library:
        '     - Right-click the DLL project in Solution Explorer and select properties
        '     - Application: Note that the Application Type is set to Class Library
        '     - Compile: Leave the Build output folder set to "bin\Debug\" or "bin\Release\"
        '     - Compile > Build Events (lower right): Set the post-build event to something like "copy $(TargetDir)$(TargetFileName) ..\..\..\"
        '   * C# Class Library:
        '     - Right-click the DLL project in Solution Explorer and select properties
        '     - Application: Note that the Application Type is set to Class Library
        '     - Build: Leave the Output path set to "bin\Debug\" or "bin\Release\"
        '     - Build Events: Set the post-build event to something like "copy $(TargetDir)$(TargetFileName) ..\..\..\"
        '   * Main Visual Basic project
        '       - Right-click the main project, right-click "References" and choose "Add Reference..."
        '       - In the Browse section click "Browse" and navigate to the DLL in the common folder (repeat for both)

        ' METHOD 1 adds something like the following to the main project vbproj:
        '   <Reference Include="ClassLibrary_CSharp, Version=1.0.0.0, Culture=neutral, processorArchitecture=MSIL">
        '     <SpecificVersion>False</SpecificVersion>
        '     <HintPath>.\ClassLibrary_CSharp.dll</HintPath>
        '   </Reference>

        ' ### METHOD 2 ###
        ' Add a reference to the shared project (this is probably simpler)
        '   * Visual Basic Class Library: As above, but no need to configure the post-build event (unless required)
        '   * C# Class Library: As above, but no need to configure the post-build event (unless required)
        '   * Main Visual Basic project
        '       - Right-click the main project, right-click "References" and choose "Add Reference..."
        '       - In the Projects section select both DLL projects

        ' METHOD 2 adds something like the following to the main project vbproj:
        '   <ProjectReference Include="ClassLibrary_CSharp\ClassLibrary_CSharp.csproj">
        '     <Project>{cabbc75d-8483-41f4-9019-70a6fe49dfc6}</Project>
        '     <Name>ClassLibrary_CSharp</Name>
        '   </ProjectReference>
        #End Region

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
