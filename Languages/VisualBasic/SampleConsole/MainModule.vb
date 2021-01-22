﻿Imports System.IO
Imports System.Reflection
Imports System.Runtime.Versioning
Imports System.Runtime.InteropServices
Imports System.Linq

Imports systemHelperLibrary

' To create this console application in Visual Studio 2019:
' * Create a new project
' * Set language to "Visual Basic" and type "console" in the search bar
' * Select "Console App (.NET Framework)", then Next
' * Set the project name and location, then Create

' To change the name of the starting module (from Module1):
' * Change the module name (eg. "MainModule")
' * Open project properties > Application
' * Set "Startup object" to "MainModule"

' To set build events on a project (Project properties > Build Events in C#)
' * Project properties > Compile
' * In the lower right, click "Build Events"

Module MainModule
#Region "Enumerations"
    Public Enum Weekdays
        Monday = 0
        Tuesday
        Wednesday
        Thursday
        Friday
        Saturday
        Sunday
    End Enum

    <Flags()>
    Public Enum EnumWithFlags
        First = 1
        Second = 2
        Third = 4
        Fourth = 8
    End Enum
#End Region

    ' Property accessors
    Public ReadOnly Property OSType As String
        Get
            If (RuntimeInformation.IsOSPlatform(OSPlatform.OSX)) Then
                Return "MacOS"
            ElseIf (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)) Then
                Return "Linux"
            ElseIf (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) Then
                Return "Windows"
            End If

            Return "Unknown"
        End Get
    End Property

    Public ReadOnly Property GetAssemblyDirectory As String
        Get
            Dim assemblyPath As String = Assembly.GetEntryAssembly().Location
            Dim assemblyDirectory As String = Path.GetDirectoryName(assemblyPath)
            Return assemblyDirectory
        End Get
    End Property

    ' Methods
    Private Function GetFrameworkDisplayName()
        Dim attribs() As Object = Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(TargetFrameworkAttribute), False)
        Dim targetFramework As TargetFrameworkAttribute = attribs.SingleOrDefault() ' "SingleOrDefault" requires System.Linq
        GetFrameworkDisplayName = targetFramework.FrameworkDisplayName
    End Function

    Private Sub DisplaySystemInformation()
        ' See C#\StringsDemo\DisplaySystemInformation
        Console.WriteLine("# System Information #")
        Console.WriteLine("  OS Description           {0} ({1})", RuntimeInformation.OSDescription, OSType)
        Console.WriteLine("  OS Architecture          {0}", RuntimeInformation.OSArchitecture.ToString())
        Console.WriteLine("  Process Architecture     {0}", RuntimeInformation.ProcessArchitecture.ToString())
        Console.WriteLine("  Framework Description    {0}", RuntimeInformation.FrameworkDescription)
        Console.WriteLine()
    End Sub

    Private Sub DisplayAssemblyInfo()
        ' See C#\StringsDemo\DisplayAssemblyInfo
        Console.WriteLine("# Assembly Information #")
        Console.WriteLine(String.Format("  Assembly full path: {0}", Assembly.GetEntryAssembly().Location))
        Console.WriteLine(String.Format("  Assembly directory: {0}", GetAssemblyDirectory))
        #if DEBUG then
            Console.WriteLine("  Assembly built in DEBUG mode")
        #else
            Console.WriteLine("  Assembly built in RELEASE mode")
        #end if
        Console.WriteLine()
    End Sub

    Private Sub DisplayEnumInfo()
        ' See C#\StringsDemo\Info_Enum
        Console.WriteLine("# Enumerations #")
        Console.WriteLine("  Use [Enum] or System.Enum")
        Console.WriteLine("  Square brackets indicate to the compiler that ""Enum"" should be intepreted as a type (even though it is a keyword)")
        Console.WriteLine()

        Console.WriteLine("(Weekdays contains {0} values)", [Enum].GetNames(GetType(Weekdays)).Length)
        For Each day As Weekdays In [Enum].GetValues(GetType(Weekdays))
            Console.WriteLine("  {0,2}  {1}", DirectCast(day, Int32), day)
        Next day
        Console.WriteLine()

        Console.WriteLine("(integer -> enum using Enum.IsDefined)")
        Dim dayNums = New Integer() {2, 3, -1, 8}
        For Each dayNum As Int32 In dayNums
            If [Enum].IsDefined(GetType(Weekdays), dayNum) Then
                Console.WriteLine("  {0,-10} converts to {1}", dayNum, DirectCast(dayNum, Weekdays))
            Else
                Console.WriteLine("  {0,-10} is not an underlying value of Weekday", dayNum)
            End If
        Next dayNum
        Console.WriteLine()

        Console.WriteLine("(integer -> enum using naming hack)")
        For Each dayNum As Int32 In dayNums
            If (TypesLibrary.IsValidEnumValue(DirectCast(dayNum, Weekdays)))
                Console.WriteLine("  {0,-10} converts to {1}", dayNum, DirectCast(dayNum, Weekdays))
            Else
                Console.WriteLine("  {0,-10} is not an underlying value of Weekday", dayNum)
            End If
        Next dayNum
        Console.WriteLine()

        Console.WriteLine("(string -> enum)")
        Dim dayStrings = New String() {"2", "Friday", "8", "Blue"}
        Dim tmp As String
        For Each dayString As String In dayStrings
            tmp = String.Format("'{0}'", dayString)
            Dim day As Weekdays
            If ([Enum].TryParse(dayString, True, day)) Then
                If ([Enum].IsDefined(GetType(Weekdays), day) Or day.ToString().Contains(","))
                    Console.WriteLine("  {0,-10} converts to {1}", tmp, day.ToString())
                Else
                    Console.WriteLine("  {0,-10} is not an underlying value of Weekdays", tmp)
                End If
            Else
                Console.WriteLine("  {0,-10} is not a member of Weekdays", tmp)
            End If
        Next dayString
        Console.WriteLine()

        Console.WriteLine("(enum with FlagsAttribute)")
        For val As Integer = 0 To 16
            Console.WriteLine("{0,3} - {1:G}", val, CType(val, EnumWithFlags))
        Next
        Console.WriteLine()
    End Sub

    Sub Main()
        ' This application combines some examples from these C# samples:
        ' * SampleConsole
        ' * StringsDemo
        Console.WriteLine("Caution! Visual Basic is rather disgusting...avoid!")
        Console.WriteLine()
        Dim msgWelcome As String = String.Format("### VB.NET console application, targetting {0} ###", GetFrameworkDisplayName())
        Console.WriteLine(msgWelcome)
        Console.WriteLine()

        ' Display some system information
        DisplaySystemInformation()

        ' Show some information about this assembly
        DisplayAssemblyInfo()

        ' Enumerations
        DisplayEnumInfo()
    End Sub

End Module