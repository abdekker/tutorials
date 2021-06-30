Option Strict Off
#Region "Note on Option Strict"
' "Option Strict On" is recommended, giving some of these benefits:
' * Catches implicit narrowing conversions
'       e.g. converting Int32 to Int16
' * Prevents late-binding (assigning an object to a variable declared to be of type Object)
'       Late-binding can be useful, such as using COM objects where the object type will only be determined at
'       run-time. In this case, set Project > Properties > Compile > Option strict = "On" and "Late binding; call
'       could fail at run time" = "Warning"
' * Catches implicit typing that results in an Object type
'       e.g "Dim myNum = 5" when "Dim myNum as Integer = 5" was intended
' * Results in faster, more optimised code

' In general, have "Option Strict On" enabled at file or project level. But it is disabled in this project in order
' to demonstrate explicit casting and other techniques
#End Region

Imports System.IO
Imports System.Reflection
Imports System.Runtime.Versioning
Imports System.Runtime.InteropServices
Imports System.Linq

Imports Microsoft.VisualBasic   ' For legacy string and other VB6 methods

Imports systemHelperLibrary
Imports typesLib = systemHelperLibrary.TypesLibrary

' Following import demonstrates a kind of "typedef" in Visual Basic => "S" becomes an alias for "System.String"
Imports S = System.String

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

    #Region "Structures"
    Structure Person
        Dim name As String
        Dim age As Integer

        Public Sub New(ByVal theirName As String,
                       ByVal theirAge As Integer)
            name = theirName
            age = theirAge
        End Sub
    End Structure
    #End Region

    #Region "Property accessors"
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
    #End Region

    #Region "Helper methods"
    Private Function GetFrameworkDisplayName() As String
        Dim attribs() As Object = Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(TargetFrameworkAttribute), False)
        Dim targetFramework As TargetFrameworkAttribute = attribs.SingleOrDefault() ' "SingleOrDefault" requires System.Linq
        GetFrameworkDisplayName = targetFramework.FrameworkDisplayName
    End Function

    Function IsValid(ByVal checkValue As Integer, ByRef grandTotal As Integer) As Boolean
        ' Testing short-circuit behaviour in logical operators (e.g. And, AndAlso)
        If (checkValue > 15) Then
            ' The warning is not displayed if the call to IsValid() is part of a short-circuited expression
            MsgBox(checkValue.ToString() + " is not a valid value.")
            Return False
        Else
            ' grandTotal is not updated if the call to IsValid() is part of a short-circuited expression
            grandTotal += checkValue
            Return True
        End If
    End Function
    #End Region

    #Region "Main test methods"
    Private Sub DisplaySystemInformation()
        ' See C#\StringsDemo\DisplaySystemInformation
        Console.WriteLine("### System Information ###")
        Console.WriteLine("  OS Description           {0} ({1})", RuntimeInformation.OSDescription, OSType)
        Console.WriteLine("  OS Architecture          {0}", RuntimeInformation.OSArchitecture.ToString())
        Console.WriteLine("  Process Architecture     {0}", RuntimeInformation.ProcessArchitecture.ToString())
        Console.WriteLine("  Framework Description    {0}", RuntimeInformation.FrameworkDescription)
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub DisplayAssemblyInfo()
        ' See C#\StringsDemo\DisplayAssemblyInfo
        Console.WriteLine("### Assembly Information ###")
        Console.WriteLine(String.Format("  Assembly full path: {0}", Assembly.GetEntryAssembly().Location))
        Console.WriteLine(String.Format("  Assembly directory: {0}", GetAssemblyDirectory))
        #if DEBUG then
            Console.WriteLine("  Assembly built in DEBUG mode")
        #else
            Console.WriteLine("  Assembly built in RELEASE mode")
        #end if

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Loops()
        ' Demonstrating the main control structures used in VB for loops
        Console.WriteLine("### Loops in Visual Basic ###")
        Console.WriteLine("(NB! 'loop' is a reserved word in VB!")

        Const txtFile As String = "sampleReading.txt"
        If (File.Exists(txtFile)) Then
            Console.WriteLine()
            Console.WriteLine(" (reading text file: {0})", txtFile)
            Dim sr As StreamReader = File.OpenText(txtFile)

            While sr.Peek() >= 0
                Console.WriteLine(sr.ReadLine())
            End While

            sr.Close()
            Console.WriteLine()
        End If

        Console.WriteLine(" (While...End While)")
        Console.WriteLine("   (exit with 'Exit While', like C++ 'break')")
        Console.WriteLine("   (continue to next iteration with 'Continue While', like C++ 'continue')")
        Dim index As Integer = 10
        While index <= 50
            Console.Write(index.ToString & " ")
            index += 10
        End While
        Console.WriteLine()
        Console.WriteLine()

        Console.WriteLine(" (Do...Loop Until)")
        Console.WriteLine("   (exit with 'Exit Do', like C++ 'break')")
        Console.WriteLine("   (continue to next iteration with 'Continue Do', like C++ 'continue')")
        index = 10
        Do
            Console.Write(index.ToString & " ")
            index += 10
        Loop Until index > 50
        Console.WriteLine()
        Console.WriteLine()

        Console.WriteLine(" (Do While...Loop)")
        index = 10
        Do While (index <= 50)
            Console.Write(index.ToString & " ")
            index += 10
        Loop
        Console.WriteLine()
        Console.WriteLine()

        ' For reading the text file, convert the While...End While loop above to:
        '   Do While sr.Peek() >= 0
        '       Console.WriteLine(sr.ReadLine())
        '   Loop

        Console.WriteLine(" (For...Next, also demonstrating 'Step' and nesting)")
        For outer As Integer = 3 To 1 Step -1
            Console.Write(outer.ToString & ": ")
            For inner As Integer = 10 To 50 Step 10
                Console.Write(inner.ToString & " ")
            Next inner
            Console.WriteLine()
        Next outer
        Console.WriteLine()

        Console.WriteLine(" (For Each...Next, also demonstrating exit/continue)")
        Dim numbersList As List(Of Integer) = New List(Of Integer)
        ' Or: Dim numbers() As Integer = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12}
        For index = 1 To 12
            numbersList.Add(index)
        Next ' Or: "Next index"

        For Each index In numbersList ' Or: For Each number As Integer In numbersList
            ' Skip numbers 5 through 8
            If (index >= 5) And (index) <= 8 Then
                Continue For
            End If

            ' Display the number
            Console.Write(index.ToString() & " ")

            ' If number is 10, exit the loop
            If (index >= 10) Then
                Exit For
            End If
        Next
        Console.WriteLine()

        Console.WriteLine()
        Console.WriteLine(" (Using LBound and UBound of an array)")
        Console.WriteLine(" (Note: LBound = 0 and UBound = -1 for an empty array)")
        Dim numbersArray() As Integer = {-123, 0, 7, 13, 2147483647}
        For index = LBound(numbersArray) To UBound(numbersArray)
            Console.WriteLine("    " & index.ToString() & ": " & numbersArray(index))
        Next

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Strings_Basic()
        ' Some details on strings and string formatting in Visual Basic
        Console.WriteLine("### Strings - Basic ###")

        Console.WriteLine("(basic formatting)")
        Dim name As String = "Fred"
        Dim height As Single = 1.936
        Dim age As Integer = 29
        Console.WriteLine("  a) {0} is {1:0.00}m tall and {2} years old [implicit formatting]", name, height, age)
        Console.WriteLine(String.Format("  b) {0} is {1:0.00}m tall and {2} years old [explicit formatting]", name, height, age))
        Console.WriteLine($"  c) {name} is {height:0.00}m tall and {age} years old [string interpolation]")
        Console.WriteLine()

        Console.WriteLine("(string arrays)")
        Dim array1() As String = { "hello", "11", "everyone", "!" }
        Console.WriteLine("  " + String.Join(" ", array1) + "   [Dim array() As String = { ... }]")

        Dim array2() As String = New String() { "hello", "22", "everyone", "?" }
        Console.WriteLine("  " + String.Join(" ", array2) + "   [Dim array() As String = New String { ... }]")

        Dim s1 As String = "hello,33,everyone,#"
        Dim array3() As String = s1.Split(",")
        Console.WriteLine("  " + String.Join(" ", array3) + "   [using String.Split(char)]")
        Console.WriteLine()

        Console.WriteLine("(alias for System.String in VB, similar to ""typedef"")")
        Dim s2 As S = "Today I'm an alias"
        Console.WriteLine("  {0}    [using Imports S = System.String alias]", s2)
        Console.WriteLine()

        Dim randomLength As Byte = 40
        Console.WriteLine("(randomised strings, length {0})", randomLength)
        Console.WriteLine("  {0,-20}{1}", "Letters only", StringLibrary.GetRandomString(randomLength, true))
        Console.WriteLine("  {0,-20}{1}", "Any printable", StringLibrary.GetRandomString(randomLength, false))
        Console.WriteLine("  {0,-20}{1}", "Remove illegal", StringLibrary.GetRandomString(randomLength, false, true))

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Strings_Basic_Search()
        ' String searching
        Console.WriteLine("### Strings - Search ###")

        Console.WriteLine("(searching strings)")
        Dim msg As String = "According to Joe, this is a beautiful day. Joe rides a bicycle!"
        Dim joeUpper As String = "Joe"
        Dim joeLower As String = "joe"
        Dim jedi As String = "Jedi"
        Console.WriteLine("  (string = '{0}')", msg)
        Console.WriteLine()

        Console.WriteLine("  (InStr & InStrRev & Len - legacy and should be avoided, though InStr is faster than IndexOf for strings)")
        Console.WriteLine("    Instr ({0}):             {1,3}", joeUpper, InStr(msg, joeUpper))
        Console.WriteLine("    Instr (Binary) ({0}):    {1,3}", joeLower, InStr(msg, joeLower))
        Console.WriteLine("    Instr (Text)  ({0}):     {1,3}", joeLower, InStr(msg, joeLower, CompareMethod.Text))
        Console.WriteLine("    Instr ({0}):            {1,3}", jedi, InStr(msg, jedi))
        Console.WriteLine()
        Console.WriteLine("    InstrRev ({0}):          {1,3}", joeUpper, InStrRev(msg, joeUpper))
        Console.WriteLine("    InStrRev (Binary) ({0}): {1,3}", joeLower, InStrRev(msg, joeLower))
        Console.WriteLine("    InStrRev (Text)  ({0}):  {1,3}", joeLower, InStrRev(msg, joeLower, -1, CompareMethod.Text))
        Console.WriteLine("    InStrRev ({0}):         {1,3}", jedi, InStrRev(msg, jedi))
        Console.WriteLine()
        Console.WriteLine("    Len ({0}):              {1,3}", joeUpper, Len(joeUpper))
        Console.WriteLine("    Len (msg):               {0,3}", Len(msg))
        Console.WriteLine()

        Console.WriteLine("  (IndexOf & LastIndexOf & Length - modern .NET equivalents)")
        Console.WriteLine("    IndexOf ({0}):               {1,3}", joeUpper, msg.IndexOf(joeUpper))
        Console.WriteLine("    IndexOf (case) ({0}):        {1,3}", joeLower, msg.IndexOf(joeLower))
        Console.WriteLine("    IndexOf (no case) ({0}):     {1,3}", joeLower, msg.IndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase))
        Console.WriteLine("    IndexOf ({0}):               {1,3}", jedi, msg.IndexOf(jedi))
        Console.WriteLine()
        Console.WriteLine("    LastIndexOf ({0}):           {1,3}", joeUpper, msg.LastIndexOf(joeUpper))
        Console.WriteLine("    LastIndexOf (case) ({0}):    {1,3}", joeLower, msg.LastIndexOf(joeLower))
        Console.WriteLine("    LastIndexOf (no case) ({0}): {1,3}", joeLower, msg.LastIndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase))
        Console.WriteLine("    LastIndexOf ({0}):          {1,3}", jedi, msg.LastIndexOf(jedi))
        Console.WriteLine()
        Console.WriteLine("    Length ({0}):               {1,3}", joeUpper, joeUpper.Length)
        Console.WriteLine("    Length (msg):                {0,3}", msg.Length)

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Strings_Performance_InStr()
        ' Performance tests between legacy (Microsoft.VisualBasic) and modern .NET string equivalents
        ' NB! The modern equivalents are not always faster!

        ' General conclusion:
        ' * Use the legacy "InStr" and "InstrRev" for string searching, particularly case-sensitive (the default)
        ' * Case-insensitive searching are approximately equivalent
        ' * Use the modern .NET equivalent "IndexOf" and "LastIndexOf" for character searches
        ' * Use the modern .NET equivalent "Length" for the length of a string
        Const ms_to_ns As Single = 1000000.0f
        
        Console.WriteLine("### Strings - Performance tests for legacy method (InStr) ###")
        Dim empty As String = String.Empty
        Dim blank As String = ""
        Dim msg As String = "According to Joe, this is a beautiful day. Joe rides a bicycle!"

        Dim joeUpper As String = "Joe"
        Dim joeLower As String = "joe"
        Dim jedi As String = "Super-Jedi-Yoda the Magnificent!"
        Dim myChar As Char = "u"
        Console.WriteLine("  (Strings to search : empty = '{0}', blank = '{1}', msg = '{2}')", empty, blank, msg)
        Console.WriteLine("  (Search terms      : 1 = '{0}', 2 = '{1}', 3 = '{2}', 4 = '{3}')", joeUpper, joeLower, jedi, myChar)
        Console.WriteLine()

        Dim index, loops As Integer
        Dim startTicks, elapsedTicks As Integer
        Dim avg As Double
        Dim count As Int64

        Console.WriteLine("###############################################################################")
        loops = 1000000000
        Console.WriteLine("  (Microsoft.VisualBasic.InStr v System.String.IndexOf [strings, case-sensitive])")
        Console.WriteLine("    (InStr, empty, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(empty, joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 200000000
        Console.WriteLine("    (IndexOf, empty, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.IndexOf(joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 1000000000
        Console.WriteLine("    (InStr, blank, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(blank, joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 200000000
        Console.WriteLine("    (IndexOf, blank, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += blank.IndexOf(joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 100000000
        Console.WriteLine("    (InStr, msg, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(msg, joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 20000000
        Console.WriteLine("    (IndexOf, msg, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.IndexOf(joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("  (Microsoft.VisualBasic.InStr v System.String.IndexOf [strings, case-sensitive])")
        loops = 1000000000
        Console.WriteLine("    (InStr, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(empty, joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 200000000
        Console.WriteLine("    (IndexOf, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.IndexOf(joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 50000000
        Console.WriteLine("    (InStr, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(msg, joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 10000000
        Console.WriteLine("    (IndexOf, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.IndexOf(joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("  (Microsoft.VisualBasic.InStr v System.String.IndexOf [strings, case-insensitive])")
        loops = 1000000000
        Console.WriteLine("    (InStr, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(empty, joeLower, CompareMethod.Text)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 500000000
        Console.WriteLine("    (IndexOf, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.IndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 20000000
        Console.WriteLine("    (InStr, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(msg, joeLower, CompareMethod.Text)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 20000000
        Console.WriteLine("    (IndexOf, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.IndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        loops = 500000000
        Console.WriteLine("  (Microsoft.VisualBasic.InStr v System.String.IndexOf [character])")
        Console.WriteLine("    (InStr, empty, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(empty, myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 1000000000
        Console.WriteLine("    (IndexOf, empty, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.IndexOf(myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 50000000
        Console.WriteLine("    (InStr, msg, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(msg, myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 100000000
        Console.WriteLine("    (IndexOf, msg, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.IndexOf(myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Strings_Performance_InStrRev()
        ' See "VB_Strings_Performance_InStr"
        Const ms_to_ns As Single = 1000000.0f
        
        Console.WriteLine("### Strings - Performance tests for legacy method (Instr) ###")
        Dim empty As String = String.Empty
        Dim msg As String = "According to Joe, this is a beautiful day. Joe rides a bicycle!"

        Dim joeUpper As String = "Joe"
        Dim joeLower As String = "joe"
        Dim jedi As String = "Super-Jedi-Yoda the Magnificent!"
        Dim myChar As Char = "u"
        Console.WriteLine("  (Strings to search : empty = '{0}', msg = '{1}')", empty, msg)
        Console.WriteLine("  (Search terms      : 1 = '{0}', 2 = '{1}', 3 = '{2}', 4 = '{3}')", joeUpper, joeLower, jedi, myChar)
        Console.WriteLine()

        Dim index, loops As Integer
        Dim startTicks, elapsedTicks As Integer
        Dim avg As Double
        Dim count As Int64

        Console.WriteLine("###############################################################################")
        loops = 1000000000
        Console.WriteLine("  (Microsoft.VisualBasic.InStrRev v System.String.LastIndexOf [strings, case-sensitive])")
        Console.WriteLine("    (InStrRev, empty, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(empty, joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 1000000000
        Console.WriteLine("    (LastIndexOf, empty, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.LastIndexOf(joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 100000000
        Console.WriteLine("    (InStrRev, msg, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(msg, joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 20000000
        Console.WriteLine("    (LastIndexOf, msg, '{0}')", joeUpper)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.LastIndexOf(joeUpper)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("  (Microsoft.VisualBasic.InStrRev v System.String.IndexOf [strings, case-sensitive])")
        loops = 1000000000
        Console.WriteLine("    (InStrRev, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(empty, joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 1000000000
        Console.WriteLine("    (LastIndexOf, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.LastIndexOf(joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 50000000
        Console.WriteLine("    (InStrRev, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(msg, joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 10000000
        Console.WriteLine("    (LastIndexOf, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.LastIndexOf(joeLower)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("  (Microsoft.VisualBasic.InStrRev v System.String.IndexOf [strings, case-insensitive])")
        loops = 1000000000
        Console.WriteLine("    (InStrRev, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStr(empty, joeLower, CompareMethod.Text)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 1000000000
        Console.WriteLine("    (LastIndexOf, empty, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.LastIndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 100000000
        Console.WriteLine("    (InStrRev, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(msg, joeLower, CompareMethod.Text)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 20000000
        Console.WriteLine("    (LastIndexOf, msg, '{0}')", joeLower)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.LastIndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        loops = 500000000
        Console.WriteLine("  (Microsoft.VisualBasic.InStrRev v System.String.IndexOf [character])")
        Console.WriteLine("    (InStrRev, empty, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(empty, myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 2000000000
        Console.WriteLine("    (LastIndexOf, empty, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.LastIndexOf(myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 50000000
        Console.WriteLine("    (InStrRev, msg, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += InStrRev(msg, myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 100000000
        Console.WriteLine("    (LastIndexOf, msg, '{0}')", myChar)
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.LastIndexOf(myChar)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Strings_Performance_Len()
        ' See "VB_Strings_Performance_InStr"
        Const ms_to_ns As Single = 1000000.0f
        
        Console.WriteLine("### Strings - Performance tests for legacy method (Len) ###")
        Dim empty As String = String.Empty
        Dim msg As String = "According to Joe, this is a beautiful day. Joe rides a bicycle!"

        Console.WriteLine("  (Strings to measure : empty = '{0}', msg = '{1}')", empty, msg)
        Console.WriteLine()

        Dim index, loops As Int64
        Dim startTicks, elapsedTicks As Integer
        Dim avg As Double
        Dim count As Int64

        Console.WriteLine("###############################################################################")
        loops = 5000000000
        Console.WriteLine("  (Microsoft.VisualBasic.Len v System.String.Length)")
        Console.WriteLine("    (Len, empty)")
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += Len(empty)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 5000000000
        Console.WriteLine("    (Length, empty)")
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += empty.Length
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine()

        loops = 5000000000
        Console.WriteLine("    (Len, msg)")
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += Len(msg)
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)

        loops = 5000000000
        Console.WriteLine("    (Length, msg)")
        startTicks = Environment.TickCount
        count = 0
        For index = 1 To loops
            count += msg.Length
        Next
        elapsedTicks = (Environment.TickCount - startTicks)
        avg = ((elapsedTicks / loops) * ms_to_ns)
        Console.WriteLine("       {0}ms for {1:N0} loops, average {2:0.0000}ns", elapsedTicks, loops, avg)
        Console.WriteLine("###############################################################################")

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Integers()
        Console.WriteLine("### Integers ###")

        Const Formatting As String = "  {0,-10}{1,-10}{2,-18}{3,-14}{4,-22}{5}"
        Console.WriteLine(Formatting, "Type", "Name", "Full Name", "Size (bytes)", "Min", "Max")

        Console.WriteLine()
        Console.WriteLine("(signed)")
        ' The "If True Then...End If" constructs used below artificially create local scope.
        ' Alternatives include:
        '       With True...End With
        '       With Nothing...End With
        '       With 0...End With
        '       Do...Loop Until True
        If True Then
            ' Boolean (System.Boolean)
            Dim min As Boolean = False
            Dim max As Boolean = True
            Console.WriteLine(Formatting,
                "Boolean", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' SByte (System.SByte)
            Dim min As SByte = SByte.MinValue
            Dim max As SByte = SByte.MaxValue
            Console.WriteLine(Formatting,
                "SByte", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' Short (System.Int16)
            Dim min As Short = Short.MinValue
            Dim max As Short = Short.MaxValue
            Console.WriteLine(Formatting,
                "Short", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' Integer (System.Int32)
            Dim min As Integer = Integer.MinValue
            Dim max As Integer = Integer.MaxValue
            Console.WriteLine(Formatting,
                "Integer", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' Long (System.Int64)
            Dim min As Long = Long.MinValue
            Dim max As Long = Long.MaxValue
            Console.WriteLine(Formatting,
                "Long", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        Console.WriteLine()
        Console.WriteLine("(unsigned)")
        If True Then
            ' Byte (System.Byte)
            Dim min As Byte = Byte.MinValue
            Dim max As Byte = Byte.MaxValue
            Console.WriteLine(Formatting,
                "Byte", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' UShort (System.UInt16)
            Dim min As UShort = UShort.MinValue
            Dim max As UShort = UShort.MaxValue
            Console.WriteLine(Formatting,
                "UShort", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' UInteger (System.UInt32)
            Dim min As UInteger = UInteger.MinValue
            Dim max As UInteger = UInteger.MaxValue
            Console.WriteLine(Formatting,
                "UInteger", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If

        If True Then
            ' ULong (System.UInt64)
            Dim min As ULong = ULong.MinValue
            Dim max As ULong = ULong.MaxValue
            Console.WriteLine(Formatting,
                "ULong", typesLib.GetObjectName(min), typesLib.GetObjectFullName(min), typesLib.GetObjectSize(min), min, max)
        End If
        Console.WriteLine()

        Console.WriteLine("(arrays)")
        Dim intData() As Integer = {-123, 0, 7, 13, 2147483647}
        Console.Write("  ")
        For Each num In intData
            Console.Write(num.ToString() + " ")
        Next
        Console.WriteLine()

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Integers_Casting_From_Object()
        Console.WriteLine("### Integer casting - from object ###")
        Console.WriteLine("(Note: Each casting technique has different performance implications)")
        Console.WriteLine()

        Const Formatting As String = "    Object {0,-5}{1,10} {2,-5}{3}"
        Dim objInt As Object
        Dim myInt As Integer
        Dim myShort As Short

        Console.WriteLine("  (implicit casting by direct assignment)")
        objInt = 3
        myInt = objInt
        Console.WriteLine(Formatting, objInt, "Integer", myInt, "(direct assignment)")
        myShort = objInt
        Console.WriteLine(Formatting, objInt, "Short", myShort, "(direct assignment)")
        Console.WriteLine()

        Console.WriteLine("  (DirectCast - requires one type to inherit from or implement the other type)")
        objInt = 5
        myInt = DirectCast(objInt, Integer)
        Console.WriteLine(Formatting, objInt, "Integer", myInt, "(DirectCast)")

        Try
            myShort = DirectCast(objInt, Short)
            Console.WriteLine(Formatting, objInt, "Short", myShort, "(DirectCast)")
        Catch ex As Exception
            Console.WriteLine("  DirectCast fails for 'Short': " + ex.Message + " (expected)")
        End Try
        Console.WriteLine()

        Console.WriteLine("  (TryCast - works on reference types only (not value types), so does not apply to integers)")
        Console.WriteLine()

        Console.WriteLine("  (CType)")
        objInt = 7
        myInt = CType(objInt, Integer)
        Console.WriteLine(Formatting, objInt, "Integer", myInt, "(CType)")
        myShort = CType(objInt, Short)
        Console.WriteLine(Formatting, objInt, "Short", myShort, "(CType)")
        Console.WriteLine()

        Console.WriteLine("  (CInt)")
        objInt = 11
        myInt = CInt(objInt)
        Console.WriteLine(Formatting, objInt, "Integer", myInt, "(CInt)")
        myShort = CInt(objInt)
        Console.WriteLine(Formatting, objInt, "Short", myShort, "(CInt)")
        Console.WriteLine()

        Console.WriteLine("  (CShort)")
        objInt = 13
        myInt = CShort(objInt)
        Console.WriteLine(Formatting, objInt, "Integer", myInt, "(CShort)")
        myShort = CShort(objInt)
        Console.WriteLine(Formatting, objInt, "Short", myShort, "(CShort)")
        Console.WriteLine()

        Console.WriteLine("  (Convert)")
        objInt = 17
        myInt = Convert.ToInt32(objInt)
        Console.WriteLine(Formatting, objInt, "Integer", myInt, "(Convert)")
        myShort = Convert.ToInt16(objInt)
        Console.WriteLine(Formatting, objInt, "Short", myShort, "(Convert)")

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Integers_Casting_From_String()
        Console.WriteLine("### Integer casting - from String ###")
        Console.WriteLine("(Note: Each casting technique has different performance implications)")
        Console.WriteLine()

        Const Formatting As String = "    String {0,-5}{1,10} {2,-5}{3}"
        Dim myString As String
        Dim myInt As Integer
        Dim myShort As Short

        Console.WriteLine("  (implicit casting by direct assignment)")
        myString = "3"
        myInt = myString
        Console.WriteLine(Formatting, myString, "Integer", myInt, "(direct assignment)")
        myShort = myString
        Console.WriteLine(Formatting, myString, "Short", myShort, "(direct assignment)")
        Console.WriteLine()

        Console.WriteLine("  (DirectCast - requires one type to inherit from or implement the other type, so does not apply to integers)")
        Console.WriteLine()

        Console.WriteLine("  (TryCast - works on reference types only (not value types), so does not apply to integers)")
        Console.WriteLine()

        Console.WriteLine("  (CType)")
        myString = "5"
        myInt = CType(myString, Integer)
        Console.WriteLine(Formatting, myString, "Integer", myInt, "(CType)")
        myShort = CType(myString, Short)
        Console.WriteLine(Formatting, myString, "Short", myShort, "(CType)")
        Console.WriteLine()

        Console.WriteLine("  (CInt)")
        myString = "7"
        myInt = CInt(myString)
        Console.WriteLine(Formatting, myString, "Integer", myInt, "(CInt)")
        myShort = CInt(myString)
        Console.WriteLine(Formatting, myString, "Short", myShort, "(CInt)")
        Console.WriteLine()

        Console.WriteLine("  (CShort)")
        myString = "11"
        myInt = CShort(myString)
        Console.WriteLine(Formatting, myString, "Integer", myInt, "(CShort)")
        myShort = CShort(myString)
        Console.WriteLine(Formatting, myString, "Short", myShort, "(CShort)")
        Console.WriteLine()

        Console.WriteLine("  (Convert)")
        myString = "13"
        myInt = Convert.ToInt32(myString)
        Console.WriteLine(Formatting, myString, "Integer", myInt, "(Convert)")
        myShort = Convert.ToInt16(myString)
        Console.WriteLine(Formatting, myString, "Short", myShort, "(Convert)")

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Integers_Casting_Between_Integers()
        Console.WriteLine("### Integer casting - between integer types ###")
        Console.WriteLine()

        Const Formatting As String = "    Integer {0,-5} Short {1,-5}{2}"
        Dim myInt As Integer
        Dim myShort As Short

        Console.WriteLine("  (implicit casting by direct assignment)")
        myInt = 3
        myShort = myInt
        Console.WriteLine(Formatting, myInt, myShort, "(direct assignment, short = int)")
        myShort = 5
        myInt = myShort
        Console.WriteLine(Formatting, myInt, myShort, "(direct assignment, int = short)")
        Console.WriteLine()

        Console.WriteLine("  (DirectCast - not applicable because Integer and Short are different types)")
        Console.WriteLine()
        
        Console.WriteLine("  (TryCast - not applicable because Integer and Short are different types)")
        Console.WriteLine()

        Console.WriteLine("  (CType)")
        myInt = 7
        myShort = CType(myInt, Integer)
        Console.WriteLine(Formatting, myInt, myShort, "(CType, short = int)")
        myShort = 11
        myInt = CType(myShort, Short)
        Console.WriteLine(Formatting, myInt, myShort, "(CType, int = short)")
        Console.WriteLine()

        Console.WriteLine("  (CInt)")
        myInt = 13
        myShort = CInt(myInt)
        Console.WriteLine(Formatting, myInt, myShort, "(CInt, short = int)")
        myShort = 17
        myInt = CInt(myShort)
        Console.WriteLine(Formatting, myInt, myShort, "(CInt, int = short)")
        Console.WriteLine()

        Console.WriteLine("  (CShort)")
        myInt = 19
        myShort = CShort(myInt)
        Console.WriteLine(Formatting, myInt, myShort, "(CShort, short = int)")
        myShort = 21
        myInt = CShort(myShort)
        Console.WriteLine(Formatting, myInt, myShort, "(CShort, int = short)")
        Console.WriteLine()

        Console.WriteLine("  (Convert)")
        myInt = 23
        myShort = Convert.ToInt16(myInt)
        Console.WriteLine(Formatting, myInt, myShort, "(Convert, short = int)")
        myShort = 29
        myInt = Convert.ToInt32(myShort)
        Console.WriteLine(Formatting, myInt, myShort, "(Convert, int = short)")

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_LogicalOperators()
        ' Logical operators in Visual Basic

        ' Short-circuit operators should generally be preferred to non short-circuit operators because code is more
        ' efficient (as potentially expensive methods do not need to be called).

        ' One exception is when all checks must be done, such as method calls that perform important system checks or
        ' change variables. However, better code design is to remove these "hidden" checks => if the check is
        ' genuinely important, explicitly call it.

        ' Where possible use the short-circuit operators in new code, and convert older VB6 code to use the newer
        ' operators where feasible.
        Console.WriteLine("### Logical operators in Visual Basic ###")
        Console.WriteLine("  VB uses these operators: And, Or, Not, AndAlso, OrElse, IsNot")
        Console.WriteLine("  ""And"" and ""Or"" are non short-circuit operators like ""&"" and ""|"" in C#")
        Console.WriteLine("  ""AndAlso"" and ""OrElse"" are short-circuit operators like ""&&"" and ""||"" in C#")
        Console.WriteLine()

        Const highestAllowed As Integer = 45
        Dim amount As Integer = 12
        Dim total As Integer = 0
        Dim totalLast As Integer = total
        Console.WriteLine("  Amount={0}, Total={1}", amount, total)
        Console.WriteLine()

        Console.WriteLine("(""And"" is a non short-circuit operator)")
        If (amount > highestAllowed) And (IsValid(amount, total)) Then
            ' The preceding statement calls IsValid()
        End If
        Console.WriteLine("  Amount={0}, Total={1}, {2}", amount, total, If(total = totalLast, "SAME", "CHANGED"))
        Console.WriteLine()

        Console.WriteLine("(""AndAlso"" is a short-circuit operator)")
        totalLast = total
        If (amount > highestAllowed) AndAlso (IsValid(amount, total)) Then
            ' The preceding statement does NOT call IsValid()
        End If
        Console.WriteLine("  Amount={0}, Total={1}, {2}", amount, total, If(total = totalLast, "SAME", "CHANGED"))
        Console.WriteLine()

        Console.WriteLine("(""Or"" is a non short-circuit operator)")
        totalLast = total
        If (amount < highestAllowed) Or (IsValid(amount, total)) Then
            ' The preceding statement calls IsValid()
        End If
        Console.WriteLine("  Amount={0}, Total={1}, {2}", amount, total, If(total = totalLast, "SAME", "CHANGED"))
        Console.WriteLine()

        Console.WriteLine("(""OrElse"" is a short-circuit operator)")
        totalLast = total
        If (amount < highestAllowed) OrElse (IsValid(amount, total)) Then
             ' The preceding statement does NOT call IsValid()
        End If
        Console.WriteLine("  Amount={0}, Total={1}, {2}", amount, total, If(total = totalLast, "SAME", "CHANGED"))
        Console.WriteLine()

        Console.WriteLine("Since 2008, VB introduces a ternary operator ""If"" in three and two-argument varieties")
        Console.WriteLine("  Similar to the C++ ternary operator: ""int value = (condition) ? -5 : 27""")
        Console.WriteLine("  Somewhat more complicated than the C++ version since the return is of type Object")
        Console.WriteLine()
        Console.WriteLine("(three-argument ternary operator using short-circuit evaluation)")
        Dim numerator As Integer = 12
        Dim denominator As Integer = 3
        Console.WriteLine("  A={0}, B={1}, A/B={2}", numerator, denominator, If(denominator = 0, "NOT EVALUATED", numerator/denominator))
        denominator = 0
        Console.WriteLine("  A={0}, B={1}, A/B={2}", numerator, denominator, If(denominator = 0, "NOT EVALUATED", numerator/denominator))

        Console.WriteLine("(two-argument ternary operator using nullable 1st argument)")
        Dim first? As Integer = 3
        Dim second As Integer = 6
        Console.WriteLine("  1st={0}, 2nd={1},          [1st if not Nothing, else 2nd]={2}",
            If(first IsNot Nothing, first, "(Nothing)"), second, If(first, second))
        second = Nothing
        Console.WriteLine("  1st={0}, 2nd={1},          [1st if not Nothing, else 2nd]={2}",
            If(first IsNot Nothing, first, "(Nothing)"), second, If(first, second))
        first = Nothing
        second = 6
        Console.WriteLine("  1st={0}, 2nd={1},  [1st if not Nothing, else 2nd]={2}",
            If(first IsNot Nothing, first, "(Nothing)"), second, If(first, second))

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_NullableTypes()
        ' Nullable types were introduced into .NET 4.6 (2015)
        Console.WriteLine("### Nullable types ###")

        ' Create a list of people
        Dim people As List(Of Person) = New List(Of Person) From
        {
            New Person("Sally", 37),
            New Person("Andrew", 71),
            Nothing,
            New Person("Fred", 16)
        }

        ' Alternatively, the list could be created like this:
        'Dim people As List(Of Person) = New List(Of Person)
        'Dim p As Person
        'p.name = "Sally"
        'p.age = 37
        'people.Add(p)

        'p.name = "Andrew"
        'p.age = 71
        'people.Add(p)

        'people.Add(Nothing)

        'p.name = "Fred"
        'p.age = 16
        'people.Add(p)

        Console.WriteLine("(raw list)")
        Dim index As Integer = 1
        For Each item As Person In people
            Console.WriteLine("  Person {0}: Name: {1,-10} Age: {2}", index, item.name, item.age)
            index += 1
        Next
        Console.WriteLine()

        Console.WriteLine("(using nullable type syntax with '?.')")
        index = 1
        For Each item? As Person In people  ' Note the "item?" nullable type
            Console.WriteLine("  Person {0}: Name: {1,-10} Age: {2}", index, item?.name, item?.age)
            index += 1
        Next
        Console.WriteLine()

        Console.WriteLine("(using nullable type with ternary operator)")
        index = 1
        For Each item? As Person In people  ' Note the "item?" nullable type
            Console.WriteLine("  Person {0}: Name: {1,-10} Age: {2}",
                index,
                If (item?.name Is Nothing, "(blank)", item?.name),
                If (item?.age Is Nothing, -1, item?.age))
            index += 1
        Next
        Console.WriteLine()

        Console.WriteLine("(nullable value types - using Integer as example)")
        ' Can be declared in three ways:
        ' * Dim first? As Integer
        ' * Dim first As Integer?
        ' * Dim first As Nullable(Of Integer)
        Dim first? As Integer = 3
        Dim second As Integer = 7
        Console.WriteLine("  1st [nullable]={0}, 2nd [std value type]={1}   [both set to valid values]", first, second)
        first = Nothing
        second = Nothing
        Console.WriteLine("  1st [nullable]={0}, 2nd [std value type]={1}    [both set to Nothing]", first, second)
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Enumerations()
        ' Some details on enumerations in Visual Basic (see C#\StringsDemo\Info_Enum)
        Console.WriteLine("### Enumerations ###")
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
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub VB_Like()
        ' Some details on using the "Like" keyword in Visual Basic
        Console.WriteLine("### Like (keyword) ###")
        Console.WriteLine("  ""Like"" compares a string against a pattern (similar to regular expressions)")
        Console.WriteLine("  Not currently supported in .NET Core and .NET Standard projects")
        Console.WriteLine()

        Dim testCheck As Boolean

        ' Returns True (does "F" satisfy "F"?)
        Console.WriteLine("  {0,-30}{1}", """F"" Like ""F""", ("F" Like "F"))

        ' Returns False for Option Compare Binary, and True for Option Compare Text (does "F" satisfy "f"?)
        Console.WriteLine("  {0,-30}{1}", """F"" Like ""f""", ("F" Like "f"))

        ' Returns False (does "F" satisfy "FFF"?)
        Console.WriteLine("  {0,-30}{1}", """F"" Like ""FFF""", ("F" Like "FFF"))

        ' Returns True (does "aBBBa" have an "a" at the beginning, an "a" at the end, and any number of characters
        ' in between?)
        Console.WriteLine("  {0,-30}{1}", """aBBBa"" Like ""a*a""", ("aBBBa" Like "a*a"))

        ' Returns True (does "F" occur in the set of characters from "A" through "Z"?)
        Console.WriteLine("  {0,-30}{1}", """F"" Like ""[A-Z]""", ("F" Like "[A-Z]"))

        ' Returns False (does "F" NOT occur in the set of characters from "A" through "Z"?)
        Console.WriteLine("  {0,-30}{1}", """F"" Like ""[!A-Z]""", ("F" Like "[!A-Z]"))

        ' Returns True (does "a2a" begin and end with an "a" and have any single-digit number in between?)
        Console.WriteLine("  {0,-30}{1}", """a2a"" Like ""a#a""", ("a2a" Like "a#a"))

        ' Returns True (does "aM5b" begin with an "a", followed by any character from the set "L" through "P",
        ' followed by any single-digit number, and end with any character NOT in the character set "c" through "e"?)
        Console.WriteLine("  {0,-30}{1}", """aM5b"" Like ""a[L-P]#[!c-e]""", ("aM5b" Like "a[L-P]#[!c-e]"))

        ' Returns True (does "BAT123khg" begin with a "B", followed by any single character, followed by a "T", and
        ' end with zero or more characters of any type?)
        Console.WriteLine("  {0,-30}{1}", """BAT123khg"" Like ""B?T*""", ("BAT123khg" Like "B?T*"))

        ' Returns False (does "CAT123khg" begin with a "B", followed by any single character, followed by a "T", and
        ' end with zero or more characters of any type?)
        Console.WriteLine("  {0,-30}{1}", """CAT123khg"" Like ""B?T*""", ("CAT123khg" Like "B?T*"))
        Console.WriteLine("#")
        Console.WriteLine()
    End Sub
    #End Region

    Sub Main()
        ' This application combines some examples from the following C# samples:
        ' * SampleConsole
        ' * StringsDemo
        Console.WriteLine("Caution! Visual Basic is rather disgusting...avoid if possible!")
        Console.WriteLine()
        Dim msgWelcome As String = String.Format("### VB.NET console application, targeting {0} ###", GetFrameworkDisplayName())
        Console.WriteLine(msgWelcome)
        Console.WriteLine()

        ' Which sections are we going to display?
        ' Note: "&H" is a signed hexadecimal literal, so "Dim i As UInt32 = &HFFFFFFFF" results in the BC30439 compile
        ' error due to numerical overflow. Resolve by either:
        '   * Using the largest representable literal for a signed type e.g. "Dim i As UInt32 = &H7FFFFFFF" or
        '   * Add the "UI" or "UL" qualifier onto the end of the string literal e.g. "Dim i As UInt32 = &H7FFFFFFFUI" or
        '   * Project > Properties > Compile > Advanced Compile Options > Remove integer overflow checks
        Const DISPLAY_ALL_SECTIONS As UInt64            = &HFFFFFFFFFFFFFFFFUL ' Generally use this one
        Const DISPLAY_SYS_INFO As UInt64                = &H0000000000000001
        Const DISPLAY_ASSEMBLY_INFO As UInt64           = &H0000000000000002
        Const DISPLAY_LOOPING As UInt64                 = &H0000000000000004
        Const DISPLAY_STRINGS As UInt64                 = &H0000000000000010
        Const DISPLAY_STRINGS_PERFORMANCE As UInt64     = &H0000000000000020
        Const DISPLAY_INTEGERS As UInt64                = &H0000000000000100
        Const DISPLAY_INT_CASTING As UInt64             = &H0000000000000200
        Const DISPLAY_LOGICAL_OPERATORS As UInt64       = &H0000000000001000
        Const DISPLAY_NULLABLE_TYPES As UInt64          = &H0000000000002000
        Const DISPLAY_ENUMS As UInt64                   = &H0000000000004000
        Const DISPLAY_LIKE As UInt64                    = &H0000000000008000
        Dim display As UInt64 = DISPLAY_ALL_SECTIONS

        ' Display some system information
        If ((display And DISPLAY_SYS_INFO) <> 0) Then
            DisplaySystemInformation()
        End If

        ' Show some information about this assembly
        If ((display And DISPLAY_ASSEMBLY_INFO) <> 0) Then
            DisplayAssemblyInfo()
        End If

        ' Loops and other control structures in VB
        If ((display And DISPLAY_LOOPING) <> 0) Then
            VB_Loops()
        End If

        ' Strings
        If ((display And DISPLAY_STRINGS) <> 0) Then
            VB_Strings_Basic()
            VB_Strings_Basic_Search()
        End If

        'If ((display And DISPLAY_STRINGS_PERFORMANCE) <> 0) Then
        '    ' Avoid string performance tests unless actually testing!
        '    VB_Strings_Performance_InStr()
        '    VB_Strings_Performance_InStrRev()
        '    VB_Strings_Performance_Len()
        'End If

        ' Integers
        If ((display And DISPLAY_INTEGERS) <> 0) Then
            VB_Integers()
        End If

        ' Casting (int)
        If ((display And DISPLAY_INT_CASTING) <> 0) Then
            VB_Integers_Casting_From_Object()
            VB_Integers_Casting_From_String()
            VB_Integers_Casting_Between_Integers()
            ' TODO: Measure performance of different casting operations
        End If

        ' Logical operators
        If ((display And DISPLAY_LOGICAL_OPERATORS) <> 0) Then
            VB_LogicalOperators()
        End If

        ' Nullable types
        If ((display And DISPLAY_NULLABLE_TYPES) <> 0) Then
            VB_NullableTypes()
        End If

        ' Enumerations
        If ((display And DISPLAY_ENUMS) <> 0) Then
            VB_Enumerations()
        End If

        ' "Like" keyword
        If ((display And DISPLAY_LIKE) <> 0) Then
            VB_Like()
        End If

        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub

End Module
