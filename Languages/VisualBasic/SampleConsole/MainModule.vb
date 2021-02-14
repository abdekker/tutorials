Imports System.IO
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

    ' Helper methods
    Private Function GetFrameworkDisplayName()
        Dim attribs() As Object = Assembly.GetExecutingAssembly().GetCustomAttributes(GetType(TargetFrameworkAttribute), False)
        Dim targetFramework As TargetFrameworkAttribute = attribs.SingleOrDefault() ' "SingleOrDefault" requires System.Linq
        GetFrameworkDisplayName = targetFramework.FrameworkDisplayName
    End Function

    ' Main methods
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

    Private Sub StringsInVB()
        ' Some details on strings and string formatting in Visual Basic
        Console.WriteLine("### Strings ###")

        Console.WriteLine("(basic formatting)")
        Dim name As String = "Fred"
        Dim height As Single = 1.936
        Dim age As Integer = 29
        Console.WriteLine("  a) {0} is {1:0.00}m tall and {2} years old [implicit formatting]", name, height, age)
        Console.WriteLine(String.Format("  b) {0} is {1:0.00}m tall and {2} years old [explicit formatting]", name, height, age))
        Console.WriteLine($"  c) {name} is {height:0.00}m tall and {age} years old [string interpolation]")
        Console.WriteLine()

        Console.WriteLine("(searching strings)")
        Dim msg As String = "According to Joe, this is a beautiful day. Joe rides a bicycle!"
        Dim joe As String = "Joe"
        Dim joeLower As String = "joe"
        Dim jedi As String = "Jedi"
        Console.WriteLine("  (string = '{0}')", msg)
        Console.WriteLine()

        Console.WriteLine("  (InStr & InStrRev - these are legacy and can be avoided)")
        Console.WriteLine("    Instr ({0}):             {1,3}", joe, InStr(msg, joe))
        Console.WriteLine("    Instr (Binary) ({0}):    {1,3}", joeLower, InStr(msg, joeLower))
        Console.WriteLine("    Instr (Text)  ({0}):     {1,3}", joeLower, InStr(msg, joeLower, CompareMethod.Text))
        Console.WriteLine("    Instr ({0}):            {1,3}", jedi, InStr(msg, jedi))
        Console.WriteLine()
        Console.WriteLine("    InstrRev ({0}):          {1,3}", joe, InStrRev(msg, joe))
        Console.WriteLine("    InStrRev (Binary) ({0}): {1,3}", joeLower, InStrRev(msg, joeLower))
        Console.WriteLine("    InStrRev (Text)  ({0}):  {1,3}", joeLower, InStrRev(msg, joeLower, -1, CompareMethod.Text))
        Console.WriteLine("    InStrRev ({0}):         {1,3}", jedi, InStrRev(msg, jedi))
        Console.WriteLine()

        Console.WriteLine("  (IndexOf & LastIndexOf - modern .NET equivalents)")
        Console.WriteLine("    IndexOf ({0}):               {1,3}", joe, msg.IndexOf(joe))
        Console.WriteLine("    IndexOf (case) ({0}):        {1,3}", joe, msg.IndexOf(joeLower))
        Console.WriteLine("    IndexOf ({0}):               {1,3}", joe, msg.IndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase))
        Console.WriteLine("    IndexOf ({0}):               {1,3}", joe, msg.IndexOf(jedi))
        Console.WriteLine()
        Console.WriteLine("    LastIndexOf ({0}):           {1,3}", joe, msg.LastIndexOf(joe))
        Console.WriteLine("    LastIndexOf (case) ({0}):    {1,3}", joe, msg.LastIndexOf(joeLower))
        Console.WriteLine("    LastIndexOf (no case) ({0}): {1,3}", joe, msg.LastIndexOf(joeLower, StringComparison.InvariantCultureIgnoreCase))
        Console.WriteLine("    LastIndexOf ({0}):           {1,3}", joe, msg.LastIndexOf(jedi))

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub NullableTypesInVB()
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
                If (item?.name = Nothing, "(blank)", item?.name),
                If (item?.age = Nothing, -1, item?.age))
            index += 1
        Next

        Console.WriteLine("#")
        Console.WriteLine()
    End Sub

    Private Sub EnumerationsInVB()
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

    Sub Main()
        ' This application combines some examples from these C# samples:
        ' * SampleConsole
        ' * StringsDemo
        Console.WriteLine("Caution! Visual Basic is rather disgusting...avoid!")
        Console.WriteLine()
        Dim msgWelcome As String = String.Format("### VB.NET console application, targeting {0} ###", GetFrameworkDisplayName())
        Console.WriteLine(msgWelcome)
        Console.WriteLine()

        ' Display some system information
        'DisplaySystemInformation()

        ' Show some information about this assembly
        'DisplayAssemblyInfo()

        ' Additional demonstrations
        StringsInVB()
        'NullableTypesInVB()
        'EnumerationsInVB()

        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()
    End Sub

End Module
