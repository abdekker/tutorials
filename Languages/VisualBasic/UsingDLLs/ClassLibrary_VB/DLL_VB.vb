Public Class DLL_VB
    ' Automatically implemented property
    Private Property MyID As Integer

    ' Constructor/Destructor
    Public Sub New()
        ' Constructor which takes no parameters
        Console.WriteLine("    (DLL_VB constructor; no parameters)")
        SetMyID()
    End Sub

    Public Sub New(ByRef param1 As String, ByRef param2 As Integer)
        ' Constructor which takes two parameters
        Console.WriteLine("    (DLL_VB constructor with parameters; param1 = '{0}', param2 = {1})", param1, param2)
        SetMyID()
    End Sub

    Protected Overrides Sub Finalize()
        Console.WriteLine("    (DLL_VB destructor ({0}))", MyID)
    End Sub

    ' Test methods
    Public Sub TestMethod1
        Console.WriteLine("    (DLL_VB::TestMethod1; no parameters)")
    End Sub

    Public Sub TestMethod2(ByRef param1 As String, ByRef param2 As Integer)
        Console.WriteLine("    (DLL_VB::TestMethod2; param1 = '{0}', param2 = {1})", param1, param2)
    End Sub

    ' Helper method
    Private Sub SetMyID()
        ' Set an unique ID to identify this DLL when it is destroyed
        Dim randm As Random = New Random(Environment.TickCount)
        MyID = randm.Next(1000,9999)
    End Sub
End Class
