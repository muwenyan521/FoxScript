Public Class Fail
    Inherits Exception
End Class

Public Class OK
    Inherits Exception
End Class

Public Class CannotFindCustomErrorException
    Inherits Exception
    Public Sub New(Message As String)
        MyBase.New(Message)
    End Sub
    Public Sub New()
    End Sub
End Class