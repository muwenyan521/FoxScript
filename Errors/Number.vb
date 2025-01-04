Public Class NumberError
    Public Shared ErrorDictionary As New Dictionary(Of Type, String) From
    {
        {GetType(OverflowException), "数值溢出"},
        {GetType(FormatException), "不是一个数值"}
    }

End Class
