'Public Class Environment
'    Public store As New Dictionary(Of String, Fox_Object)
'    Public outer As Environment = Nothing '外层环境

'    '获取一个名字的值 返回 一个有Object和Boolean的元组
'    Public Function GetValue(name As String) As ValueTuple(Of Object, Boolean)
'        '...
'        Dim n = name.Replace(vbCr, "").Replace(vbLf, "").Replace(" ", "")

'        '初始化
'        Dim obj As Object = Nothing
'        Dim ok As Boolean = False

'        '尝试获取值
'        If store.ContainsKey(n) Then
'            ok = True
'            obj = store(n)
'            '返回元组
'            Return New ValueTuple(Of Object, Boolean)(obj, ok)
'        End If

'        If outer IsNot Nothing Then
'            Dim outer_result = outer.GetValue(n)
'            If outer_result.Item2 Then
'                Return outer_result
'            End If
'        End If

'        Return New ValueTuple(Of Object, Boolean)(obj, ok)
'    End Function

'    '设置指定名字的值，返回一个Fox_Object类型的对象
'    Public Function SetValue(name As String, val As Fox_Object) As Fox_Object
'        ...
'        Dim n = name.Replace(vbCr, "").Replace(vbLf, "").Replace(" ", "")

'        '如果字典包含名字
'        If store.ContainsKey(n) Then
'            '设置值
'            store(n) = val
'        Else '否则
'            '添加到字典
'            store.Add(n, val)
'        End If
'        Return val
'    End Function

'End Class

Public Class Data
    Public FoxObject As Fox_Object '数据
    Public isReadonly As Boolean '是否为常量

    Public Sub New(foxObj As Fox_Object, isReadonly As Boolean)
        FoxObject = foxObj
        Me.isReadonly = isReadonly
    End Sub
End Class

Public Class Environment
    Public store As New Dictionary(Of String, Data)
    Public outer As Environment = Nothing '外层环境

    '获取一个名字的值 返回 一个有Object和Boolean的元组
    Public Function GetValue(name As String) As ValueTuple(Of Object, Boolean)
        '...
        Dim n = name.Replace(vbCr, "").Replace(vbLf, "").Replace(" ", "")

        '初始化
        Dim obj As Object = Nothing
        Dim ok As Boolean = False

        '尝试获取值
        If store.ContainsKey(n) Then
            ok = True
            obj = store(n).FoxObject
            '返回元组
            Return New ValueTuple(Of Object, Boolean)(obj, ok)
        End If

        If outer IsNot Nothing Then
            Dim outer_result = outer.GetValue(n)
            If outer_result.Item2 Then
                Return outer_result
            End If
        End If

        Return New ValueTuple(Of Object, Boolean)(obj, ok)
    End Function

    '设置指定名字的值，返回一个Fox_Object类型的对象，并指定是否为只读
    Public Function SetValue(name As String, val As Fox_Object, isReadonly As Boolean) As Fox_Object
        '...
        Dim n = name.Replace(vbCr, "").Replace(vbLf, "").Replace(" ", "")

        '如果字典包含名字
        If store.ContainsKey(n) Then
            '设置值
            If store(n).isReadonly Then
                Return New Fox_Error($"{n} 为只读!")
            End If

            store(n) = New Data(val, isReadonly)
        Else '否则
            '添加到字典
            store.Add(n, New Data(val, isReadonly))
        End If
        Return val
    End Function

    '从字典删除指定名字的标识符
    Public Sub RemoveValue(name As String)
        '...
        Dim n = name.Replace(vbCr, "").Replace(vbLf, "").Replace(" ", "")

        '如果字典包含名字
        If store.ContainsKey(n) Then
            store.Remove(n)
        End If
    End Sub
End Class


