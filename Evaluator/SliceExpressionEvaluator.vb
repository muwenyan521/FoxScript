Imports System.Numerics

Imports FoxScript.Evaluator
Imports FoxScript.Utils
Imports FoxScript.BooleanUtils
Imports FoxScript.FileSystemUtils
Imports FoxScript.StringUtils
Imports FoxScript.ErrorUtils
Imports FoxScript.StatementUtils


Public Class SliceExpressionEvaluator
    Implements IEvaluatorHandler
    Private ReadOnly _Evaluator As Evaluator

    Public Sub New(ByRef evaluator As Evaluator)
        _Evaluator = evaluator
    End Sub

    ' 实现接口的 Eval 方法
    Public Function Eval(ParamArray Args() As Object) As Fox_Object Implements IEvaluatorHandler.Eval
        Return _Eval(Args(0), Args(1), Args(2), Args(3))
    End Function

    Public Function _Eval(
        Obj As Fox_Object,
        startIndexObject As Fox_Object,
        stopIndexObject As Fox_Object,
        stepObject As Fox_Object
    ) As Fox_Object   '返回一个对象 
        If TypeOf Obj Is Fox_Array Then
            Return _ArrayObjectEval(Obj, startIndexObject, stopIndexObject, stepObject)
        ElseIf TypeOf Obj Is Fox_String Then
            Return _StringObjectEval(Obj, startIndexObject, stopIndexObject, stepObject)
        End If

        Return ThrowError($"不支持的类型 {Obj.Type}")
    End Function

    Private Function _ArrayObjectEval(
        arrayObj As Fox_Object,
        startIndexObject As Fox_Object,
        stopIndexObject As Fox_Object,
        stepObject As Fox_Object
    ) As Fox_Object
        Dim stopIndex As Long

        Dim elements = TryCast(arrayObj, Fox_Array).Elements.ToArray
        ' 处理未设定结束索引的情况
        If stopIndexObject Is Nothing Then
            stopIndex = elements.Length
        Else
            stopIndex = CLng(TryCast(stopIndexObject, Fox_Integer).Value.ToString)
        End If

        Dim startIndex = CLng(TryCast(startIndexObject, Fox_Integer).Value.ToString)
        Dim stepIndex = CLng(TryCast(stepObject, Fox_Integer).Value.ToString)

        ' 处理负数结束索引
        If stopIndex < 0 Then
            stopIndex = elements.Length + stopIndex
        End If

        ' 初始化结果列表
        Dim arr As New List(Of Fox_Object)()

        ' 根据步长进行遍历，实现切片功能
        If stepIndex > 0 Then
            ' 正向步长
            For i As Long = startIndex To stopIndex - 1 Step stepIndex
                If i >= 0 AndAlso i < elements.Length Then
                    arr.Add(elements(i))
                End If
            Next
        ElseIf stepIndex < 0 Then
            ' 反向步长
            For i As Long = startIndex To stopIndex + 1 Step stepIndex
                If i >= 0 AndAlso i < elements.Length Then
                    arr.Add(elements(i))
                End If
            Next
        End If

        Return New Fox_Array With {.Elements = arr}
    End Function

    Private Function _StringObjectEval(
        stringObj As Fox_Object,
        startIndexObject As Fox_Object,
        stopIndexObject As Fox_Object,
        stepObject As Fox_Object
    ) As Fox_Object
        Dim stopIndex As Long

        Dim str As String = TryCast(stringObj, Fox_String).Value
        ' 处理未设定结束索引的情况
        If stopIndexObject Is Nothing Then
            stopIndex = str.Length
        Else
            stopIndex = CLng(TryCast(stopIndexObject, Fox_Integer).Value.ToString)
        End If

        Dim startIndex = CLng(TryCast(startIndexObject, Fox_Integer).Value.ToString)
        Dim stepIndex = CLng(TryCast(stepObject, Fox_Integer).Value.ToString)

        ' 处理负数结束索引
        If stopIndex < 0 Then
            stopIndex = str.Length + stopIndex
        End If

        ' 初始化结果列表
        Dim strObj As New Fox_String With {.Value = ""}

        ' 根据步长进行遍历，实现切片功能
        If stepIndex > 0 Then
            ' 正向步长
            For i As Long = startIndex To stopIndex - 1 Step stepIndex
                If i >= 0 AndAlso i < str.Length Then
                    strObj.Value &= str(i)
                End If
            Next
        ElseIf stepIndex < 0 Then
            ' 反向步长
            For i As Long = startIndex To stopIndex + 1 Step stepIndex
                If i >= 0 AndAlso i < str.Length Then
                    strObj.Value &= str(i)
                End If
            Next
        End If

        Return strObj
    End Function
End Class
