Imports System.Numerics

Imports FoxScript.Evaluator
Imports FoxScript.Utils
Imports FoxScript.BooleanUtils
Imports FoxScript.FileSystemUtils
Imports FoxScript.StringUtils
Imports FoxScript.ErrorUtils
Imports FoxScript.StatementUtils


Public Class InfixExpressionEvaluator
    Implements IEvaluatorHandler
    Private ReadOnly _Evaluator As Evaluator
    Private ReadOnly _TypeHandlers = New Dictionary(Of ObjectType, Func(Of String, Fox_Object, Fox_Object, Fox_Object)) From {
        {ObjectType.INTEGER_OBJ, AddressOf EvalIntegerInfixExpression},
        {ObjectType.BOOL_OBJ, AddressOf EvalBoolInfixExpression},
        {ObjectType.STRING_OBJ, AddressOf EvalStringInfixExpression},
        {ObjectType.DOUBLE_OBJ, AddressOf EvalDoubleInfixExpression},
        {ObjectType.ARRAY_OBJ, AddressOf EvalArrayInfixExpression},
        {ObjectType.NOTHINGL_OBJ, AddressOf EvalNothingInfixExpression}
    }

    Private ReadOnly _ConvertHandlers = New Dictionary(Of ObjectType, Func(Of IEnumerable(Of Object), Object)) From {
        {ObjectType.INTEGER_OBJ, Builtins.builtinFuncs("CInt").BuiltinFunction},
        {ObjectType.BOOL_OBJ, Builtins.builtinFuncs("CBool").BuiltinFunction},
        {ObjectType.STRING_OBJ, Builtins.builtinFuncs("CStr").BuiltinFunction},
        {ObjectType.ARRAY_OBJ, Builtins.builtinFuncs("CArray").BuiltinFunction}
    }

    Public Sub New(ByRef evaluator As Evaluator)
        _Evaluator = evaluator
    End Sub

    ' 实现接口的 Eval 方法
    Public Function Eval(ParamArray Args() As Object) As Fox_Object Implements IEvaluatorHandler.Eval
        If Args.Length = 3 AndAlso TypeOf Args(0) Is String AndAlso TypeOf Args(1) Is Fox_Object AndAlso TypeOf Args(2) Is Fox_Object Then
            Return ThrowError("参数错误")
        Else
            Return Eval(DirectCast(Args(0), String), DirectCast(Args(1), Fox_Object), DirectCast(Args(2), Fox_Object))
        End If
    End Function

    Public Function Eval(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object '操作符右边的对象
    ) As Fox_Object   '返回一个对象 

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        If Left.Type = ObjectType.CLASS_OBJ OrElse Right.Type = ObjectType.CLASS_OBJ Then
            Return EvalClassObjectInfixExpression(operator_, Left, Right)
        End If

        ' 根据类型选择不同的处理函数
        Dim handler As Func(Of String, Fox_Object, Fox_Object, Fox_Object) = GetTypeHandlers(Left.Type, Right.Type)
        If handler IsNot Nothing Then
            Return handler(operator_, GetType_ConvertHandlers(Left.Type, Right.Type)({Left}), GetType_ConvertHandlers(Left.Type, Right.Type)({Right}))
        Else
            Return ThrowError($"类型错误: {Left.Type} {operator_} {Right.Type}")
        End If
    End Function

    Private Function GetTypeHandlers(leftType As ObjectType, rightType As ObjectType) As Func(Of String, Fox_Object, Fox_Object, Fox_Object)
        If leftType = ObjectType.DOUBLE_OBJ OrElse rightType = ObjectType.DOUBLE_OBJ Then
            Return AddressOf EvalDoubleInfixExpression
        End If

        If _TypeHandlers.ContainsKey(leftType) Then
            Return _TypeHandlers(leftType)
        End If

        Return Nothing

    End Function

    Private Function EvalClassObjectInfixExpression(
        operator_ As String,
        Left As Fox_Object,
        Right As Fox_Object
    ) As Fox_Object

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        If Left.Type <> ObjectType.CLASS_OBJ Then
            Return ThrowError($"未知的操作: {Left.Type} {operator_} {TryCast(Right, Fox_Class).Name.Value}")
        End If

        If Right.Type <> ObjectType.CLASS_OBJ Then
            Return ThrowError($"未知的操作: {TryCast(Left, Fox_Class).Name.Value} {operator_} {Right.Type}")
        End If

        Dim leftObject As Fox_Class = Left
        Dim rightObject As Fox_Class = Right

        Dim functionObject = Nothing
        Select Case operator_
            Case "+"
                Dim AddFuncLitreal As FunctionLiteral = FindFunctionLiteral("__Add__", leftObject.Body.Statements)
                If AddFuncLitreal Is Nothing Then Return ThrowError($"未知的操作:{leftObject.Name.Value} {operator_} {rightObject.Name.Value}")

                functionObject = _Evaluator.Eval(AddFuncLitreal, leftObject.Env)
                If IsError(functionObject) Then Return functionObject

                Return _Evaluator.ApplyFunction(functionObject, New List(Of Fox_Object) From {rightObject})
            Case "-"
                Dim MinusFuncLitreal As FunctionLiteral = FindFunctionLiteral("__Subtract__", leftObject.Body.Statements)
                If MinusFuncLitreal Is Nothing Then Return ThrowError($"未知的操作:{leftObject.Name.Value} {operator_} {rightObject.Name.Value}")

                functionObject = _Evaluator.Eval(MinusFuncLitreal, leftObject.Env)
                If IsError(functionObject) Then Return functionObject

                Return _Evaluator.ApplyFunction(functionObject, New List(Of Fox_Object) From {rightObject})
            Case "*"
                Dim MultiplyFuncLitreal As FunctionLiteral = FindFunctionLiteral("__Multiply__", leftObject.Body.Statements)
                If MultiplyFuncLitreal Is Nothing Then Return ThrowError($"未知的操作:{leftObject.Name.Value} {operator_} {rightObject.Name.Value}")

                functionObject = _Evaluator.Eval(MultiplyFuncLitreal, leftObject.Env)
                If IsError(functionObject) Then Return functionObject

                Return _Evaluator.ApplyFunction(functionObject, New List(Of Fox_Object) From {rightObject})
            Case "/"
                Dim DivideFuncLitreal As FunctionLiteral = FindFunctionLiteral("__Divide__", leftObject.Body.Statements)
                If DivideFuncLitreal Is Nothing Then Return ThrowError($"未知的操作:{leftObject.Name.Value} {operator_} {rightObject.Name.Value}")

                functionObject = _Evaluator.Eval(DivideFuncLitreal, leftObject.Env)
                If IsError(functionObject) Then Return functionObject

                Return _Evaluator.ApplyFunction(functionObject, New List(Of Fox_Object) From {rightObject})
            Case Else
                Return ThrowError($"未知的操作: {TryCast(Left, Fox_Class).Name.Value} {operator_} {TryCast(Right, Fox_Class).Name.Value}")
        End Select
    End Function
    Private Function GetType_ConvertHandlers(leftType As ObjectType, rightType As ObjectType) As Func(Of IEnumerable(Of Object), Object)
        If leftType = ObjectType.NOTHINGL_OBJ AndAlso rightType = ObjectType.NOTHINGL_OBJ Then Return Builtins.builtinFuncs("CNothing").BuiltinFunction
        If leftType = ObjectType.DOUBLE_OBJ OrElse rightType = ObjectType.DOUBLE_OBJ Then Return Builtins.builtinFuncs("CDbl").BuiltinFunction

        If leftType = ObjectType.NOTHINGL_OBJ Then
            Return GetType_ConvertHandlers(rightType, rightType)
        End If

        If _ConvertHandlers.ContainsKey(leftType) Then
            Return _ConvertHandlers(leftType)
        End If

        Return Nothing

        'Select Case leftType
        '    Case ObjectType.INTEGER_OBJ
        '        Return Builtins.builtinFuncs("CInt").BuiltinFunction
        '    Case ObjectType.BOOL_OBJ
        '        Return Builtins.builtinFuncs("CBool").BuiltinFunction
        '    Case ObjectType.STRING_OBJ
        '        Return Builtins.builtinFuncs("CStr").BuiltinFunction
        '    Case ObjectType.ARRAY_OBJ
        '        Return Builtins.builtinFuncs("CArray").BuiltinFunction
        '    Case ObjectType.NOTHINGL_OBJ
        '        Return GetType_ConvertHandlers(rightType, rightType)
        '    Case Else
        '        Return Nothing
        'End Select
    End Function
    Public Function EvalArrayInfixExpression(
        operator_ As String,
        Left As Fox_Object,
        Right As Fox_Object
    ) As Fox_Object

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        Dim leftVal = TryCast(Left, Fox_Array).Elements
        Dim rightVal = TryCast(Right, Fox_Array).Elements

        '判断操作符 并调用函数获取对应的Fox_String对象
        Select Case operator_
            Case "+"
                Dim r = leftVal
                For Each item As Fox_Object In rightVal
                    r.Add(item)
                Next

                Return New Fox_Array With {.Elements = r}
            Case "==" '等于
                If leftVal.Count <> rightVal.Count Then Return New Fox_Bool With {.Value = False}
                Dim results = New List(Of Boolean)

                For i = 0 To leftVal.Count - 1
                    Dim left_obj = leftVal(i)
                    Dim right_obj = rightVal(i)

                    Dim obj = Eval("==", left_obj, right_obj)
                    Dim bool_obj = TryCast(obj, Fox_Bool)

                    If IsError(obj) Then Return Fox_False
                    results.Add(bool_obj.Value)
                Next

                Return New Fox_Bool With {.Value = Not results.Contains(False)}
            Case "!=", "<>" '不等于
                If leftVal.Count <> rightVal.Count Then Return New Fox_Bool With {.Value = True}
                Dim results = New List(Of Boolean)

                For i = 0 To leftVal.Count - 1
                    Dim left_obj = leftVal(i)
                    Dim right_obj = rightVal(i)

                    Dim obj = Eval("<>", left_obj, right_obj)
                    Dim bool_obj = TryCast(obj, Fox_Bool)

                    If IsError(obj) Then Return Fox_False
                    results.Add(bool_obj.Value)
                Next

                Return New Fox_Bool With {.Value = results.Contains(True)}
            Case Else
                '信息大致内容: 未知的操作符 : [左值] [运算符] [右值]
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Right.Type}")
        End Select
    End Function


    Public Function EvalStringInfixExpression(
        operator_ As String,
        Left As Fox_Object,
        Right As Fox_Object
    ) As Fox_Object

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        Dim leftVal = TryCast(Left, Fox_String).Value
        Dim rightVal = TryCast(Right, Fox_String).Value

        '判断操作符 并调用函数获取对应的Fox_String对象
        Select Case operator_
            Case "+"
                Return New Fox_String With {.Value = leftVal & rightVal}
            Case "==" '等于
                Return New Fox_Bool With {.Value = leftVal = rightVal}
            Case "!=", "<>" '不等于
                Return New Fox_Bool With {.Value = leftVal <> rightVal}
            Case Else
                '信息大致内容: 未知的操作符 : [左值] [运算符] [右值]
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Right.Type}")
        End Select
    End Function

    '求布尔值
    Public Function EvalBoolInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object'操作符右边的对象
    ) As Fox_Object '返回一个 Fox_Object对象

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        '尝试转换对象为 Fox_Bool型并获取布尔值
        Dim leftVal = TryCast(Left, Fox_Bool).Value
        Dim rightVal = TryCast(Right, Fox_Bool).Value

        '判断操作符 并调用函数获取对应的Fox_Bool对象
        Select Case operator_
            Case "+" '加
                Return New Fox_Integer With {.Value = New BigInteger(BoolToLong(leftVal) + BoolToLong(rightVal))}
            Case "-" '减
                Return New Fox_Integer With {.Value = New BigInteger(BoolToLong(leftVal) - BoolToLong(rightVal))}
            Case "*" '乘
                Return New Fox_Integer With {.Value = New BigInteger(BoolToLong(leftVal) * BoolToLong(rightVal))}
            Case "/" '除
                If rightVal = 0 Then '除数为0
                    '报错: 除数不能为0
                    Return ThrowError("除数不能为0")
                End If

                Return New Fox_Double With {.Value = BoolToLong(leftVal) / BoolToLong(rightVal)}
            Case "<" '小于
                Return NativeBoolToBooleanObject(BoolToLong(leftVal) < BoolToLong(rightVal))
            Case ">" '大于
                Return NativeBoolToBooleanObject(BoolToLong(leftVal) > BoolToLong(rightVal))
            Case "==" '等于
                Return NativeBoolToBooleanObject(leftVal = rightVal)
            Case "!=", "<>" '不等于
                Return NativeBoolToBooleanObject(leftVal <> rightVal)
            Case Else
                '信息大致内容: 未知的操作符 : [左值] [运算符] [右值]
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Right.Type}")
        End Select
    End Function

    Public Function EvalNothingInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object ' 操作符右边的对象
    ) As Fox_Object

        Dim leftVal = If(TypeOf Left Is Fox_Nothing, Nothing, CType(Left, Object).Value)
        Dim rightVal = If(TypeOf Right Is Fox_Nothing, Nothing, CType(Right, Object).Value)


        '判断操作符并新建对应对象
        Select Case operator_
            Case "==" '等于
                Return NativeBoolToBooleanObject(leftVal Is rightVal)  '返回Fox_Bool类型对象
            Case "!=", "<>" '不等于
                Return NativeBoolToBooleanObject(leftVal IsNot rightVal)  '返回Fox_Bool类型对象
            Case Else
                '未知的操作符 : [左值] [操作符] [右值]
                Return ThrowError($"未知的操作: {leftVal} {operator_} {rightVal}")
        End Select
    End Function


    '求整数值
    Public Function EvalIntegerInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object ' 操作符右边的对象
    ) As Fox_Object

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        '尝试转换对象为Fox_Integer类型并获取数值
        Dim leftVal = TryCast(Left, Fox_Integer).Value
        Dim rightVal = TryCast(Right, Fox_Integer).Value



        '判断操作符并新建对应对象
        Select Case operator_
            Case "+"
                Return New Fox_Integer With {.Value = leftVal + rightVal} '返回Fox_Integer对象
            Case "-"
                Return New Fox_Integer With {.Value = leftVal - rightVal}  '返回Fox_Integer对象
            Case "*"
                Return New Fox_Double With {.Value = CDbl(leftVal.ToString) * CDbl(rightVal.ToString)}  '返回Fox_Integer对象
            Case "/"
                If rightVal = 0 Then '除数为0
                    '报错: 除数不能为0
                    Return ThrowError("除数不能为0")
                End If

                Return New Fox_Double With {.Value = CDbl(leftVal.ToString) / CDbl(rightVal.ToString)}  '返回Fox_Double对象
            Case "<" '小于
                Return NativeBoolToBooleanObject(leftVal < rightVal)  '返回Fox_Bool类型对象
            Case ">" '大于
                Return NativeBoolToBooleanObject(leftVal > rightVal)  '返回Fox_Bool类型对象
            Case "==" '等于
                Return NativeBoolToBooleanObject(leftVal = rightVal)  '返回Fox_Bool类型对象
            Case "!=", "<>" '不等于
                Return NativeBoolToBooleanObject(leftVal <> rightVal)  '返回Fox_Bool类型对象
            Case Else
                '未知的操作符 : [左值] [操作符] [右值]
                Return ThrowError($"未知的操作: {leftVal} {operator_} {rightVal}")
        End Select
    End Function

    '求小数值
    Public Function EvalDoubleInfixExpression(
    operator_ As String, '操作符
    Left As Fox_Object, '操作符左边的对象
    Right As Fox_Object ' 操作符右边的对象
) As Fox_Object

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

        '尝试转换对象为Fox_Integer类型并获取数值
        Dim leftVal = TryCast(Left, Fox_Double).Value
        Dim rightVal = TryCast(Right, Fox_Double).Value

        '判断操作符并新建对应对象
        Select Case operator_
            Case "+"
                Return New Fox_Double With {.Value = leftVal + rightVal} '返回Fox_Double对象
            Case "-"
                Return New Fox_Double With {.Value = leftVal - rightVal}  '返回Fox_Double对象
            Case "*"
                Return New Fox_Double With {.Value = leftVal * rightVal}  '返回Fox_Double对象
            Case "/"
                If rightVal = 0 Then '除数为0
                    '报错: 除数不能为0
                    Return ThrowError("除数不能为0")
                End If

                Return New Fox_Double With {.Value = leftVal / rightVal}  '返回Fox_Double对象
            Case "<" '小于
                Return NativeBoolToBooleanObject(leftVal < rightVal)  '返回Fox_Double对象
            Case ">" '大于
                Return NativeBoolToBooleanObject(leftVal > rightVal)  '返回Fox_Double对象
            Case "==" '等于
                Return NativeBoolToBooleanObject(leftVal = rightVal)  '返回Fox_Double对象
            Case "!=", "<>" '不等于
                Return NativeBoolToBooleanObject(leftVal <> rightVal)  '返回Fox_Double对象
            Case Else
                '未知的操作符 : [左值] [操作符] [右值]
                Return ThrowError($"未知的操作: {leftVal} {operator_} {rightVal}")
        End Select
    End Function

End Class
