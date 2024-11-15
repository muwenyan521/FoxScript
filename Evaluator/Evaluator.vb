Imports System.Reflection
Imports System.Windows.Forms.VisualStyles.VisualStyleElement.Tab

Public Class Evaluator
    Public Shared Fox_True As Fox_Bool = New Fox_Bool() With {.Value = True}
    Public Shared Fox_False As Fox_Bool = New Fox_Bool() With {.Value = False}
    Public Shared Fox_Nothing As Fox_Nothing = New Fox_Nothing()
    Public Fox_One As Fox_Integer = New Fox_Integer() With {.Value = 1}
    Public Fox_Zero As Fox_Integer = New Fox_Integer() With {.Value = 0}

    '判断是否为Error对象. 返回一个布尔值
    Public Function isError(obj As Fox_Object)
        If obj Is Nothing Then Return False
        Return obj.Type = ObjectType.ERROR_OBJ
    End Function

    '求值函数 返回 Fox_Object 类型的对象
    Public Function eval(node As Object, ByRef env As Environment) As Fox_Object
        If node Is Nothing Then Return Nothing '判空

        Select Case node.GetType() '获取类型
            Case GetType(Program) '如果是Program

                '求所有语句的Fox_Object对象
                Return evalProgram(node.Statements, env)
            Case GetType(ExpressionStatement) '若为表达式

                '把这个表达式丢给自己判断
                Return eval(node.Expression, env)
            Case GetType(IntegerLiteral) '若为整数

                '返回一个 Fox_Integer类型 的对象
                Return New Fox_Integer With {.Value = node.Value}
            Case GetType(DoubleLiteral) '若为整数

                '返回一个 Fox_Double类型 的对象
                Return New Fox_Double With {.Value = node.Value}
            Case GetType(Bool) '若为布尔类型

                '调用函数获取对应的Fox_Bool对象
                Return nativeBoolToBooleanObject(node.Value)
            Case GetType(PrefixExpression) '前缀表达式
                '把右边内容丢给自己
                Dim right = eval(node.Right, env)

                '是否为错误对象
                If isError(right) Then Return right
                '计算值
                Return evalPrefixExpression(node.Operator_, right)
            Case GetType(InfixExpression) '中缀表达式

                '把左边内容丢给自己判断
                Dim Left = eval(node.Left, env)
                '把右边内容丢给自己判断
                Dim Right = eval(node.Right, env)

                '是否为错误对象
                If isError(Left) Then Return Left
                If isError(Right) Then Return Right

                '计算值
                Return evalInfixExpression(node.Operator_, Left, Right)
            Case GetType(BlockStatement) '若为代码块

                ' 求语句所有的Fox_Object类型的对象
                Return evalBlockStatement(node, env)
            Case GetType(IfExpression) '若为 if表达式

                '调用函数计算表达式
                Return evalIfExpression(node, env)
            Case GetType(ReturnStatement) '若为返回语句
                Dim val = eval(node.ReturnValue, env)

                '是否为错误对象
                If isError(val) Then Return val
                Return New Fox_ReturnValue With {.Value = val}

            Case GetType(DimStatement) '若为 Dim 语句
                Dim dim_stmt = TryCast(node, DimStatement)

                Dim get_Ident = env.GetValue(dim_stmt.Name.ToString)
                If get_Ident.Item2 Then
                    Return ThrowError($"标识符 {dim_stmt.Name.ToString} 已存在! 内容为:{get_Ident.Item1.Inspect}")
                End If

                Dim value = eval(dim_stmt.Value, env)

                '检查是否为错误对象
                If isError(value) Then Return value

                env.SetValue(dim_stmt.Name.Value, value)

            Case GetType(FunctionLiteral)
                '获取函数名
                Dim funcName = TryCast(node, FunctionLiteral).Name
                env.SetValue(funcName.Value, Nothing)

                '获取形参
                Dim params = TryCast(node, FunctionLiteral).Parameters

                '获取函数代码块
                Dim body = node.Body
                env.SetValue(funcName.Value, New Fox_Function With {.Parameters = params, .Env = env, .Body = body, .Name = funcName})
                '返回
                Return env.GetValue(funcName.Value).Item1

            Case GetType(CallExpression) '若为调用表达式

                '计算表达式的值
                Dim Func = eval(node.Func, env)

                '判断是否为错误对象
                If isError(Func) Then Return Func

                '计算所有表达式实参
                Dim args = evalExpressions(node.Arguments, env)

                '如果参数数量为1 并且 这一个参数为错误对象 返回这一个参数
                If args.Count = 1 AndAlso isError(args(0)) Then Return args(0)

                Return applyFunction(Func, args)
            'Case GetType(ObjectCreateExpression) '若为创建对象表达式
            '    Dim objCreateExp = TryCast(node, ObjectCreateExpression)

            '    '计算表达式的值
            '    Dim ObjType = eval(objCreateExp.ObjType, env)

            '    '判断是否为错误对象
            '    If isError(ObjType) Then Return ObjType

            '    '计算所有表达式实参
            '    Dim args = evalExpressions(objCreateExp.Arguments, env)

            '    '如果参数数量为1 并且 这一个参数为错误对象 返回这一个参数
            '    If args.Count = 1 AndAlso isError(args(0)) Then Return args(0)

            '    Return applyFunction(ObjType, args)

            Case GetType(Identifier) '若为标识符

                '求标识符的值
                Return evalIdentifier(DirectCast(node, Identifier), env)
            Case GetType(StringLiteral)
                Return New Fox_String With {.Value = node.Value}
            Case GetType(ArrayLiteral)
                Dim elements = evalExpressions(node.Elements, env)
                If elements.Count = 1 AndAlso isError(elements(0)) Then
                    Return elements(0)
                End If
                Return New Fox_Array With {.Elements = elements}

            Case GetType(IndexExpression）
                Dim Left = eval(node.Left, env)
                If isError(Left) Then
                    Return Left
                End If
                Dim Index = eval(node.Index, env)
                If isError(Index) Then
                    Return Index
                End If
                Return evalIndexExpression(Left, Index)
            Case GetType(DictionaryLiteral)
                Return evalHashLiteral(node, env)
            Case GetType(ForStatement)
                Dim for_stmt = TryCast(node, ForStatement)

                Dim items = eval(for_stmt.Items, env)

                '检查是否为错误对象
                If isError(items) Then Return items

                Dim get_itemVar = env.GetValue(for_stmt.ItemVar.ToString)
                If get_itemVar.Item2 Then
                    Return ThrowError($"标识符 {for_stmt.ItemVar} 已存在! 内容为:{get_itemVar.Item1.Inspect}")
                End If


                If items.Type <> ObjectType.ARRAY_OBJ Then Return ThrowError($"for循环不支持遍历 {items.Type} 的值")
                For Each __item__ As Fox_Object In TryCast(items, Fox_Array).Elements
                    '注册迭代变量
                    env.SetValue(for_stmt.ItemVar.ToString, __item__)

                    Dim r = eval(for_stmt.LoopBlock, env)
                    If r Is Nothing Then Continue For
                    r.Inspect()
                Next
            Case GetType(WhileStatement)
                Dim while_stmt = TryCast(node, WhileStatement)

                Dim cond_obj = eval(while_stmt.LoopCondition, env)

                '检查是否为错误对象
                If isError(cond_obj) Then Return cond_obj

                If cond_obj.Type <> ObjectType.BOOL_OBJ Then Return ThrowError($"while循环不支持条件为 {cond_obj.Type} 的值")

                While TryCast(cond_obj, Fox_Bool).Value
                    cond_obj = eval(while_stmt.LoopCondition, env)
                    Dim r = eval(while_stmt.LoopBlock, env)
                    If r Is Nothing Then Continue While
                    r.Inspect()
                End While
            Case GetType(AssignmentExpression)
                Dim val = eval(node.Value, env)

                '检查是否为错误对象
                If isError(val) Then Return val

                Dim assignmentExp = TryCast(node, AssignmentExpression)
                Dim get_Ident = env.GetValue(assignmentExp.Identifier.ToString)
                If Not get_Ident.Item2 Then
                    Return ThrowError($"标识符 {assignmentExp.Identifier.ToString} 不存在!")
                End If

                '设置值
                env.SetValue(assignmentExp.Identifier.ToString, eval(assignmentExp.Value, env))
            Case GetType(ObjectCallExpression)
                Dim call_exp = TryCast(node, ObjectCallExpression)
                Dim obj = eval(call_exp.Obj, env)

                If isError(obj) Then Return obj

                Dim func_name = Nothing
                If TypeOf call_exp.Func Is InfixExpression Then
                    func_name = TryCast(TryCast(call_exp.Func, InfixExpression).Left, Object).Func.Value
                Else
                    func_name = TryCast(call_exp.Func, Object).Func.Value
                End If

                Dim func As Fox_Builtin = Nothing
                If TryCast(obj, Object).Funcs.ContainsKey(func_name) Then
                    func = TryCast(obj, Object).Funcs(func_name)
                Else
                    Return ThrowError($"{TryCast(obj, Object).Type} 类型的对象没有 {func_name} 函数")
                End If

                '计算所有表达式实参  
                Dim args = evalExpressions(If(call_exp.Arguments IsNot Nothing, call_exp.Arguments, New List(Of Expression)), env)

                '如果参数数量为1 并且 这一个参数为错误对象 返回这一个参数  
                If args.Count = 1 AndAlso isError(args(0)) Then Return args(0)

                '应用函数并获得结果  
                Dim result = applyFunction(func, args)

                '检查是否有链式调用  
                If TypeOf result Is Object AndAlso TryCast(node.Obj, ObjectCallExpression) IsNot Nothing Then
                    '获取父节点作为可能的链式调用  
                    Dim parentCallExp = TryCast(node.Obj, ObjectCallExpression)

                    '如果父节点是一个对象调用表达式，并且它的对象是刚刚计算的结果  递归地处理链式调用 
                    If parentCallExp.Obj Is call_exp Then
                        Return eval(parentCallExp, env)
                    End If
                End If

                '如果没有链式调用或链式调用处理完毕，返回当前调用的结果  
                Return result
            Case GetType(NotExpression)
                Dim notExp = TryCast(node, NotExpression)
                Dim rightObject = eval(notExp.Right, env)
                If isError(rightObject) Then Return rightObject

                Return nativeBoolToBooleanObject(Not isTruthy(rightObject))

            Case GetType(AndExpression)
                Dim andExp = TryCast(node, AndExpression)

                Dim leftObject = eval(andExp.Left, env)
                If isError(leftObject) Then Return leftObject

                Dim leftBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({leftObject}), Fox_Object)
                If Not isTruthy(leftBoolObject) Then
                    Return Fox_False
                End If

                Dim rightObject = eval(andExp.Right, env)
                If isError(rightObject) Then Return rightObject
                Dim rightBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({rightObject}), Fox_Object)

                If Not isTruthy(rightBoolObject) Then
                    Return Fox_False
                End If

                Return Fox_True

            Case GetType(OrExpression)
                Dim orExp = TryCast(node, OrExpression)

                Dim leftObject = eval(orExp.Left, env)
                If isError(leftObject) Then Return leftObject

                Dim leftBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({leftObject}), Fox_Object)
                If isTruthy(leftBoolObject) Then
                    Return Fox_True
                End If

                Dim rightObject = eval(orExp.Right, env)
                If isError(rightObject) Then Return rightObject
                Dim rightBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({rightObject}), Fox_Object)

                If isTruthy(rightBoolObject) Then
                    Return Fox_True
                End If

                Return Fox_False
        End Select

        Return Nothing
    End Function

    Public Function evalHashLiteral(
        Node As DictionaryLiteral,
        env As Environment
    ) As Fox_Object
        Dim pairs = New Dictionary(Of Fox_DictionaryKey, Fox_DictionaryPair)

        For Each parisNode In Node.Pairs
            Dim keyNode = parisNode.Key
            Dim valueNode = parisNode.Value

            Dim key = eval(keyNode, env)
            If isError(key) Then Return key

            Dim dictKeyType = key.Type
            Dim dictKey As Object = key
            Dim dictValue = eval(valueNode, env)
            If isError(dictValue) Then Return dictValue

            Select Case dictKeyType
                Case ObjectType.INTEGER_OBJ
                    pairs.Add(
                        Fox_DictionaryHelper.CreateKey(dictKey),
                        New Fox_DictionaryPair With {
                            .Key = Fox_DictionaryHelper.CreateKey(dictKey),
                            .Value = dictValue
                        }
                    )
                Case ObjectType.STRING_OBJ

                    Dim val = TryCast(dictKey, Fox_String).Inspect

                    pairs.Add(
                        Fox_DictionaryHelper.CreateKey(
                            New Fox_String() With {.Value = val}
                        ),
                        New Fox_DictionaryPair With {
                            .Key =
                                Fox_DictionaryHelper.CreateKey(
                                    New Fox_String() With {.Value = val}
                                ),
                            .Value = dictValue
                        }
                    )
                Case ObjectType.BOOL_OBJ

                    pairs.Add(
                        Fox_DictionaryHelper.CreateKey(dictKey),
                        New Fox_DictionaryPair With {
                            .Key = Fox_DictionaryHelper.CreateKey(dictKey),
                            .Value = dictValue
                        }
                    )
                Case Else
                    Return ThrowError($"字典的键不支持{key.Type}类型")
            End Select




        Next

        Return New Fox_Dictionary With {.Pairs = pairs}
    End Function

    Public Function evalIndexExpression(Left As Fox_Object, Index As Fox_Object) As Fox_Object
        If Left.Type() = ObjectType.ARRAY_OBJ AndAlso Index.Type() = ObjectType.INTEGER_OBJ Then
            Return evalArrayIndexExpression(Left, Index)
        ElseIf Left.Type() = ObjectType.DICTIONARY_OBJ Then
            Return evalDictionaryIndexExpression(Left, Index)
        Else
            Return ThrowError($"索引操作不支持 {Left.Type()}")
        End If
    End Function

    Public Function evalDictionaryIndexExpression(Dict As Fox_Object, Index As Fox_Object) As Fox_Object
        Dim dictionaryObject = TryCast(Dict, Fox_Dictionary)
        Dim key_obj = TryCast(Fox_DictionaryHelper.CreateKey(Index), Fox_DictionaryKey)

        If dictionaryObject Is Nothing Then Return ThrowError($"不是一个字典: 类型 {Dict.Type} 内容 {Dict.Inspect}")
        If key_obj Is Nothing Then Return ThrowError($"不是一个键: {Index.Type} 内容 {Index.Inspect}")

        Dim pair = Nothing
        Dim key_in_dict = False

        For Each k As Fox_DictionaryKey In dictionaryObject.Pairs.Keys
            If k.ValueType = key_obj.ValueType AndAlso (k.Value.ToString.Replace("""", "") = key_obj.Inspect) Then
                pair = dictionaryObject.Pairs(k)
                Return pair.Value
            End If
        Next


        If Not key_in_dict Then Return ThrowError($"键 {key_obj.Inspect} 不存在于字典")

        Return pair
    End Function

    Public Function evalArrayIndexExpression(Array As Fox_Object, Index As Fox_Object) As Fox_Object
        Dim arrayObject = TryCast(Array, Fox_Array)
        Dim idx_obj = TryCast(Index, Fox_Integer)
        Dim max = CLng(arrayObject.Elements.Count - 1)

        If arrayObject Is Nothing Then Return ThrowError($"不是一个数组: {Array.Type} 内容 {Array.Inspect}")
        If idx_obj Is Nothing Then Return ThrowError($"不是一个整数: {Index.Type} 内容 {Index.Inspect}")

        If idx_obj.Value < 0 Then
            Return ThrowError($"索引的值小于0")
        ElseIf idx_obj.Value > max Then
            Return ThrowError($"索引的值大于数组索引最大值{max}")
        End If
        Return arrayObject.Elements(idx_obj.Value)
    End Function
    Public Function applyFunction(func As Fox_Object, args As List(Of Fox_Object)) As Fox_Object

        Select Case func.GetType
            Case GetType(Fox_Function)
                Dim f = TryCast(func, Fox_Function)
                Dim extendedEnv = extendFunctionEnv(f, args)
                Dim evaluated = eval(f.Body, extendedEnv)
                Return unwrapReturnValue(evaluated)
            Case GetType(Fox_Builtin)
                Dim builtin = TryCast(func, Fox_Builtin)
                Return builtin.BuiltinFunction(args)
            Case Else
                Return ThrowError($"不是一个函数: { func.Type}")
        End Select


    End Function

    Public Function extendFunctionEnv(
        func As Fox_Function,
        args As List(Of Fox_Object)
    ) As Environment
        Dim env = New Environment With {.outer = func.Env}

        If func.Parameters Is Nothing Then Return env
        For parmaIndex = 0 To func.Parameters.Count - 1
            'If args.Count = func.Parameters.Count Then
            env.SetValue(func.Parameters(parmaIndex).Value, args(parmaIndex))
            'ElseIf args.Count > func.Parameters.Count Then
            '    env.SetValue(func.Parameters(parmaIndex).Value, ThrowError($"实参数量超过形参"))
            'ElseIf args.Count < func.Parameters.Count Then
            '    env.SetValue(func.Parameters(parmaIndex).Value, ThrowError($"形参数量超过实参"))
            'End If
        Next

        Return env
    End Function
    Public Function unwrapReturnValue(obj As Fox_Object) As Fox_Object
        Dim returnValue = TryCast(obj, Fox_ReturnValue)
        If returnValue IsNot Nothing Then Return returnValue.Value
        Return returnValue
    End Function

    Public Function evalExpressions(
        exps As List(Of Expression),
        ByRef env As Environment
    ) As List(Of Fox_Object)
        Dim result As New List(Of Fox_Object)

        For Each e As Expression In exps
            Dim evaluated = eval(e, env)
            If isError(evaluated) Then Return New List(Of Fox_Object) From {evaluated}
            result.Add(evaluated)
        Next

        Return result
    End Function

    Public Function evalIdentifier(
        Node As Identifier,
        ByRef env As Environment
    ) As Fox_Object
        '尝试获取值
        Dim val = env.GetValue(Node.Value.Replace(vbCr, "").Replace(vbLf, ""))

        '如果标识符不存在
        If Not val.Item2 Then
            Dim builtin_func_val = Nothing
            Dim builtin_func = Builtins.builtinFuncs.TryGetValue(Node.Value.Replace(vbCr, "").Replace(vbLf, ""), builtin_func_val)
            If builtin_func Then
                Return builtin_func_val
            End If

            'Dim builtin_class_val = Nothing
            'Dim builtin_class = Builtins.builtinClasses.TryGetValue(Node.Value.Replace(vbCr, "").Replace(vbLf, ""), builtin_class_val)
            'If builtin_class Then
            '    Return builtin_class_val
            'End If

            '找不到标识符: [标识符]
            Return ThrowError("找不到标识符: " & Node.Value)
        End If

        Return val.Item1
    End Function

    '引发异常 返回一个Fox_Error对象
    Public Shared Function ThrowError(message As String) As Fox_Error
        '返回
        Return New Fox_Error With {.Message = message}
    End Function

    '解析代码块语句
    Public Function evalBlockStatement(block As BlockStatement, env As Environment) As Fox_Object
        Dim result = Nothing

        '循环遍历代码块的所有语句
        For Each s As Statement In block.Statements
            '求值
            result = eval(s, env)

            '判断
            If result IsNot Nothing Then
                Dim type = result.Type
                If type = ObjectType.RETURN_VALUE_OBJ OrElse type = ObjectType.ERROR_OBJ Then
                    Return result
                End If
            End If
        Next

        Return result
    End Function

    '获取if表达式的值 返回一个 Fox_Object 类型的对象
    Public Function evalIfExpression(if_exp As IfExpression, env As Environment) As Fox_Object
        '获取条件表达式
        Dim condition = eval(if_exp.Condition, env)

        '是否为错误对象
        If isError(condition) Then Return condition

        If isTruthy(condition) Then '条件为真
            '为真则返回 默认条件代码块
            Return eval(if_exp.Consequence, env)
        ElseIf if_exp.ElseIf_List IsNot Nothing Then ' 条件不为真 但有ElseIf分支
            '遍历分支列表
            For Each elseif_exp As ElseIfExpression In if_exp.ElseIf_List
                '求条件的值
                Dim cond = eval(elseif_exp.Condition, env)

                '条件为真
                If isTruthy(cond) Then
                    '返回分支代码块
                    Return eval(elseif_exp.Consequence, env)
                End If
            Next

            If if_exp.Alternative IsNot Nothing Then '条件不为真 但是 有Else分支代码块 
                '返回Else分支代码块
                Return eval(if_exp.Alternative, env)
            End If

        ElseIf if_exp.Alternative IsNot Nothing Then '条件不为真 但是 有Else分支代码块 
            '返回Else分支代码块
            Return eval(if_exp.Alternative, env)
        Else '都不是
            Return Fox_Nothing
        End If
    End Function

    Public Function isTruthy(obj As Fox_Object) As Boolean
        '判空
        If obj Is Nothing Then Return False

        Select Case obj.GetType
            Case GetType(Fox_Nothing)
                Return False
            Case GetType(Fox_Bool)
                Return TryCast(obj, Fox_Bool).Value
            Case GetType(Fox_Integer)
                Return TryCast(obj, Fox_Integer).Value > 0
            Case Else
                Return False
        End Select
    End Function

    Public Function evalInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object '操作符右边的对象
    ) As Fox_Object '返回一个对象

        '判空
        If Left Is Nothing Then
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Fox_Nothing.Type}")
            End If
            Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Right.Type}")
        Else
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Fox_Nothing.Type}")
            End If
        End If


        If Left.Type() = ObjectType.INTEGER_OBJ AndAlso Right.Type() = ObjectType.INTEGER_OBJ Then '如果左右两边类型都是 Integer
            Return evalIntegerInfixExpression(operator_, Left, Right) '调用函数解析 
        ElseIf Left.Type() = ObjectType.BOOL_OBJ AndAlso Right.Type() = ObjectType.BOOL_OBJ Then '如果左右两边类型都是 Bool
            Return evalBoolInfixExpression(operator_, Left, Right) '调用函数解析
        ElseIf Left.Type() = ObjectType.STRING_OBJ AndAlso Right.Type() = ObjectType.STRING_OBJ Then '如果左右两边类型都是 String
            Return evalStringInfixExpression(operator_, Left, Right)
        ElseIf Left.Type() = ObjectType.DOUBLE_OBJ AndAlso Right.Type() = ObjectType.DOUBLE_OBJ Then '如果左右两边类型都是 Double
            Return evalDoubleInfixExpression(operator_, Left, Right)
        ElseIf Left.Type() = ObjectType.ARRAY_OBJ AndAlso Right.Type() = ObjectType.ARRAY_OBJ Then '如果左右两边类型都是 Array
            Return evalArrayInfixExpression(operator_, Left, Right)
        ElseIf Left.Type() <> Right.Type() Then
            '信息大致内容: 类型不正确 : [左值] [运算符] [右值]
            Return ThrowError($"类型错误: {Left.Type()} {operator_} {Right.Type}")
        Else
            '信息大致内容: 未知的操作 : [左值] [运算符] [右值]
            Return ThrowError($"未知的操作: {Left.Type} {operator_} {Right.Type}")
        End If
    End Function
    Public Function evalArrayInfixExpression(
      operator_ As String,
      Left As Fox_Object,
      Right As Fox_Object
    ) As Fox_Object

        '判空
        If Left Is Nothing Then
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Fox_Nothing.Type}")
            End If
            Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Right.Type}")
        Else
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Fox_Nothing.Type}")
            End If
        End If

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

                    Dim obj = evalInfixExpression("==", left_obj, right_obj)
                    Dim bool_obj = TryCast(obj, Fox_Bool)

                    If isError(obj) Then Return Fox_False
                    results.Add(bool_obj.Value)
                Next

                Return New Fox_Bool With {.Value = Not results.Contains(False)}
            Case "!=", "<>" '不等于
            Case Else
                '信息大致内容: 未知的操作符 : [左值] [运算符] [右值]
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Right.Type}")
        End Select
    End Function


    Public Function evalStringInfixExpression(
      operator_ As String,
      Left As Fox_Object,
      Right As Fox_Object
    ) As Fox_Object

        '判空
        If Left Is Nothing Then
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Fox_Nothing.Type}")
            End If
            Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Right.Type}")
        Else
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Fox_Nothing.Type}")
            End If
        End If

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
    Public Function evalBoolInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object'操作符右边的对象
    ) As Fox_Object '返回一个 Fox_Object对象

        '判空
        If Left Is Nothing Then
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Fox_Nothing.Type}")
            End If
            Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Right.Type}")
        Else
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Fox_Nothing.Type}")
            End If
        End If

        '尝试转换对象为 Fox_Bool型并获取布尔值
        Dim leftVal = TryCast(Left, Fox_Bool).Value
        Dim rightVal = TryCast(Right, Fox_Bool).Value

        '判断操作符 并调用函数获取对应的Fox_Bool对象
        Select Case operator_
            Case "<" '小于
                Return nativeBoolToBooleanObject(BoolToLong(leftVal) < BoolToLong(rightVal))
            Case ">" '大于
                Return nativeBoolToBooleanObject(BoolToLong(leftVal) > BoolToLong(rightVal))
            Case "==" '等于
                Return nativeBoolToBooleanObject(leftVal = rightVal)
            Case "!=", "<>" '不等于
                Return nativeBoolToBooleanObject(leftVal <> rightVal)
            Case Else
                '信息大致内容: 未知的操作符 : [左值] [运算符] [右值]
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Right.Type}")
        End Select
    End Function

    '求整数值
    Public Function evalIntegerInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object ' 操作符右边的对象
    ) As Fox_Object

        '判空
        If Left Is Nothing Then
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Fox_Nothing.Type}")
            End If
            Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Right.Type}")
        Else
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Fox_Nothing.Type}")
            End If
        End If

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
                Return New Fox_Integer With {.Value = leftVal * rightVal}  '返回Fox_Integer对象
            Case "/"
                If rightVal <> 0 Then '除数为0
                    Return New Fox_Integer With {.Value = leftVal / rightVal}  '返回Fox_Integer对象
                Else
                    '报错: 除数不能为0
                    Return ThrowError("除数不能为0")
                End If
            Case "<" '小于
                Return nativeBoolToBooleanObject(leftVal < rightVal)  '返回Fox_Bool类型对象
            Case ">" '大于
                Return nativeBoolToBooleanObject(leftVal > rightVal)  '返回Fox_Bool类型对象
            Case "==" '等于
                Return nativeBoolToBooleanObject(leftVal = rightVal)  '返回Fox_Bool类型对象
            Case "!=", "<>" '不等于
                Return nativeBoolToBooleanObject(leftVal <> rightVal)  '返回Fox_Bool类型对象
            Case Else
                '未知的操作符 : [左值] [操作符] [右值]
                Return ThrowError($"未知的操作: {leftVal} {operator_} {rightVal}")
        End Select
    End Function

    '求小数值
    Public Function evalDoubleInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object ' 操作符右边的对象
    ) As Fox_Object

        '判空
        If Left Is Nothing Then
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Fox_Nothing.Type}")
            End If
            Return ThrowError($"未知的操作: {Fox_Nothing.Type} {operator_} {Right.Type}")
        Else
            If Right Is Nothing Then
                Return ThrowError($"未知的操作: {Left.Type} {operator_} {Fox_Nothing.Type}")
            End If
        End If

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
                If rightVal <> 0 Then '除数为0
                    Return New Fox_Double With {.Value = leftVal / rightVal}  '返回Fox_Double对象
                Else
                    '报错: 除数不能为0
                    Return ThrowError("除数不能为0")
                End If
            Case "<" '小于
                Return nativeBoolToBooleanObject(leftVal < rightVal)  '返回Fox_Double对象
            Case ">" '大于
                Return nativeBoolToBooleanObject(leftVal > rightVal)  '返回Fox_Double对象
            Case "==" '等于
                Return nativeBoolToBooleanObject(leftVal = rightVal)  '返回Fox_Double对象
            Case "!=", "<>" '不等于
                Return nativeBoolToBooleanObject(leftVal <> rightVal)  '返回Fox_Double对象
            Case Else
                '未知的操作符 : [左值] [操作符] [右值]
                Return ThrowError($"未知的操作: {leftVal} {operator_} {rightVal}")
        End Select
    End Function


    '求前缀表达式的值新建 Fox_Object
    Public Function evalPrefixExpression(operator_ As String, right As Fox_Object) As Fox_Object
        '判断操作符并调用函数获取对应对象 

        '判空
        If right Is Nothing Then
            Return ThrowError($"未知的操作: {Fox_Nothing.Inspect} {operator_} {Fox_Nothing.Inspect}")
        End If

        Select Case operator_
            Case "!" '感叹号 表示 Not
                Return EvalNotOperatorExpression(right) 'Bool类型
            Case "-"
                Return evalMinusPrefixOperatorExpression(right) 'Bool或Integer类型
            Case Else
                '信息大致内容: 未知的操作: [运算符] ,数值 [数值] 
                Return ThrowError($"未知的操作: {operator_}. {right}")
        End Select
    End Function

    Public Function evalMinusPrefixOperatorExpression(Right As Fox_Object) As Fox_Object
        If Right.Type() = ObjectType.INTEGER_OBJ Then '如果是Integer类型
            Dim value = TryCast(Right, Fox_Integer).Value '获取数值
            Return New Fox_Integer With {.Value = -value} '新建对象
        ElseIf Right.Type = ObjectType.BOOL_OBJ Then '如果是Bool类型
            Dim value = TryCast(Right, Fox_Bool).Value '获取Bool值

            '为什么boolean转long会变成负数啊...
            Return New Fox_Integer With {.Value = BoolToLong(0) + BoolToLong(value)}
        End If

        Return Fox_Nothing
    End Function

    Public Shared Function BoolToLong(bool As Boolean) As Long
        '翻译:转Long ( 转String ( 转Long ( 逻辑 ) .Replace("-","") ) ) 
        '这样做是因为CLng转boolean会变成负数
        Return CLng(CStr(CLng(bool)).Replace("-", ""))
    End Function

    '取对象的反向布尔值
    Public Function EvalNotOperatorExpression(right As Fox_Object) As Fox_Object
        '省流： 101%反骨
        Select Case right.Inspect.ToLower
            Case Fox_True.Inspect() '若为True
                Return Fox_False '返回False
            Case Fox_False.Inspect() '若为False
                Return Fox_True '返回True
            Case Fox_Nothing.Inspect() '若为Nothing
                Return Fox_True '返回True
            Case Else
                If right.Type = ObjectType.INTEGER_OBJ Then '如果为整数对象
                    If CLng(right.Inspect) = 0 Then '数值为0
                        Return Fox_True '返回 True
                    Else '否则
                        Return Fox_False '返回 False
                    End If
                End If
                Return Fox_False
        End Select
    End Function

    Public Function nativeBoolToBooleanObject(Input As Boolean) As Fox_Bool
        If Input Then
            Return Fox_True
        End If
        Return Fox_False
    End Function

    Public Function evalProgram(stmts As List(Of Statement), env As Environment) As Fox_Object
        Dim result As Fox_Object

        '遍历所有语句
        For Each s As Object In stmts
            If s.GetType <> GetType(EOLStatement) AndAlso s.Token.TokenType <> TokenType.无意义 AndAlso s.Token.TokenType <> TokenType.ILLEGAL Then
                result = eval(s, env)
            End If

            '转换result
            Select Case s.GetType
                Case GetType(Fox_ReturnValue)
                    '转为ReturnValue
                    If TryCast(result, Fox_ReturnValue) IsNot Nothing Then Return TryCast(result, Fox_ReturnValue).Value
                Case GetType(Fox_Error)
                    Return result
            End Select
        Next
        Return result
    End Function

End Class
