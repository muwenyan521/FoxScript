Imports System.Numerics
Imports System.Windows.Forms

Public Class Evaluator
    Public Shared Fox_True As Fox_Bool = New Fox_Bool() With {.Value = True}
    Public Shared Fox_False As Fox_Bool = New Fox_Bool() With {.Value = False}
    Public Shared Fox_Nothing As Fox_Nothing = New Fox_Nothing()
    Public Shared Fox_One As Fox_Integer = New Fox_Integer() With {.Value = 1}
    Public Shared Fox_Zero As Fox_Integer = New Fox_Integer() With {.Value = 0}

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

                '获取表达式语句的表达式 并求值
                Return eval(node.Expression, env)
            Case GetType(IntegerLiteral) '若为整数

                '返回一个 Fox_Integer类型 的对象
                Return New Fox_Integer With {.Value = node.Value}
            Case GetType(DoubleLiteral) '若为小数

                '返回一个 Fox_Double类型 的对象
                Return New Fox_Double With {.Value = node.Value}
            Case GetType(Bool) '若为布尔类型

                '调用函数获取对应的Fox_Bool对象
                Return nativeBoolToBooleanObject(node.Value)
            Case GetType(PrefixExpression) '前缀表达式
                '将右侧表达式求值
                Dim right = eval(node.Right, env)

                '是否为错误对象
                If isError(right) Then Return right

                '计算前缀表达式的值
                Return evalPrefixExpression(node.Operator_, right)
            Case GetType(InfixExpression) '中缀表达式

                '将左侧的表达式求值
                Dim Left = eval(node.Left, env)
                '将右侧的表达式求值
                Dim Right = eval(node.Right, env)

                '是否为错误对象
                If isError(Left) Then Return Left
                If isError(Right) Then Return Right

                '计算中缀表达式的值
                Return evalInfixExpression(node.Operator_, Left, Right)
            Case GetType(BlockStatement) '若为代码块

                ' 求语句所有的Fox_Object类型的对象
                Return evalBlockStatement(node, env)
            Case GetType(IfExpression) '若为 if表达式

                '计算If表达式的值
                Return evalIfExpression(node, env)
            Case GetType(ReturnStatement) '若为返回语句
                '将欲返回的表达式进行求值
                Dim val = eval(node.ReturnValue, env)

                '是否为错误对象
                If isError(val) Then Return val

                '返回一个值是 val 的 Fox_ReturnValue 对象
                Return New Fox_ReturnValue With {.Value = val}
            Case GetType(DimStatement) '若为 Dim 语句
                Dim dim_stmt = TryCast(node, DimStatement)

                '尝试获取标识符
                Dim get_Ident = env.GetValue(dim_stmt.Name.ToString)

                '如果存在
                If get_Ident.Item2 Then
                    Return ThrowError($"标识符 {dim_stmt.Name.ToString} 已存在! 内容为:{get_Ident.Item1.Inspect}")
                End If

                '对欲设置为值的表达式求值
                Dim value = eval(dim_stmt.Value, env)

                '检查是否为错误对象
                If isError(value) Then Return value

                '设置指定标识符的值
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

                '应用函数
                Return applyFunction(Func, args)

            Case GetType(Identifier) '若为标识符

                '求标识符的值
                Return evalIdentifier(DirectCast(node, Identifier), env)
            Case GetType(StringLiteral)
                '返回一个 Fox_String 的对象
                Return New Fox_String With {.Value = node.Value}
            Case GetType(ArrayLiteral)
                '对所有的表达式进行求值
                Dim elements = evalExpressions(node.Elements, env)

                If elements.Count = 1 AndAlso isError(elements(0)) Then
                    Return elements(0)
                End If

                '返回一个 Fox_Array 对象
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
                '对字典表达式 求值
                Return evalHashLiteral(node, env)
            Case GetType(ForStatement)
                '将当前节点转换成 ForStatement
                Dim for_stmt = TryCast(node, ForStatement)

                '对For语句欲遍历的表达式进行求值
                Dim items = eval(for_stmt.Items, env)

                '检查是否为错误对象
                If isError(items) Then Return items

                '检查欲遍历的对象是否为列表
                If items.Type <> ObjectType.ARRAY_OBJ Then Return ThrowError($"for循环不支持遍历 {items.Type} 的值")

                '开始遍历
                For Each __item__ As Fox_Object In TryCast(items, Fox_Array).Elements
                    '注册迭代变量
                    env.SetValue(for_stmt.ItemVar.ToString, __item__)

                    '对For语句中欲重复执行的代码进行求值
                    Dim r = eval(for_stmt.LoopBlock, env)
                    If r Is Nothing Then Continue For
                    r.Inspect()
                Next
            Case GetType(WhileStatement)
                '将当前节点转换成 WhileStatement
                Dim while_stmt = TryCast(node, WhileStatement)

                '计算While语句的循环条件
                Dim cond_obj = eval(while_stmt.LoopCondition, env)

                '检查是否为错误对象
                If isError(cond_obj) Then Return cond_obj

                '检查循环条件的对象类型是否为Bool类型
                If cond_obj.Type <> ObjectType.BOOL_OBJ Then Return ThrowError($"while循环不支持条件为 {cond_obj.Type} 的值")

                'While循环
                While TryCast(cond_obj, Fox_Bool).Value
                    '再次计算条件表达式的值
                    cond_obj = eval(while_stmt.LoopCondition, env)

                    '计算While语句欲循环的代码
                    Dim r = eval(while_stmt.LoopBlock, env)
                    If r Is Nothing Then Continue While
                    If isError(r) Then Return r

                    r.Inspect()
                End While
            Case GetType(AssignmentExpression)
                Dim assignmentExp = TryCast(node, AssignmentExpression)
                Dim value = evalAssignmentExpression(assignmentExp.SetExp, node.Value, env)

                Return value

            Case GetType(NotExpression)
                Dim notExp = TryCast(node, NotExpression)

                '计算右侧欲取反的表达式
                Dim rightObject = eval(notExp.Right, env)
                If isError(rightObject) Then Return rightObject

                Return nativeBoolToBooleanObject(Not isTruthy(rightObject))
            Case GetType(AndExpression)
                Dim andExp = TryCast(node, AndExpression)

                '计算左侧欲进行与运算的表达式
                Dim leftObject = eval(andExp.Left, env)
                If isError(leftObject) Then Return leftObject

                '将对象转换成布尔值
                Dim leftBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({leftObject}), Fox_Object)

                '如果不为真
                If Not isTruthy(leftBoolObject) Then
                    Return Fox_False '返回假
                End If

                '计算右侧欲进行与运算的表达式
                Dim rightObject = eval(andExp.Right, env)
                If isError(rightObject) Then Return rightObject

                '将对象转换成布尔值
                Dim rightBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({rightObject}), Fox_Object)

                '如果不为真
                If Not isTruthy(rightBoolObject) Then
                    Return Fox_False '返回假
                End If

                '如果两个布尔值都为真 返回真
                Return Fox_True

            Case GetType(OrExpression)
                Dim orExp = TryCast(node, OrExpression)

                '计算左侧欲进行或运算的表达式
                Dim leftObject = eval(orExp.Left, env)
                If isError(leftObject) Then Return leftObject

                '将对象转换成布尔值
                Dim leftBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({leftObject}), Fox_Object)

                '如果为真
                If isTruthy(leftBoolObject) Then
                    Return Fox_True '短路 返回真
                End If

                '计算右侧欲进行或运算的表达式
                Dim rightObject = eval(orExp.Right, env)
                If isError(rightObject) Then Return rightObject

                '将对象转换成布尔值
                Dim rightBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({rightObject}), Fox_Object)

                '如果为真
                If isTruthy(rightBoolObject) Then
                    Return Fox_True '返回真
                End If

                '如果两个布尔值都为假 返回假
                Return Fox_False
            Case GetType(ClassStatement)
                Dim classStmt = TryCast(node, ClassStatement)

                '获取类名
                Dim className = classStmt.Name
                env.SetValue(className.Value, Nothing)

                '获取类代码块
                Dim body = classStmt.Body

                Dim clsObj = New Fox_Class With {.Env = New Environment With {.outer = env}, .Body = body, .Name = className}

                Dim Stmts = FindAllStatement(body.Statements, GetType(ExpressionStatement))
                Dim expStmts As New List(Of ExpressionStatement)

                For Each stmt As ExpressionStatement In Stmts
                    expStmts.Add(stmt)
                Next


                Dim funcLiterals = FindAllExpression(expStmts, GetType(FunctionLiteral))
                For Each stmt As FunctionLiteral In funcLiterals
                    Dim funcLiteral = TryCast(stmt, FunctionLiteral)
                    If funcLiteral.Name.Value.Replace(" ", "") = "New" Then
                        clsObj.CreateFunc = funcLiteral
                    End If
                Next

                eval(clsObj.Body, clsObj.Env)
                clsObj.Env.SetValue("Me", clsObj)

                env.SetValue(className.Value, clsObj)

                Dim classObject = env.GetValue(className.Value).Item1

                '返回
                Return classObject

            Case GetType(ObjectMemberExpression)
                Dim objMemberExp = TryCast(node, ObjectMemberExpression)
                Dim leftObj = eval(objMemberExp.Left, env)
                If isError(leftObj) Then Return leftObj

                Return evalObjectMemberExpression(leftObj, objMemberExp.Right, env)
            Case GetType(ObjectCreateExpression)
                Dim objCreateExp = TryCast(node, ObjectCreateExpression)

                Dim r = eval(objCreateExp.ObjType, env)
                If isError(r) Then Return r

                Dim classObj As Fox_Class = r
                Dim newObj = New Fox_Class With {.Body = classObj.Body, .Env = New Environment, .Name = classObj.Name, .CreateFunc = classObj.CreateFunc, .CreateArgs = objCreateExp.Arguments}

                eval(newObj.Body, newObj.Env)
                newObj.Env.SetValue("Me", newObj)

                If newObj.CreateFunc IsNot Nothing Then
                    eval(New CallExpression With
                         {
                            .Arguments = newObj.CreateArgs,
                            .Func = newObj.CreateFunc.Name
                         },
                         newObj.Env
                    )
                End If

                Return newObj
            Case GetType(FileImportExpression)
                Dim fileImportExp = TryCast(node, FileImportExpression)
                Dim className = fileImportExp.AliasName.ToString.Replace("(", "").Replace(")", "")

                Dim classObj = New Fox_Class With {.Body = New BlockStatement, .Env = New Environment, .Name = New Identifier With {.Value = className}}
                Dim filePath As Object = eval(fileImportExp.FilePath, env)
                If isError(filePath) Then Return filePath

                Dim runner As New Runner With {.Mode = "FILE", .FilePath = filePath.Value}
                classObj.Body.Statements = runner.Run(classObj.Env)

                env.SetValue(className, classObj)
        End Select

        Return Nothing
    End Function

    Public Function evalAssignmentExpression(SetExp As Expression, valueExp As Expression, ByRef env As Environment)
        Select Case SetExp.GetType
            Case GetType(Identifier)
                '转换为表达式为Identifier
                Dim ident = TryCast(SetExp, Identifier)

                '尝试寻找标识符
                Dim get_Ident = env.GetValue(ident.ToString)

                '标识符不存在
                If Not get_Ident.Item2 Then
                    '报错
                    Return ThrowError($"标识符 {ident.ToString} 不存在! ")
                End If

                '将环境中标识符的值修改
                env.SetValue(ident.Value, eval(valueExp, env))

            Case GetType(ObjectMemberExpression)
                Dim ObjectMemberExp = TryCast(SetExp, ObjectMemberExpression)
                Dim ClassObject As Object = eval(ObjectMemberExp.Left, env)
                If isError(ClassObject) Then Return ClassObject

                Dim name = ObjectMemberExp.Right.ToString.Replace(" ", "")
                Dim value = eval(valueExp, env)
                ClassObject.Env.SetValue(name, value)
                ClassObject.Env.SetValue("Me", ClassObject)

        End Select

        Return Nothing
    End Function

    Public Sub RemoveAllStatement(ByRef OriginStatements As List(Of Statement), Type As Type)
        ' 创建一个要删除的元素的列表
        Dim Removes As New List(Of Statement)

        ' 遍历原始列表，将符合条件的元素添加到Removes列表中
        For Each stmt As Statement In OriginStatements
            If stmt.GetType() Is Type Then
                Removes.Add(stmt)
            End If
        Next

        ' 遍历Removes列表，并从原始列表中删除这些元素
        For Each stmt As Statement In Removes
            OriginStatements.Remove(stmt)
        Next
    End Sub

    Public Function FindAllStatement(OriginStatements As List(Of Statement), Type As Type) As List(Of Statement)
        Dim Statements As New List(Of Statement)
        For Each stmt As Statement In OriginStatements
            If stmt.GetType = Type Then
                Statements.Add(stmt)
            End If
        Next

        Return Statements
    End Function

    Public Function FindAllExpression(OriginExpressions As List(Of ExpressionStatement), Type As Type)
        Dim Expressions As New List(Of Expression)
        For Each exp As ExpressionStatement In OriginExpressions
            If exp.Expression Is Nothing Then Continue For

            If exp.Expression.GetType = Type Then
                Expressions.Add(exp.Expression)
            End If
        Next

        Return Expressions
    End Function


    Public Function evalObjectMemberExpression(leftObject As Fox_Object, rightExp As Expression, ByRef env As Environment)
        Select Case leftObject.Type
            Case ObjectType.CLASS_OBJ
                Dim classObj = TryCast(leftObject, Fox_Class)

                Dim result = eval(rightExp, classObj.Env)

                Return result
        End Select

        Return Nothing
    End Function

    Public Function ContainsStatementOfType(Statements As List(Of Statement), Type As Type)
        For Each Statement As Statement In Statements
            If Statement.GetType = Type Then
                Return True
            End If
        Next

        Return False
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
            '当左侧的对象是一个列表，且索引是一个整数，那么计算索引表达式的值
            Return evalArrayIndexExpression(Left, Index)
        ElseIf Left.Type() = ObjectType.DICTIONARY_OBJ Then
            '当左侧的对象是一个字典，那么计算索引表达式的值
            Return evalDictionaryIndexExpression(Left, Index)
        ElseIf Left.Type() = ObjectType.STRING_OBJ AndAlso Index.Type() = ObjectType.INTEGER_OBJ Then
            '当左侧的对象是一个字符串，且索引是一个整数，那么计算索引表达式的值
            Return evalStringIndexExpression(Left, Index)
        Else
            Return ThrowError($"索引操作不支持 {Left.Type()}")
        End If
    End Function

    Public Function evalStringIndexExpression(Str As Fox_Object, Index As Fox_Object) As Fox_Object
        '转换对象
        Dim stringObject = TryCast(Str, Fox_String)
        Dim indexObject = TryCast(Index, Fox_Integer)

        '获取对象的值
        Dim strValue = stringObject.Value
        Dim indexValue = indexObject.Value

        '比较索引值是否超过最大索引值
        If indexValue >= strValue.Count Then
            Return ThrowError($"索引 {indexValue.ToString} 超出字符串范围")
        End If

        '返回一个Fox_String对象 值为字符串对应索引的字符
        Return New Fox_String With {.Value = strValue(indexValue)}
    End Function


    Public Function evalDictionaryIndexExpression(Dict As Fox_Object, Index As Fox_Object) As Fox_Object
        '尝试转换
        Dim dictionaryObject = TryCast(Dict, Fox_Dictionary)

        '字典键对象
        Dim key_obj As Fox_DictionaryKey = Fox_DictionaryHelper.CreateKey(Index)

        '如果转换失败，报错
        If dictionaryObject Is Nothing Then Return ThrowError($"不是一个字典: 类型 {Dict.Type} 内容 {Dict.Inspect}")

        Dim pair As Fox_DictionaryPair = Nothing

        '遍历字典的键列表
        For Each k As Fox_DictionaryKey In dictionaryObject.Pairs.Keys
            '如果键的类型 等于 当前键的类型 且 键的值 等于 当前键的值
            If k.ValueType = key_obj.ValueType AndAlso (k.Value.ToString.Replace("""", "") = key_obj.Inspect) Then
                pair = dictionaryObject.Pairs(k)
                Return pair.Value
            End If
        Next

        Return ThrowError($"键 {key_obj.Inspect} 不存在于字典")

        Return pair
    End Function

    Public Function evalArrayIndexExpression(Array As Fox_Object, Index As Fox_Object) As Fox_Object
        '尝试转换
        Dim arrayObject = TryCast(Array, Fox_Array)
        Dim idx_obj = TryCast(Index, Fox_Integer)

        '获取数组最大索引值
        Dim max = CLng(arrayObject.Elements.Count - 1)

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
                '如果是Fox_Function 将对象转为 Fox_Function 对象
                Dim f = TryCast(func, Fox_Function)

                '扩展函数环境
                Dim extendedEnv = extendFunctionEnv(f, args)

                '对欲运行的代码求值
                Dim evaluated = eval(f.Body, extendedEnv)

                '解包返回函数
                Return unwrapReturnValue(evaluated)
            Case GetType(Fox_Builtin)
                '若为Fox_Builtin，将对象转换为Fox_Builtin对象
                Dim builtin = TryCast(func, Fox_Builtin)

                '调用原生函数并返回它的值
                Return builtin.BuiltinFunction(args)
            Case Else
                Return ThrowError($"不是一个函数: { func.Type}")
        End Select


    End Function

    Public Function extendFunctionEnv(
        ByRef func As Fox_Function,
        args As List(Of Fox_Object)
    ) As Environment
        Dim env = New Environment With {.outer = func.Env}

        Dim class_functioncall = env.outer.GetValue("Me")

        If func.Parameters Is Nothing Then Return env

        If args.Count > func.Parameters.Count Then
            env.SetValue(func.Parameters(0).Value, ThrowError($"实参数量超过形参"))
            Return env
        ElseIf args.Count < func.Parameters.Count Then
            env.SetValue(func.Parameters(0).Value, ThrowError($"形参数量超过实参"))
            Return env
        End If

        For parmaIndex = 0 To func.Parameters.Count - 1
            env.SetValue(func.Parameters(parmaIndex).Value, args(parmaIndex))
        Next

        If class_functioncall.Item2 Then
        End If

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
        '创建新列表
        Dim result As New List(Of Fox_Object)

        '遍历表达式
        For Each e As Expression In exps
            '对表达式求值
            Dim evaluated = eval(e, env)

            '若为错误对象则返回
            If isError(evaluated) Then Return New List(Of Fox_Object) From {evaluated}

            '添加对象至列表中
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
            '尝试获取内置函数
            Dim builtin_func_val = Nothing
            Dim builtin_func = Builtins.builtinFuncs.TryGetValue(Node.Value.Replace(vbCr, "").Replace(vbLf, ""), builtin_func_val)
            If builtin_func Then
                '若查找到了函数，则返回这个函数
                Return builtin_func_val
            End If

            '尝试获取内置标识符
            Dim builtin_var_val As Fox_Builtin = Nothing
            Dim builtin_var = Builtins.builtinVars.TryGetValue(Node.Value.Replace(vbCr, "").Replace(vbLf, "").Replace(" ", ""), builtin_var_val)
            If builtin_var Then
                '找到标识符后返回
                Return builtin_var_val.BuiltinIdentifier
            End If

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

        Return Fox_Nothing
    End Function

    Public Function isTruthy(obj As Fox_Object) As Boolean
        '判空
        If obj Is Nothing Then Return False

        Select Case obj.GetType
            Case GetType(Fox_Nothing)
                'Nothing 空值 返回False
                Return False
            Case GetType(Fox_Bool)
                '获取对象的值
                Return TryCast(obj, Fox_Bool).Value
            Case GetType(Fox_Integer), GetType(Fox_Double)
                '当整数/小数值不为零 返回真 否则返回假
                Return TryCast(obj, Object).Value <> 0
            Case Else
                '未知的类型 返回假
                Return False
        End Select
    End Function

    Public Function CheckObject(operator_ As String, Left As Fox_Object, Right As Fox_Object)
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

        If isError(Left) Then Return Left
        If isError(Right) Then Return Right

        Return Nothing
    End Function

    Public Function evalInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object '操作符右边的对象
    ) As Fox_Object '返回一个对象

        '判空
        Dim result = CheckObject(operator_, Left, Right)
        If result IsNot Nothing Then Return result

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
            Return AddressOf evalDoubleInfixExpression
        End If

        Select Case leftType
            Case ObjectType.INTEGER_OBJ
                Return AddressOf evalIntegerInfixExpression
            Case ObjectType.BOOL_OBJ
                Return AddressOf evalBoolInfixExpression
            Case ObjectType.STRING_OBJ
                Return AddressOf evalStringInfixExpression
            Case ObjectType.DOUBLE_OBJ
                Return AddressOf evalDoubleInfixExpression
            Case ObjectType.ARRAY_OBJ
                Return AddressOf evalArrayInfixExpression
            Case ObjectType.NOTHINGL_OBJ
                Return AddressOf evalNothingInfixExpression
            Case Else
                Return Nothing
        End Select
    End Function

    Private Function GetType_ConvertHandlers(leftType As ObjectType, rightType As ObjectType) As Func(Of IEnumerable(Of Object), Object)
        If leftType = ObjectType.NOTHINGL_OBJ AndAlso rightType = ObjectType.NOTHINGL_OBJ Then Return Builtins.builtinFuncs("CNothing").BuiltinFunction
        If leftType = ObjectType.DOUBLE_OBJ OrElse rightType = ObjectType.DOUBLE_OBJ Then Return Builtins.builtinFuncs("CDbl").BuiltinFunction

        Select Case leftType
            Case ObjectType.INTEGER_OBJ
                Return Builtins.builtinFuncs("CInt").BuiltinFunction
            Case ObjectType.BOOL_OBJ
                Return Builtins.builtinFuncs("CBool").BuiltinFunction
            Case ObjectType.STRING_OBJ
                Return Builtins.builtinFuncs("CStr").BuiltinFunction
            Case ObjectType.ARRAY_OBJ
                Return Builtins.builtinFuncs("CArray").BuiltinFunction
            Case ObjectType.NOTHINGL_OBJ
                Return GetType_ConvertHandlers(rightType, rightType)
            Case Else
                Return Nothing
        End Select
    End Function
    Public Function evalArrayInfixExpression(
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

                    Dim obj = evalInfixExpression("==", left_obj, right_obj)
                    Dim bool_obj = TryCast(obj, Fox_Bool)

                    If isError(obj) Then Return Fox_False
                    results.Add(bool_obj.Value)
                Next

                Return New Fox_Bool With {.Value = Not results.Contains(False)}
            Case "!=", "<>" '不等于
                If leftVal.Count <> rightVal.Count Then Return New Fox_Bool With {.Value = True}
                Dim results = New List(Of Boolean)

                For i = 0 To leftVal.Count - 1
                    Dim left_obj = leftVal(i)
                    Dim right_obj = rightVal(i)

                    Dim obj = evalInfixExpression("<>", left_obj, right_obj)
                    Dim bool_obj = TryCast(obj, Fox_Bool)

                    If isError(obj) Then Return Fox_False
                    results.Add(bool_obj.Value)
                Next

                Return New Fox_Bool With {.Value = results.Contains(True)}
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
    Public Function evalBoolInfixExpression(
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

    Public Function evalNothingInfixExpression(
        operator_ As String, '操作符
        Left As Fox_Object, '操作符左边的对象
        Right As Fox_Object ' 操作符右边的对象
    ) As Fox_Object

        Dim leftVal = If(TypeOf Left Is Fox_Nothing, Nothing, CType(Left, Object).Value)
        Dim rightVal = If(TypeOf Right Is Fox_Nothing, Nothing, CType(Right, Object).Value)


        '判断操作符并新建对应对象
        Select Case operator_
            Case "==" '等于
                Return nativeBoolToBooleanObject(leftVal Is rightVal)  '返回Fox_Bool类型对象
            Case "!=", "<>" '不等于
                Return nativeBoolToBooleanObject(leftVal IsNot rightVal)  '返回Fox_Bool类型对象
            Case Else
                '未知的操作符 : [左值] [操作符] [右值]
                Return ThrowError($"未知的操作: {leftVal} {operator_} {rightVal}")
        End Select
    End Function


    '求整数值
    Public Function evalIntegerInfixExpression(
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
        Select Case Right.Type
            Case ObjectType.INTEGER_OBJ
                Dim value As BigInteger = TryCast(Right, Fox_Integer).Value '获取数值
                Return New Fox_Integer With {.Value = -value} '新建对象并返回
            Case ObjectType.DOUBLE_OBJ
                Dim value As Decimal = TryCast(Right, Fox_Double).Value '获取数值
                Return New Fox_Double With {.Value = -value} '新建对象并返回
            Case ObjectType.BOOL_OBJ
                Dim value = TryCast(Right, Fox_Bool).Value '获取Bool值

                '为什么boolean转long会变成负数啊...
                Return New Fox_Integer With {.Value = BoolToLong(0) + BoolToLong(value)}
        End Select

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
        Dim result As Fox_Object = Nothing

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
