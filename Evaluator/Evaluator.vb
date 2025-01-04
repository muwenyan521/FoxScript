Imports System.IO
Imports System.Numerics
Imports System.Reflection

Imports FoxScript.Utils
Imports FoxScript.BooleanUtils
Imports FoxScript.FileSystemUtils
Imports FoxScript.StringUtils
Imports FoxScript.ErrorUtils
Imports FoxScript.StatementUtils
Imports FoxScript.ObjectUtils
Imports System.Windows.Forms.AxHost

Public Interface IEvaluatorHandler
    Function Eval(ParamArray Args() As Object) As Fox_Object
End Interface

Public Class Evaluator
    Public Fox_ConstantPool As New Dictionary(Of Object, Fox_Object)
    Public EvaluatorHandlers As New Dictionary(Of Type, IEvaluatorHandler)


    Public Sub New()
        For i = 0 To 256
            Fox_ConstantPool.Add(i, New Fox_Integer With {.Value = i})
        Next

        EvaluatorHandlers = New Dictionary(Of Type, IEvaluatorHandler) From {
            {GetType(InfixExpression), New InfixExpressionEvaluator(Me)},
            {GetType(SliceExpression), New SliceExpressionEvaluator(Me)},
            {GetType(AssignmentExpression), New AssignmentExpressionEvaluator(Me)}
        }
    End Sub

    '判断是否为Error对象. 返回一个布尔值
    Public Shared Function IsError(obj As Fox_Object)
        If obj Is Nothing Then Return False
        Return obj.Type = ObjectType.ERROR_OBJ
    End Function

    '求值函数 返回 Fox_Object 类型的对象
    Public Function Eval(node As Object, ByRef env As Environment) As Fox_Object
        If node Is Nothing Then Return Nothing '判空

        Select Case node.GetType() '获取类型
            Case GetType(Program) '如果是Program

                '求所有语句的Fox_Object对象
                Return EvalProgram(node.Statements, env)
            Case GetType(ExpressionStatement) '若为表达式

                '获取表达式语句的表达式 并求值
                Return Eval(node.Expression, env)
            Case GetType(IntegerLiteral) '若为整数

                '返回一个 Fox_Integer类型 的对象
                Dim num = CLng(node.Value.ToString)
                If ContainsKey(Fox_ConstantPool, num) Then Return GetKeyValue(Fox_ConstantPool, num)
                Return New Fox_Integer With {.Value = node.Value}
            Case GetType(DoubleLiteral) '若为小数

                '返回一个 Fox_Double类型 的对象
                Return New Fox_Double With {.Value = node.Value}
            Case GetType(Bool) '若为布尔类型
                '调用函数获取对应的Fox_Bool对象
                Return NativeBoolToBooleanObject(node.Value)
            Case GetType(PrefixExpression) '前缀表达式
                '将右侧表达式求值
                Dim right = Eval(node.Right, env)

                '是否为错误对象
                If IsError(right) Then Return right

                '计算前缀表达式的值
                Return EvalPrefixExpression(node.Operator_, right)
            Case GetType(InfixExpression) '中缀表达式

                '将左侧的表达式求值
                Dim Left = Eval(node.Left, env)
                '将右侧的表达式求值
                Dim Right = Eval(node.Right, env)

                '是否为错误对象
                If IsError(Left) Then Return Left
                If IsError(Right) Then Return Right

                '计算中缀表达式的值
                Return EvaluatorHandlers(GetType(InfixExpression)).Eval(node.Operator_, Left, Right)
            Case GetType(BlockStatement) '若为代码块

                ' 求语句所有的Fox_Object类型的对象
                Return EvalBlockStatement(node, env)
            Case GetType(IfExpression) '若为 if表达式

                '计算If表达式的值
                Return EvalIfExpression(node, env)
            Case GetType(ReturnStatement) '若为返回语句
                '将欲返回的表达式进行求值
                Dim val = Eval(node.ReturnValue, env)

                '是否为错误对象
                If IsError(val) Then Return val

                '返回一个值是 val 的 Fox_ReturnValue 对象
                Return New Fox_ReturnValue With {.Value = val}
            Case GetType(DimStatement) '若为 Dim 语句
                Dim dim_stmt = TryCast(node, DimStatement)

                '对欲设置为值的表达式求值
                Dim value = Eval(dim_stmt.Value, env)

                '检查是否为错误对象
                If IsError(value) Then Return value

                '设置指定标识符的值
                env.SetValue(dim_stmt.Name.Value, value, dim_stmt.IsReadOnly)
            Case GetType(FunctionLiteral)
                '获取函数名
                Dim funcName = TryCast(node, FunctionLiteral).Name
                env.SetValue(funcName.Value, Nothing, False)

                '获取形参
                Dim params = TryCast(node, FunctionLiteral).Parameters

                '获取函数代码块
                Dim body = node.Body
                env.SetValue(funcName.Value, New Fox_Function With {.Parameters = params, .Env = env, .Body = body, .Name = funcName}, False)
                '返回
                Return env.GetValue(funcName.Value).Item1
            Case GetType(CallExpression) '若为调用表达式

                '计算表达式的值
                Dim Func = Eval(node.Func, env)

                '判断是否为错误对象
                If IsError(Func) Then Return Func

                '计算所有表达式实参
                Dim args = EvalExpressions(node.Arguments, env)

                '如果参数数量为1 并且 这一个参数为错误对象 返回这一个参数
                If args.Count = 1 AndAlso IsError(args(0)) Then Return args(0)

                '应用函数
                Return ApplyFunction(Func, args)

            Case GetType(Identifier) '若为标识符

                '求标识符的值
                Return EvalIdentifier(DirectCast(node, Identifier), env)
            Case GetType(StringLiteral)
                '返回一个 Fox_String 的对象
                Return New Fox_String With {.Value = node.Value}
            Case GetType(ArrayLiteral)
                '对所有的表达式进行求值
                Dim elements = EvalExpressions(node.Elements, env)

                If elements.Count = 1 AndAlso IsError(elements(0)) Then
                    Return elements(0)
                End If

                '返回一个 Fox_Array 对象
                Return New Fox_Array With {.Elements = elements}
            Case GetType(IndexExpression)
                Dim Left = Eval(node.Left, env)
                If IsError(Left) Then
                    Return Left
                End If
                Dim Index = Eval(node.Index, env)
                If IsError(Index) Then
                    Return Index
                End If
                Return EvalIndexExpression(Left, Index)
            Case GetType(DictionaryLiteral)
                '对字典表达式 求值
                Return EvalHashLiteral(node, env)
            Case GetType(ForStatement)
                '将当前节点转换成 ForStatement
                Dim for_stmt = TryCast(node, ForStatement)

                '对For语句欲遍历的表达式进行求值
                Dim items = Eval(for_stmt.Items, env)

                '检查是否为错误对象
                If IsError(items) Then Return items

                '检查欲遍历的对象是否为列表
                If items.Type <> ObjectType.ARRAY_OBJ Then Return ThrowError($"for循环不支持遍历 {items.Type} 的值")

                '开始遍历
                For Each __item__ As Fox_Object In TryCast(items, Fox_Array).Elements
                    '注册迭代变量
                    env.SetValue(for_stmt.ItemVar.ToString, __item__, False)

                    '对For语句中欲重复执行的代码进行求值
                    Dim r = Eval(for_stmt.LoopBlock, env)
                    If r Is Nothing Then Continue For
                    r.Inspect()
                Next
            Case GetType(WhileStatement)
                '将当前节点转换成 WhileStatement
                Dim while_stmt = TryCast(node, WhileStatement)

                '计算While语句的循环条件
                Dim cond_obj = Eval(while_stmt.LoopCondition, env)

                '检查是否为错误对象
                If IsError(cond_obj) Then Return cond_obj

                '检查循环条件的对象类型是否为Bool类型
                If cond_obj.Type <> ObjectType.BOOL_OBJ Then Return ThrowError($"while循环不支持条件为 {cond_obj.Type} 的值")

                'While循环
                While TryCast(cond_obj, Fox_Bool).Value
                    '再次计算条件表达式的值
                    cond_obj = Eval(while_stmt.LoopCondition, env)
                    If Not TryCast(cond_obj, Fox_Bool).Value Then
                        Continue While
                    End If

                    '计算While语句欲循环的代码
                    Dim r = Eval(while_stmt.LoopBlock, env)
                    If r Is Nothing Then Continue While

                    If IsError(r) Then Return r

                    r.Inspect()
                End While
            Case GetType(AssignmentExpression)
                Dim assignmentExp = TryCast(node, AssignmentExpression)
                Dim value = EvaluatorHandlers(assignmentExp.GetType).Eval(assignmentExp.SetExp, node.Value, env)
                Return value
            Case GetType(NotExpression)
                Dim notExp = TryCast(node, NotExpression)

                '计算右侧欲取反的表达式
                Dim rightObject = Eval(notExp.Right, env)
                If IsError(rightObject) Then Return rightObject

                Return NativeBoolToBooleanObject(Not IsTruthy(rightObject))
            Case GetType(AndExpression)
                Dim andExp = TryCast(node, AndExpression)

                '计算左侧欲进行与运算的表达式
                Dim leftObject = Eval(andExp.Left, env)
                If IsError(leftObject) Then Return leftObject

                '将对象转换成布尔值
                Dim leftBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({leftObject}), Fox_Object)

                '如果不为真
                If Not IsTruthy(leftBoolObject) Then
                    Return Fox_False '返回假
                End If

                '计算右侧欲进行与运算的表达式
                Dim rightObject = Eval(andExp.Right, env)
                If IsError(rightObject) Then Return rightObject

                '将对象转换成布尔值
                Dim rightBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({rightObject}), Fox_Object)

                '如果不为真
                If Not IsTruthy(rightBoolObject) Then
                    Return Fox_False '返回假
                End If

                '如果两个布尔值都为真 返回真
                Return Fox_True

            Case GetType(OrExpression)
                Dim orExp = TryCast(node, OrExpression)

                '计算左侧欲进行或运算的表达式
                Dim leftObject = Eval(orExp.Left, env)
                If IsError(leftObject) Then Return leftObject

                '将对象转换成布尔值
                Dim leftBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({leftObject}), Fox_Object)

                '如果为真
                If IsTruthy(leftBoolObject) Then
                    Return Fox_True '短路 返回真
                End If

                '计算右侧欲进行或运算的表达式
                Dim rightObject = Eval(orExp.Right, env)
                If IsError(rightObject) Then Return rightObject

                '将对象转换成布尔值
                Dim rightBoolObject As Fox_Object = TryCast(Builtins.builtinFuncs("CBool").BuiltinFunction({rightObject}), Fox_Object)

                '如果为真
                If IsTruthy(rightBoolObject) Then
                    Return Fox_True '返回真
                End If

                '如果两个布尔值都为假 返回假
                Return Fox_False
            Case GetType(ClassStatement)
                Dim classStmt = TryCast(node, ClassStatement)

                '获取类名
                Dim className As Object = classStmt.Name
                env.SetValue(className.Value, Nothing, False)

                '获取类代码块
                Dim body = classStmt.Body

                Dim clsObj = New Fox_Class With {
                    .Env = New ClassEnvironment With {.outer = env},
                    .Body = body, .Name = className,
                    .CreateFunc = FindFunctionLiteral("New", body.Statements),
                    .BaseClass = Eval(classStmt.BaseClass, env)
                }

                ExtendClassEnv(clsObj)
                Eval(clsObj.Body, clsObj.Env)

                clsObj.Env.SetValue("Me", clsObj, False)
                clsObj.Env.SetValue("MyBase", clsObj.BaseClass, False)
                env.SetValue(className.Value, clsObj, False)

                Dim classObject = env.GetValue(className.Value).Item1
                Return classObject

            Case GetType(ObjectMemberExpression)
                Dim objMemberExp = TryCast(node, ObjectMemberExpression)
                Dim leftObj = Eval(objMemberExp.Left, env)
                If IsError(leftObj) Then Return leftObj

                Return EvalObjectMemberExpression(leftObj, objMemberExp.Right, env)
            Case GetType(ObjectCreateExpression)
                Dim objCreateExp = TryCast(node, ObjectCreateExpression)

                Dim r = Eval(objCreateExp.ObjType, env)
                If IsError(r) Then Return r

                Dim newObj = Nothing
                If r.Type = ObjectType.CLASS_OBJ Then
                    Dim classObj As Fox_Class = r
                    newObj = New Fox_Class With {.Body = classObj.Body, .Env = New ClassEnvironment, .Name = classObj.Name, .CreateFunc = classObj.CreateFunc, .CreateArgs = objCreateExp.Arguments, .BaseClass = classObj.BaseClass}

                    ExtendClassEnv(newObj)
                    Dim result = Eval(newObj.Body, newObj.Env)
                    If IsError(result) Then Return result

                    newObj.Env.SetValue("Me", newObj, False)
                    newObj.Env.SetValue("MyBase", newObj.BaseClass, False)

                    If newObj.CreateFunc IsNot Nothing Then
                        Eval(New CallExpression With
                         {
                            .Arguments = newObj.CreateArgs,
                            .Func = newObj.CreateFunc.Name
                         },
                         newObj.Env
                    )
                    End If
                ElseIf r.Type = ObjectType.VB_CLASS_OBJ Then
                    Dim classObj As VBClass = r
                    newObj = New VBClass With {.Env = New Environment, .Name = classObj.Name, .Members = classObj.Members, .Instance = classObj.Instance, .CreateFunc = classObj.CreateFunc, .CreateArgs = objCreateExp.Arguments, .OnPropertyChangeFunction = classObj.OnPropertyChangeFunction}

                    For Each item As Object In newObj.Members
                        newObj.Env.SetValue(item.Name.Value, item, False)
                    Next

                    If newObj.CreateFunc IsNot Nothing Then
                        Dim createFunction As VBFunction = newObj.CreateFunc
                        Dim result = createFunction.Func(EvalExpressions(newObj.CreateArgs, env))
                        If IsError(result) Then Return result
                    End If
                End If

                Return newObj
            Case GetType(FileImportExpression)
                Dim fileImportExp = TryCast(node, FileImportExpression)
                Dim className = DirectCast(fileImportExp.AliasName, Identifier).Value

                Dim classObj = New Fox_Class With {.Body = New BlockStatement, .Env = New ClassEnvironment, .Name = New Identifier With {.Value = className}}
                Dim filePath As Object = Eval(fileImportExp.FilePath, env)
                If IsError(filePath) Then Return filePath

                Dim runner As New Runner With {.Mode = "FILE", .FilePath = filePath.Value}
                classObj.Body.Statements = runner.RunModule(classObj.Env)

                env.SetValue(className, classObj, False)
                Return Nothing
            Case GetType(FromModuleImportExpression)
                Dim fromModuleImportExp = TryCast(node, FromModuleImportExpression)
                Dim moduleName As String = Trim(DirectCast(fromModuleImportExp.ModuleName, Identifier).Value)
                Dim importItemName As String = Trim(DirectCast(fromModuleImportExp.ImportItem, Identifier).Value)
                Runner.Eval($"import {moduleName}", env)

                Dim moduleClass = env.GetValue($"{moduleName}").Item1
                If IsError(moduleClass) Then Return moduleClass

                If importItemName = "*" Then
                    For Each keyPair As KeyValuePair(Of String, Data) In moduleClass.Env.store
                        If env.store.ContainsKey(keyPair.Key) Then Continue For
                        env.store.Add(keyPair.Key, keyPair.Value)
                    Next
                    env.RemoveValue(moduleName)

                    Return Nothing
                End If

                Dim importItem As Fox_Object = moduleClass.Env.GetValue(importItemName).Item1
                env.SetValue(importItemName, importItem, False)
                env.RemoveValue(moduleName)

                Return Nothing
            Case GetType(ModuleImportExpression)
                Dim moduleImportExp = TryCast(node, ModuleImportExpression)
                Dim moduleName = Trim(DirectCast(moduleImportExp.ModuleName, Identifier).Value)

                Dim className As String = Trim(DirectCast(If(moduleImportExp.AliasName, moduleImportExp.ModuleName), Identifier).Value)
                Dim classObj = New Fox_Class With {.Body = New BlockStatement, .Env = New ClassEnvironment, .Name = New Identifier With {.Value = className}}

                Dim getModulePath = env.GetValue("ModulePath")
                If Not getModulePath.Item2 Then Return ThrowError($"找不到库: {moduleName}")

                Dim modulePath = getModulePath.Item1
                Dim filePath = FindFile(modulePath.ToList(), moduleName, "Fox")
                If filePath IsNot Nothing Then
                    Dim runner As New Runner With {.Mode = "FILE", .FilePath = filePath}
                    classObj.Body.Statements = runner.RunModule(classObj.Env)
                    env.SetValue(className, classObj, False)
                    Return Nothing
                Else
                    filePath = FindFile(modulePath.ToList(), moduleName, "Dll")
                    If filePath Is Nothing Then Return ThrowError($"找不到库: {moduleName}")
                End If

                ' 加载程序集
                Dim assembly As Assembly = Assembly.LoadFrom(filePath)

                ' 获取要实例化的类的类型
                Dim classType As Type = assembly.GetType($"{moduleName}.Main")

                If classType Is Nothing Then Return ThrowError($"找不到类: {moduleName}.Main")

                ' 创建类的实例
                Dim instance As Object = Activator.CreateInstance(classType)
                Dim SharedItems As IEnumerable(Of Fox_Object) = classType.GetField("SharedItems").GetValue(instance)

                Dim vbClassObj = New VBClass With {
                    .Name = New Identifier(className),
                    .Members = New List(Of VB_Object),
                    .Env = New Environment
                }

                For Each item As Object In SharedItems
                    vbClassObj.Members.Add(item)
                    vbClassObj.Env.SetValue(item.Name.Value, item, False)
                Next

                env.SetValue(className, vbClassObj, False)
                Return Nothing
            Case GetType(SliceExpression)
                Dim sliceExp = TryCast(node, SliceExpression)
                Dim arrayObj = Eval(sliceExp.Left, env)
                If IsError(arrayObj) Then Return arrayObj

                Dim startIndex = Eval(sliceExp.StartIndex, env)
                If IsError(startIndex) Then Return startIndex

                Dim stopIndex = Eval(sliceExp.StopIndex, env)
                If IsError(stopIndex) Then Return stopIndex

                Dim stepIndex = Eval(sliceExp.IndexStep, env)
                If IsError(stepIndex) Then Return stepIndex

                Return EvaluatorHandlers(GetType(SliceExpression)).Eval(arrayObj, startIndex, stopIndex, stepIndex)
        End Select

        Return Nothing
    End Function



    Public Function ExtendClassEnv(ByRef clsObj As Fox_Class) As Fox_Object
        'Dim funcNames = FindAllFoxObject(clsObj.Env.GetObjects, ObjectType.FUNCTION_OBJ).Select(Function(item As Fox_Function) item.Name.Value).ToList
        'Dim classNames = FindAllFoxObject(clsObj.Env.GetObjects, ObjectType.CLASS_OBJ).Select(Function(item As Fox_Class) item.Name.Value).ToList

        'clsObj.Env.Funcs = GetObjectDictionary(funcNames, clsObj.Env.GetObjects, ObjectType.FUNCTION_OBJ)
        'clsObj.Env.Classes = GetObjectDictionary(classNames, clsObj.Env.GetObjects, ObjectType.CLASS_OBJ)

        'For Each name As String In clsObj.Env.store.Keys
        '    If Not (clsObj.Env.Funcs.ContainsKey(name) OrElse clsObj.Env.Classes.ContainsKey(name)) Then
        '        clsObj.Env.Vars.Add(name, clsObj.Env.store(name))
        '    End If
        'Next

        If clsObj.BaseClass IsNot Nothing Then
            If IsError(clsObj.BaseClass) Then Return clsObj.BaseClass

            Dim baseClass As Fox_Class = clsObj.BaseClass
            Dim baseEnv As Environment = baseClass.Env

            For Each keyPair As KeyValuePair(Of String, Data) In baseEnv.store
                Dim name = keyPair.Key
                Dim data = keyPair.Value
                If name = "Me" Then Continue For
                If name = "New" Then Continue For

                clsObj.Env.SetValue(name, data.FoxObject, data.isReadonly)
            Next

            Dim EnvStore As New Dictionary(Of String, Data)(clsObj.Env.store)
            For Each keyPair As KeyValuePair(Of String, Data) In EnvStore
                Dim name = keyPair.Key
                Dim data = keyPair.Value
                If name = "Me" Then Continue For

                If TypeOf data.FoxObject Is Fox_Function Then
                    Dim func As Fox_Function = data.FoxObject
                    func.Env = clsObj.Env

                    clsObj.Env.SetValue(name, func, data.isReadonly)
                End If

            Next
        End If

        Return Nothing
    End Function

    Public Function EvalObjectMemberExpression(leftObject As Fox_Object, rightExp As Expression, ByRef env As Environment)
        Select Case leftObject.Type
            Case ObjectType.CLASS_OBJ
                Dim classObj As Fox_Class = leftObject

                Dim result = Eval(rightExp, classObj.Env)
                Return result
            Case ObjectType.VB_CLASS_OBJ
                Dim vbClassObj As VBClass = leftObject

                Dim result = Eval(rightExp, vbClassObj.Env)
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
    Public Function EvalHashLiteral(
        Node As DictionaryLiteral,
        env As Environment
    ) As Fox_Object
        Dim pairs = New Dictionary(Of Fox_DictionaryKey, Fox_DictionaryPair)

        For Each parisNode In Node.Pairs
            Dim keyNode = parisNode.Key
            Dim valueNode = parisNode.Value

            Dim key = Eval(keyNode, env)
            If IsError(key) Then Return key

            Dim dictKeyType = key.Type
            Dim dictKey As Object = key
            Dim dictValue = Eval(valueNode, env)
            If IsError(dictValue) Then Return dictValue

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

    Public Function EvalIndexExpression(Left As Fox_Object, Index As Fox_Object) As Fox_Object
        If Left.Type() = ObjectType.ARRAY_OBJ AndAlso Index.Type() = ObjectType.INTEGER_OBJ Then
            '当左侧的对象是一个列表，且索引是一个整数，那么计算索引表达式的值
            Return EvalArrayIndexExpression(Left, Index)
        ElseIf Left.Type() = ObjectType.DICTIONARY_OBJ Then
            '当左侧的对象是一个字典，那么计算索引表达式的值
            Return EvalDictionaryIndexExpression(Left, Index)
        ElseIf Left.Type() = ObjectType.STRING_OBJ AndAlso Index.Type() = ObjectType.INTEGER_OBJ Then
            '当左侧的对象是一个字符串，且索引是一个整数，那么计算索引表达式的值
            Return EvalStringIndexExpression(Left, Index)
        Else
            Return ThrowError($"索引操作不支持 {Left.Type()}")
        End If
    End Function

    Public Function EvalStringIndexExpression(Str As Fox_Object, Index As Fox_Object) As Fox_Object
        '转换对象
        Dim stringObject = TryCast(Str, Fox_String)
        Dim indexObject = TryCast(Index, Fox_Integer)

        '获取对象的值
        Dim strValue = stringObject.Value
        Dim indexValue = indexObject.Value

        '比较索引值是否超过最大索引值
        If indexValue >= strValue.Count Then
            Return ThrowError($"索引 {indexValue} 超出字符串范围")
        End If

        '返回一个Fox_String对象 值为字符串对应索引的字符
        Return New Fox_String With {.Value = strValue(indexValue)}
    End Function


    Public Function EvalDictionaryIndexExpression(Dict As Fox_Object, Index As Fox_Object) As Fox_Object
        '尝试转换
        Dim dictionaryObject = TryCast(Dict, Fox_Dictionary)

        '字典键对象
        Dim key_obj As Fox_DictionaryKey = Fox_DictionaryHelper.CreateKey(Index)

        '如果转换失败，报错
        If dictionaryObject Is Nothing Then Return ThrowError($"不是一个字典: 类型 {Dict.Type} 内容 {Dict.Inspect}")

        Dim pair As Fox_DictionaryPair

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

    Public Function EvalArrayIndexExpression(Array As Fox_Object, Index As Fox_Object) As Fox_Object
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
    Public Function ApplyFunction(func As Fox_Object, args As List(Of Fox_Object)) As Fox_Object

        Select Case func.Type
            Case ObjectType.FUNCTION_OBJ
                '如果是Fox_Function 将对象转为 Fox_Function 对象
                Dim f = TryCast(func, Fox_Function)

                '扩展函数环境
                Dim extendedEnv = ExtendFunctionEnv(f, args)

                For Each keyPair As KeyValuePair(Of String, Data) In extendedEnv.store
                    If TypeOf keyPair.Value.FoxObject Is Fox_Error Then
                        Return keyPair.Value.FoxObject
                    End If
                Next

                '对欲运行的代码求值
                Dim evaluated = Eval(f.Body, extendedEnv)
                If IsError(evaluated) Then Return evaluated

                '解包返回函数
                Return UnwrapReturnValue(evaluated)
            Case ObjectType.BUILTIN_OBJ
                '若为Fox_Builtin，将对象转换为Fox_Builtin对象
                Dim builtin = TryCast(func, Fox_Builtin)

                '调用原生函数并返回它的值
                Return builtin.BuiltinFunction(args)
            Case ObjectType.VB_FUNCTION_OBJ
                Dim vbFunction = TryCast(func, VBFunction)
                Dim result = vbFunction.Func(args)
                If IsError(result) Then Return result

                Return result
            Case Else
                Return ThrowError($"不是一个函数: { func.Type}")
        End Select


    End Function

    Public Function ExtendFunctionEnv(
        ByRef func As Fox_Function,
        args As List(Of Fox_Object)
    ) As Environment
        Dim env = New Environment With {.outer = func.Env}

        Dim class_functioncall = env.outer.GetValue("Me")
        Dim base_class_functioncall = env.outer.GetValue("MyBase")

        If func.Parameters Is Nothing Then Return env

        If args.Count > func.Parameters.Count Then
            env.SetValue("", ThrowError($"提供的参数数量过多"), True)
            Return env
        ElseIf args.Count < func.Parameters.Count Then
            env.SetValue("", ThrowError($"提供的参数数量过少"), True)
            Return env
        End If

        For parmaIndex = 0 To func.Parameters.Count - 1
            env.SetValue(func.Parameters(parmaIndex).Value, args(parmaIndex), False)
        Next

        If class_functioncall.Item2 AndAlso base_class_functioncall.Item2 Then
        End If

        Return env
    End Function
    Public Function UnwrapReturnValue(obj As Fox_Object) As Fox_Object
        Dim returnValue = TryCast(obj, Fox_ReturnValue)
        If returnValue IsNot Nothing Then Return returnValue.Value
        Return returnValue
    End Function


    Public Function EvalExpressions(
        exps As List(Of Expression),
        ByRef env As Environment
    ) As List(Of Fox_Object)
        '创建新列表
        Dim result As New List(Of Fox_Object)

        '遍历表达式
        For Each e As Expression In exps
            '对表达式求值
            Dim evaluated = Eval(e, env)

            '若为错误对象则返回
            If IsError(evaluated) Then Return New List(Of Fox_Object) From {evaluated}

            '添加对象至列表中
            result.Add(evaluated)
        Next

        Return result
    End Function

    Public Function EvalIdentifier(
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



    '解析代码块语句
    Public Function EvalBlockStatement(block As BlockStatement, env As Environment) As Fox_Object
        Dim result = Nothing

        '循环遍历代码块的所有语句
        For Each s As Statement In block.Statements
            '求值
            Dim r = Eval(s, env)
            If r IsNot Nothing Then result = r

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
    Public Function EvalIfExpression(if_exp As IfExpression, env As Environment) As Fox_Object
        '获取条件表达式
        Dim condition = Eval(if_exp.Condition, env)

        '是否为错误对象
        If IsError(condition) Then Return condition

        If IsTruthy(condition) Then '条件为真
            '为真则返回 默认条件代码块
            Return Eval(if_exp.Consequence, env)
        ElseIf if_exp.ElseIf_List IsNot Nothing Then ' 条件不为真 但有ElseIf分支
            '遍历分支列表
            For Each elseif_exp As ElseIfExpression In if_exp.ElseIf_List
                '求条件的值
                Dim cond = Eval(elseif_exp.Condition, env)

                '条件为真
                If IsTruthy(cond) Then
                    '返回分支代码块
                    Return Eval(elseif_exp.Consequence, env)
                End If
            Next

            If if_exp.Alternative IsNot Nothing Then '条件不为真 但是 有Else分支代码块 
                '返回Else分支代码块
                Return Eval(if_exp.Alternative, env)
            End If

        ElseIf if_exp.Alternative IsNot Nothing Then '条件不为真 但是 有Else分支代码块 
            '返回Else分支代码块
            Return Eval(if_exp.Alternative, env)
        Else '都不是
            Return Obj_Nothing
        End If

        Return Obj_Nothing
    End Function

    Public Function IsTruthy(obj As Fox_Object) As Boolean
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
            Case GetType(Fox_String)
                Return Not String.IsNullOrEmpty(TryCast(obj, Fox_String).Value)
            Case Else
                '未知的类型 返回假
                Return False
        End Select
    End Function





    '求前缀表达式的值新建 Fox_Object
    Public Function EvalPrefixExpression(operator_ As String, right As Fox_Object) As Fox_Object
        '判断操作符并调用函数获取对应对象 

        '判空
        If right Is Nothing Then
            Return ThrowError($"未知的操作: {Obj_Nothing.Inspect} {operator_} {Obj_Nothing.Inspect}")
        End If

        Select Case operator_
            Case "!" '感叹号 表示 Not
                Return EvalNotOperatorExpression(right) 'Bool类型
            Case "-"
                Return EvalMinusPrefixOperatorExpression(right) 'Bool或Integer类型
            Case Else
                '信息大致内容: 未知的操作: [运算符] ,数值 [数值] 
                Return ThrowError($"未知的操作: {operator_}. {right}")
        End Select
    End Function

    Public Function EvalMinusPrefixOperatorExpression(Right As Fox_Object) As Fox_Object
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

        Return Obj_Nothing
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
            Case Obj_Nothing.Inspect() '若为Nothing
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
    Public Function EvalProgram(stmts As List(Of Statement), env As Environment) As Fox_Object
        Dim result As Fox_Object = Nothing

        '遍历所有语句
        For Each s As Object In stmts
            If s.GetType <> GetType(EOLStatement) AndAlso s.Token.TokenType <> TokenType.无意义 AndAlso s.Token.TokenType <> TokenType.ILLEGAL Then
                Dim r = Eval(s, env)
                If r IsNot Nothing Then result = r
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

