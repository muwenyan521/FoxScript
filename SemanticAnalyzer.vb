Imports System.Numerics
Imports System.Runtime.InteropServices

Imports FoxScript.NumberError
Public Class SemanticAnalyzer
    Private Errors As New List(Of String)
    Private infixFunctionsDictionary As New Dictionary(Of Type, Func(Of InfixExpression, Expression)) From {
        {GetType(IntegerLiteral), AddressOf OptimizeIntegerInfixExpression},
        {GetType(InfixExpression), AddressOf OptimizeInfixExpression}
    }
    Public Sub Analysis(ByRef node As Object)
        If node Is Nothing Then Return  '判空

        Select Case node.GetType() '获取类型
            Case GetType(Program) '如果是Program

                '求所有语句的Fox_Object对象
                AnalysisProgram(node.Statements)
            Case GetType(ExpressionStatement) '若为表达式

                '获取表达式语句的表达式 并求值
                Analysis(node.Expression)
            Case GetType(IntegerLiteral) '若为整数
                Return
            Case GetType(DoubleLiteral) '若为小数
                Return
            Case GetType(Bool) '若为布尔类型
                Return
            Case GetType(PrefixExpression)
                Dim prefixExp As PrefixExpression = node

                If prefixExp.Right Is Nothing Then Errors.Add($"{prefixExp.Operator_} 后面应为表达式 在第{prefixExp.Token.Line}行")
            Case GetType(InfixExpression) '中缀表达式
                If node.Left Is Nothing OrElse node.Right Is Nothing Then
                    Errors.Add($"错误的表达式: 内容 {node.ToString} 在第{node.Token.Line}行")
                    Return
                End If

                Analysis(node.Left)
                Analysis(node.Right)

                Dim handler = GetOptimizeHandler(node.Left.GetType(), node.Right.GetType)
                If handler IsNot Nothing Then
                    Dim result = handler(node)
                    If result IsNot Nothing Then node = result
                End If
            Case GetType(BlockStatement) '若为代码块
                AnalysisBlockStatement(node)
            Case GetType(IfExpression) '若为 if表达式
                AnalysisIfExpression(node)
            Case GetType(ReturnStatement) '若为返回语句
                '将欲返回的表达式进行求值
                Analysis(node.ReturnValue)
            Case GetType(DimStatement) '若为 Dim 语句
                Dim dim_stmt = TryCast(node, DimStatement)

                Analysis(dim_stmt.Name)

                If dim_stmt.Value Is Nothing Then Errors.Add($"= 后面应为表达式 在第{node.Token.Line}行")
            Case GetType(FunctionLiteral)
                '获取函数名
                Dim funcLiteral = TryCast(node, FunctionLiteral)

                Analysis(funcLiteral.Name)

                Dim exps As New List(Of Expression)
                For Each ident As Identifier In funcLiteral.Parameters
                    exps.Add(ident)
                Next

                AnalysisExpressions(exps)
                Analysis(funcLiteral.Body)
            Case GetType(CallExpression) '若为调用表达式
                Dim call_exp As CallExpression = node

                If call_exp.Arguments Is Nothing Then Return
                AnalysisExpressions(call_exp.Arguments)
            Case GetType(Identifier) '若为标识符
                Dim ident As Identifier = node
                If Char.IsDigit(ident.Value(0)) Then
                    Errors.Add($"标识符第一个字符不可为数字 在第{node.Token.Line}行")
                End If

            Case GetType(StringLiteral)
                Return
            Case GetType(ArrayLiteral)
                AnalysisExpressions(node.Elements)
            Case GetType(IndexExpression)
                Analysis(node.Left)
                Analysis(node.Index)
            Case GetType(DictionaryLiteral)
                Dim dict_literal As DictionaryLiteral = node

                AnalysisExpressions(dict_literal.Pairs.Keys.ToList)
                AnalysisExpressions(dict_literal.Pairs.Values.ToList)
            Case GetType(ForStatement)
                '将当前节点转换成 ForStatement
                Dim for_stmt = TryCast(node, ForStatement)

                Analysis(for_stmt.Items)
                Analysis(for_stmt.ItemVar)
                Analysis(for_stmt.LoopBlock)

            Case GetType(WhileStatement)
                '将当前节点转换成 WhileStatement
                Dim while_stmt = TryCast(node, WhileStatement)

                Analysis(while_stmt.LoopCondition)
                Analysis(while_stmt.LoopBlock)

            Case GetType(AssignmentExpression)
                Dim assignmentExp = TryCast(node, AssignmentExpression)

                Analysis(assignmentExp.SetExp)

                If assignmentExp.Value Is Nothing Then Errors.Add($"= 后面应为表达式 在第{node.Token.Line}行")
                Analysis(assignmentExp.Value)
            Case GetType(NotExpression)
                Dim notExp = TryCast(node, NotExpression)

                If notExp.Right Is Nothing Then Errors.Add($"Not 后面应为表达式 在第{node.Token.Line}行")
            Case GetType(AndExpression)
                Dim andExp = TryCast(node, AndExpression)

                If andExp.Left Is Nothing Then
                    Errors.Add($"And 前面应为表达式 在第{node.Token.Line}行")
                End If

                If andExp.Right Is Nothing Then
                    Errors.Add($"And 后面应为表达式 在第{node.Token.Line}行")
                End If
            Case GetType(OrExpression)
                Dim orExp = TryCast(node, OrExpression)

                Analysis(orExp.Left)
                Analysis(orExp.Right)
            Case GetType(ClassStatement)
                Dim classStmt = TryCast(node, ClassStatement)

                Analysis(classStmt.Name)
                Analysis(classStmt.Body)

            Case GetType(ObjectMemberExpression)
                Dim objMemberExp = TryCast(node, ObjectMemberExpression)

                Analysis(objMemberExp.Left)
                Analysis(objMemberExp.Right)
            Case GetType(ObjectCreateExpression)
                Dim objCreateExp = TryCast(node, ObjectCreateExpression)

                Analysis(objCreateExp.ObjType)
                AnalysisExpressions(objCreateExp.Arguments)
            Case GetType(FileImportExpression)
                Dim fileImportExp = TryCast(node, FileImportExpression)

                Analysis(fileImportExp.FilePath)
                Analysis(fileImportExp.AliasName)
        End Select

        Return
    End Sub




    Public Sub AnalysisExpressions(exps As List(Of Expression))
        For Each exp As Expression In exps.ToList
            Analysis(exp)
        Next
    End Sub


    '解析代码块语句
    Public Sub AnalysisBlockStatement(block As BlockStatement)
        For Each s As Statement In block.Statements
            Analysis(s)
        Next

    End Sub

    '获取if表达式的值 返回一个 Fox_Object 类型的对象
    Public Sub AnalysisIfExpression(if_exp As IfExpression)
        Analysis(if_exp.Condition)
        If (TypeOf if_exp.Condition Is Bool) AndAlso (Not CType(if_exp.Condition, Bool).Value) Then
            if_exp.Consequence.Statements.Clear()
        End If

        Analysis(if_exp.Consequence)

        If if_exp.ElseIf_List IsNot Nothing Then ' 有ElseIf分支
            '遍历分支列表
            For Each elseif_exp As ElseIfExpression In if_exp.ElseIf_List
                Analysis(elseif_exp.Condition)
                Analysis(elseif_exp.Consequence)
            Next


        ElseIf if_exp.Alternative IsNot Nothing Then '条件不为真 但是 有Else分支代码块 
            Analysis(if_exp.Alternative)
        End If

    End Sub

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
            Case Else
                '未知的类型 返回假
                Return False
        End Select
    End Function

    Public Function OptimizeIntegerInfixExpression(InfixExp As InfixExpression) As Expression
        If Not ((TypeOf InfixExp.Left Is IntegerLiteral) AndAlso (TypeOf InfixExp.Right Is IntegerLiteral)) Then Return Nothing

        Dim leftVal = CType(InfixExp.Left, Object).Value.ToString
        Dim rightVal = CType(InfixExp.Right, Object).Value.ToString

        Try
            Dim LiteralDictionary As New Dictionary(Of String, Expression) From {
                {"+", New IntegerLiteral With {.Value = BigInteger.Parse(leftVal) + BigInteger.Parse(rightVal)}},
                {"-", New IntegerLiteral With {.Value = BigInteger.Parse(leftVal) - BigInteger.Parse(rightVal)}},
                {"*", New IntegerLiteral With {.Value = BigInteger.Parse(leftVal) * BigInteger.Parse(rightVal)}},
                {"==", New Bool With {.Value = BigInteger.Parse(leftVal) = BigInteger.Parse(rightVal)}},
                {"!=", New Bool With {.Value = BigInteger.Parse(leftVal) <> BigInteger.Parse(rightVal)}},
                {"<>", New Bool With {.Value = BigInteger.Parse(leftVal) <> BigInteger.Parse(rightVal)}}
            }

            If InfixExp.Operator_ = "/" Then
                If CDec(rightVal) = 0 Then
                    Errors.Add($"除数不能为0")
                    Return Nothing
                End If

                If CDec(rightVal) = 1 Then
                    Return InfixExp.Left
                End If

                Return New DoubleLiteral With {.Value = CDec(leftVal) / CDec(rightVal)}
            Else
                If Not LiteralDictionary.ContainsKey(InfixExp.Operator_) Then Return Nothing
                Return LiteralDictionary(InfixExp.Operator_)
            End If
        Catch ex As OverflowException
            Errors.Add($"无法解析数值，因为 ""{ErrorDictionary(ex.GetType)}""")
        End Try

        Return Nothing
    End Function

    Public Function OptimizeInfixExpression(InfixExp As InfixExpression) As Expression
        Dim left As Expression = InfixExp.Left
        Dim right As Expression = InfixExp.Right

        Dim optimized_left As Expression = Nothing

        Dim leftHandler = GetOptimizeHandler(right.GetType(), right.GetType)
        If leftHandler Is Nothing Then Return leftHandler

        optimized_left = leftHandler(left)

        Dim handler = GetOptimizeHandler(leftHandler.GetType(), right.GetType)
        If handler Is Nothing Then Return handler

        Return handler(New InfixExpression(InfixExp.Operator_, optimized_left, right))
    End Function

    Public Function GetOptimizeHandler(leftType As Type, rightType As Type) As Func(Of InfixExpression, Expression)
        Return If(infixFunctionsDictionary.ContainsKey(leftType), infixFunctionsDictionary(leftType), Nothing)
    End Function

    Public Shared Function BoolToLong(bool As Boolean) As Long
        '翻译:转Long ( 转String ( 转Long ( 逻辑 ) .Replace("-","") ) ) 
        '这样做是因为CLng转boolean会变成负数
        Return CLng(CStr(CLng(bool)).Replace("-", ""))
    End Function


    Public Sub AnalysisProgram(stmts As List(Of Statement))
        '遍历所有语句
        For Each s As Object In stmts
            If s.GetType <> GetType(EOLStatement) AndAlso s.Token.TokenType <> TokenType.无意义 AndAlso s.Token.TokenType <> TokenType.ILLEGAL Then
                Analysis(s)
            End If
        Next
    End Sub

    Public Function CheckErrors() As Boolean
        If Errors.Count > 0 Then
            For Each err As String In Errors
                Console.WriteLine($"{err}")
            Next

            Return True
        End If

        Return False
    End Function
End Class

