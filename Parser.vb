Imports System.IO
Imports System.Numerics

Public Enum 优先级
    无意义
    LOWEST
    EQUALS      '//== 
    LESSGREATER '// > Or <
    SUM         '// + 
    PRODUCT    ' // * 
    PREFIX     ' // -X Or !X 
    CALL_       ' // myFunction(X)
    INDEX '// array[index] 
    OBJ_MEMBER ' person.Name
End Enum

Public Class Parser
    Public 优先级字典 As New Dictionary(Of TokenType, 优先级) From
    {
        {TokenType.EQ, 优先级.EQUALS},
        {TokenType.NOT_EQ, 优先级.EQUALS},
        {TokenType.LT, 优先级.LESSGREATER},
        {TokenType.GT, 优先级.LESSGREATER},
        {TokenType.PLUS, 优先级.SUM},
        {TokenType.MINUS, 优先级.SUM},
        {TokenType.SLASH, 优先级.PRODUCT},
        {TokenType.ASTERISK, 优先级.PRODUCT},
        {TokenType.LPAREN, 优先级.CALL_},
        {TokenType.LBRACKET, 优先级.INDEX}
    }


    Dim l As Lexer
    Dim curToken As Token
    Dim peekToken As Token
    Dim tknPos As Integer = 0
    Dim tokens As List(Of Token)
    Dim errors As New List(Of String)

    Public Delegate Function PrefixParseFunction() As Expression
    Public Delegate Function InfixParseFunction(ByVal left As Expression) As Expression

    ' 声明委托字段
    Private prefixParseFns As New Dictionary(Of TokenType, PrefixParseFunction)
    Private infixParseFns As New Dictionary(Of TokenType, InfixParseFunction)

    Public Sub New(lexer As Lexer)
        l = lexer
        '初始化
        curToken = New Token(TokenType.无意义, vbNullChar)
        peekToken = New Token(TokenType.无意义, vbNullChar)

        prefixParseFns.Add(TokenType.无意义, AddressOf 无意义)
        prefixParseFns.Add(TokenType.ILLEGAL, AddressOf 无意义)
        infixParseFns.Add(TokenType.无意义, AddressOf 无意义)
        infixParseFns.Add(TokenType.ILLEGAL, AddressOf 无意义)

        prefixParseFns.Add(TokenType.ENDFUNC, AddressOf 无意义)
        prefixParseFns.Add(TokenType.IN_, AddressOf 无意义)

        '获取所有Token
        tokens = l.lexer

        '注册前缀符

        prefixParseFns.Add(TokenType.IDENT, AddressOf parseIdentifier)
        prefixParseFns.Add(TokenType.IMPORT, AddressOf parseFileImpprtExpression)
        prefixParseFns.Add(TokenType.NEW_, AddressOf parseObjectCreateExpression)
        prefixParseFns.Add(TokenType.LBRACE, AddressOf parseDictionaryLiteral)
        prefixParseFns.Add(TokenType.LBRACKET, AddressOf parseArrayLiteral)
        prefixParseFns.Add(TokenType.IF_, AddressOf parseIfExpression)
        prefixParseFns.Add(TokenType.BOOL_NOT, AddressOf parseNotExpression)
        prefixParseFns.Add(TokenType.INTNUMBER, AddressOf parseNumberLiteral)
        prefixParseFns.Add(TokenType.MINUS, AddressOf parsePrefixExpression)
        prefixParseFns.Add(TokenType.BANG, AddressOf parsePrefixExpression)
        prefixParseFns.Add(TokenType.BOOL_TRUE, AddressOf parseBoolean)
        prefixParseFns.Add(TokenType.BOOL_FALSE, AddressOf parseBoolean)
        prefixParseFns.Add(TokenType.LPAREN, AddressOf parseGroupedExpression)
        prefixParseFns.Add(TokenType.FUNC, AddressOf parseFunctionLiteral)
        prefixParseFns.Add(TokenType.STRING_, AddressOf parseStringLiteral)

        '注册中缀符
        infixParseFns.Add(TokenType.PLUS, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.MINUS, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.SLASH, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.ASTERISK, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.EQ, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.NOT_EQ, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.LT, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.GT, AddressOf parseInfixExpression)
        infixParseFns.Add(TokenType.LPAREN, AddressOf parseCallExpression)
        infixParseFns.Add(TokenType.LBRACKET, AddressOf parseIndexExpression)

    End Sub

    '解析while语句
    Public Function parseWhileStatement() As Statement
        '初始化
        Dim stmt = New WhileStatement With {.Token = curToken}

        '下一个Token
        nextToken()

        '解析表达式设置为循环条件
        stmt.LoopCondition = parseExpression(优先级.LOWEST)

        If Not expectPeek(TokenType.EOL) Then
            Return Nothing
        End If

        '下一个Token
        nextToken()

        '解析代码块语句并设置为 循环块
        stmt.LoopBlock = parseBlockStatement()

        If Not expectCur(TokenType.ENDWHILE) Then
            Return Nothing
        End If

        Return stmt
    End Function

    '解析class语句
    Public Function parseClassStatement() As Statement
        '初始化
        Dim stmt = New ClassStatement With {.Token = curToken}

        '下一个Token
        nextToken()

        '解析表达式设置为类名
        stmt.Name = parseExpression(优先级.LOWEST)

        If Not expectPeek(TokenType.EOL) Then
            Return Nothing
        End If

        '下一个Token
        nextToken()

        '解析代码块语句并设置为类的块
        stmt.Body = parseBlockStatement()

        If Not expectCur(TokenType.ENDCLASS) Then
            Return Nothing
        End If

        Return stmt
    End Function


    '解析for语句
    Public Function parseForStatement() As Statement
        '初始化
        Dim stmt = New ForStatement With {.Token = curToken}

        '下一个Token
        nextToken()

        If Not expectCur(TokenType.IDENT) Then
            Return Nothing
        End If

        '解析表达式设置为迭代变量
        stmt.ItemVar = parseExpression(优先级.LOWEST)

        If Not expectPeek(TokenType.IN_) Then
            Return Nothing
        End If

        '下一个Token
        nextToken()

        '解析表达式设置为 待遍历的列表
        stmt.Items = parseExpression(优先级.LOWEST)


        If Not expectPeek(TokenType.EOL) Then
            Return Nothing
        End If

        '解析代码块语句并设置为 循环块
        stmt.LoopBlock = parseBlockStatement()

        Return stmt
    End Function


    Public Function parseDictionaryLiteral() As Expression
        Dim dict = New DictionaryLiteral With {.Token = curToken}
        dict.Pairs = New Dictionary(Of Expression, Expression)

        While Not peekTokenIs(TokenType.RBRACE)
            nextToken()
            Dim key = parseExpression(优先级.LOWEST)

            If Not expectPeek(TokenType.COLON) Then
                Return Nothing
            End If

            nextToken()
            Dim value = parseExpression(优先级.LOWEST)

            dict.Pairs(key) = value
            If Not peekTokenIs(TokenType.RBRACE) AndAlso Not expectPeek(TokenType.COMMA) Then
                Return Nothing
            End If
        End While

        nextToken()

        Dim additionalExp = parseAdditionalExpressions(dict)
        Return If(additionalExp IsNot Nothing, additionalExp, dict)
    End Function

    Public Function parseIndexExpression(left As Expression) As Expression
        Dim exp = New IndexExpression With {.Token = curToken, .Left = left}
        nextToken()
        exp.Index = parseExpression(优先级.LOWEST)
        If Not expectPeek(TokenType.RBRACKET) Then
            Return Nothing
        End If

        Dim additionalExp = parseAdditionalExpressions(exp)
        Return If(additionalExp IsNot Nothing, additionalExp, exp)
    End Function

    Public Function parseArrayLiteral() As Expression
        Dim array = New ArrayLiteral With {.Token = curToken}

        array.Elements = parseExpressionList(TokenType.RBRACKET)

        Dim additionalExp = parseAdditionalExpressions(array)
        Return If(additionalExp IsNot Nothing, additionalExp, array)
    End Function

    Public Function parseExpressionList(end_ As TokenType) As List(Of Expression)
        Dim list = New List(Of Expression)

        If curTokenIs(end_) Then
            Return list
        End If

        If peekTokenIs(end_) Then
            nextToken()
            Return list
        End If

        nextToken()
        list.Add(parseExpression(优先级.LOWEST))

        While peekTokenIs(TokenType.COMMA)
            nextToken()
            nextToken()
            list.Add(parseExpression(优先级.LOWEST))
        End While

        If Not curTokenIs(end_) Then
            If peekTokenIs(end_) Then
                nextToken()
                Return list
            End If
            expectCur(end_)
            Return Nothing
        End If

        Return list
    End Function

    Public Function parseStringLiteral() As Expression
        Dim StrExp = New StringLiteral With {.Token = curToken, .Value = curToken.Value}

        Dim additionalExp = parseAdditionalExpressions(StrExp)
        Return If(additionalExp IsNot Nothing, additionalExp, StrExp)
    End Function

    '解析函数调用表达式
    Public Function parseCallExpression(func As Expression) As Expression
        '创建一个函数调用表达式
        Dim exp = New CallExpression With {.Token = curToken, .Func = func, .Arguments = parseExpressionList(TokenType.RPAREN)}

        Dim additionalExp = parseAdditionalExpressions(exp)
        Return If(additionalExp IsNot Nothing, additionalExp, exp)
    End Function

    '没啥意义
    Public Function 无意义()
        Return Nothing
    End Function


    '解析调用参数
    Public Function parseCallArguments() As List(Of Expression)
        Dim args = New List(Of Expression)

        '判断下一个词法单元是否为 右括号
        If peekTokenIs(TokenType.RPAREN) Then
            '如果是 那么跳转至下一个Token
            nextToken()

            '返回参数列表
            Return args
        End If

        '跳转到下一个词法单元
        nextToken()

        '将解析到的表达式添加到实参列表
        args.Add(parseExpression(优先级.LOWEST))

        '如果下一个词法单元为逗号 "," 重复执行
        While peekTokenIs(TokenType.COMMA)
            '跳转词法单元
            nextToken()
            nextToken()

            '将解析到的表达式添加到实参列表
            args.Add(parseExpression(优先级.LOWEST))
        End While

        If Not expectPeek(TokenType.RPAREN) Then
            Return Nothing
        End If

        Return args
    End Function

    '解析if表达式  
    Public Function parseIfExpression() As Expression
        '初始化
        Dim Expression = New IfExpression With {.Token = curToken}

        '下一个Token
        nextToken()

        '解析表达式并设置为条件
        Expression.Condition = parseExpression(优先级.LOWEST)

        If Not expectPeek(TokenType.THEN_) Then
            Return Nothing
        End If

        '解析代码块语句并设置为 默认的块（if条件的块）
        Expression.Consequence = parseBlockStatement()

        '如果当前Token是ElseIf
        If curTokenIs(TokenType.ELSEIF_) Then
            '解析代码块语句并设置为ElseIf条件的块

            Expression.ElseIf_List = New List(Of ElseIfExpression)
            While Not curTokenIs(TokenType.THEN_) AndAlso Not curTokenIs(TokenType.ELSE_) AndAlso Not curTokenIs(TokenType.ENDIF_) AndAlso Not curTokenIs(TokenType.EOF)
                Dim elseif_exp = parseElseIfExpression()
                Expression.ElseIf_List.Add(elseif_exp)
            End While
        End If

        '如果当前Token是Else
        If curTokenIs(TokenType.ELSE_) Then
            '解析代码块语句并设置为Else条件的块
            Expression.Alternative = parseBlockStatement()
        End If


        Dim additionalExp = parseAdditionalExpressions(Expression)
        Return If(additionalExp IsNot Nothing, additionalExp, Expression)
    End Function

    '解析not表达式  
    Public Function parseNotExpression() As Expression
        '初始化
        Dim Expression = New NotExpression With {.Token = curToken}

        '下一个Token
        nextToken()

        '解析表达式并设置为右侧表达式
        Expression.Right = parseExpression(优先级.LOWEST)

        Dim additionalExp = parseAdditionalExpressions(Expression)
        Return If(additionalExp IsNot Nothing, additionalExp, Expression)
    End Function


    '解析elseif表达式  
    Public Function parseElseIfExpression() As Expression
        '初始化
        Dim Expression = New ElseIfExpression With {.Token = curToken}

        '下一个Token
        nextToken()

        '解析表达式并设置为条件
        Expression.Condition = parseExpression(优先级.LOWEST)

        If Not expectPeek(TokenType.THEN_) AndAlso Not expectCur(TokenType.THEN_) Then
            Return Nothing
        End If

        '解析代码块语句并设置为 默认的块（elseif条件的块）
        Expression.Consequence = parseBlockStatement()

        Return Expression
    End Function

    '解析函数头
    Public Function parseFunctionLiteral() As Expression
        '初始化
        Dim lit = New FunctionLiteral With {.Token = curToken}

        '下一个Token
        nextToken()

        lit.Name = New Identifier() With {.Token = curToken, .Value = curToken.Value}

        '解析函数实参并设置
        lit.Parameters = parseFunctionParameters()

        '解析代码块并设置为函数的代码块
        lit.Body = parseBlockStatement()

        Return lit
    End Function

    '解析函数实参
    Public Function parseFunctionParameters() As List(Of Identifier)
        '初始化
        Dim identifiers As New List(Of Identifier)

        nextToken()

        '如果下一个Token是右括号
        If peekTokenIs(TokenType.RPAREN) Then
            '下一个Token
            nextToken()

            '返回
            Return identifiers
        Else
            backToken()
        End If


        '继续下一个Token
        nextToken()

        '新建Ident
        Dim ident = New Identifier With {.Token = peekToken, .Value = peekToken.Value}

        '添加至列表
        identifiers.Add(ident)

        '继续下一个Token
        nextToken()

        '重复执行直到下一个Token不是逗号
        While peekTokenIs(TokenType.COMMA)
            '继续下一个Token
            nextToken()
            nextToken()

            '创建ident_并添加至列表
            Dim ident_ = New Identifier With {.Token = curToken, .Value = curToken.Value}
            identifiers.Add(ident_)
        End While

        nextToken()
        '如果不是右括号 
        If expectCur(TokenType.RPAREN) Then
            Return identifiers
        End If

        If expectCur(TokenType.RPAREN) Then
            Return identifiers
        End If

        Return Nothing '返回空
    End Function

    '解析代码块
    Public Function parseBlockStatement() As BlockStatement
        '初始化
        Dim block = New BlockStatement With {.Token = curToken}
        block.Statements = New List(Of Statement)

        '继续下一个Token
        nextToken()

        While Not curTokenIs(TokenType.NEXT_) AndAlso Not curTokenIs(TokenType.ENDWHILE) AndAlso Not curTokenIs(TokenType.ENDFUNC) AndAlso Not curTokenIs(TokenType.ENDIF_) AndAlso Not curTokenIs(TokenType.ELSEIF_) AndAlso Not curTokenIs(TokenType.ELSE_) AndAlso Not curTokenIs(TokenType.ENDCLASS) AndAlso Not curTokenIs(TokenType.EOF)

            '解析语句
            Dim stmt = parseStatement()
            If stmt IsNot Nothing Then '判空
                block.Statements.Add(stmt) '添加stmt至列表
            End If

            '继续下一个Token
            nextToken()
        End While
        Return block
    End Function

    '...这咋说？有括号先算小括号里面的？ [绷]
    Public Function parseGroupedExpression() As Expression
        '继续下一个Token
        nextToken()

        '...
        Dim exp = parseExpression(优先级.LOWEST)

        '检查下一个Token是否右括号
        If Not expectPeek(TokenType.RPAREN) Then
            Return Nothing
        End If
        Return exp
    End Function

    '解析中缀表达式
    Public Function parseInfixExpression(left As Expression) As Expression
        '初始化
        Dim Expression = New InfixExpression With {
            .Token = curToken,
            .Operator_ = curToken.Value,
            .Left = left
        }
        '...
        Dim precedence = curPrecedence()
        nextToken()
        If Expression.Operator_ = "+" Then
            Expression.Right = parseExpression(precedence - 1)
        Else
            Expression.Right = parseExpression(precedence)
        End If

        Return Expression
    End Function

    '...
    Public Function peekPrecedence() As Integer
        If 优先级字典.ContainsKey(peekToken.TokenType) Then
            Return 优先级字典(peekToken.TokenType)
        End If

        Return 优先级.LOWEST
    End Function

    '...
    Public Function curPrecedence() As Integer
        If 优先级字典.ContainsKey(curToken.TokenType) Then
            Return 优先级字典(curToken.TokenType)
        End If

        Return 优先级.LOWEST
    End Function


    '解析前缀表达式
    Public Function parsePrefixExpression() As Expression
        Dim Expression = New PrefixExpression With {
            .Token = curToken,
            .Operator_ = curToken.Value
        }
        nextToken()
        Expression.Right = parseExpression(优先级.PREFIX)
        Return Expression
    End Function


    '解析标识符
    Public Function parseIdentifier() As Expression
        Dim ident = New Identifier With {.Token = curToken, .Value = curToken.Value}

        Dim additionalExp = parseAdditionalExpressions(ident)
        Return If(additionalExp IsNot Nothing, additionalExp, ident)

    End Function


    '解析对象创建表达式
    Public Function parseObjectCreateExpression() As Expression
        Dim objectCreateExp = New ObjectCreateExpression With {.Token = curToken}

        nextToken()
        objectCreateExp.ObjType = parseExpression(优先级.OBJ_MEMBER)

        nextToken()
        objectCreateExp.Arguments = parseExpressionList(TokenType.RPAREN)

        Dim additionalExp = parseAdditionalExpressions(objectCreateExp)
        Return If(additionalExp IsNot Nothing, additionalExp, objectCreateExp)

    End Function

    '解析文件导入表达式
    Public Function parseFileImpprtExpression() As Expression
        Dim fileImportExp = New FileImportExpression With {.Token = curToken}

        nextToken()
        fileImportExp.FilePath = parseExpression(优先级.LOWEST)

        nextToken()
        nextToken()
        fileImportExp.AliasName = parseExpression(优先级.LOWEST)

        Return fileImportExp
    End Function



    Public Function parseAdditionalExpressions(expression As Expression) As Expression
        Dim currentExp As Expression = expression
        Dim newExp As Expression

        ' 尝试解析逻辑表达式
        newExp = parseLogicalExpression(currentExp)
        If newExp IsNot Nothing Then
            currentExp = newExp
        End If

        ' 尝试解析对象成员表达式
ObjMemberParse:
        newExp = parseObjectMemberExpression(currentExp)
        If newExp IsNot Nothing Then
            currentExp = newExp
            GoTo ObjMemberParse
        End If


        newExp = parseAssignmentExpression(currentExp)
        If newExp IsNot Nothing Then
            currentExp = newExp
        End If

        Return currentExp
    End Function

    Public Function parseObjectMemberExpression(leftExp As Expression)
        If peekTokenIs(TokenType.DOT) Then
            Dim exp As New ObjectMemberExpression With {.Token = curToken}
            exp.Left = leftExp

            '下一个Token
            nextToken()
            nextToken()

            exp.Right = New Identifier With {.Token = curToken, .Value = curToken.Value}

            Return exp
        End If

        Return Nothing
    End Function

    Public Function parseAssignmentExpression(leftExp As Expression)

        If peekTokenIs(TokenType.ASSIGN) Then

            '下一个Token
            nextToken()

            Dim exp As New AssignmentExpression With {.Token = curToken, .SetExp = leftExp}

            '下一个Token
            nextToken()

            '解析表达式设置为标识符的值
            exp.Value = parseExpression(优先级.LOWEST)


            If Not expectPeek(TokenType.EOL) Then
                Return Nothing
            End If

            Return exp
        End If

        Return Nothing
    End Function

    Public Function parseLogicalExpression(leftExp As Expression)
        If peekTokenIs(TokenType.BOOL_AND) Then
            Dim andExp As New AndExpression With {.Token = peekToken, .Left = leftExp}

            '前进词法单元
            nextToken()
            nextToken()

            Dim rightExp = parseExpression(优先级.LOWEST)
            andExp.Right = rightExp

            Return andExp
        End If

        If peekTokenIs(TokenType.BOOL_OR) Then
            Dim orExp As New OrExpression With {.Token = peekToken, .Left = leftExp}

            '前进词法单元
            nextToken()
            nextToken()

            Dim rightExp = parseExpression(优先级.LOWEST)
            orExp.Right = rightExp

            Return orExp
        End If

        Return Nothing
    End Function

    '解析Bool表达式
    Public Function parseBoolean() As Expression
        Dim BoolExp = New Bool With {.Token = curToken, .Value = curTokenIs(TokenType.BOOL_TRUE)}

        Dim additionalExp = parseAdditionalExpressions(BoolExp)
        Return If(additionalExp IsNot Nothing, additionalExp, BoolExp)
    End Function

    '解析整数表达式
    Public Function parseNumberLiteral() As Expression
        Dim lit As Object = New IntegerLiteral With {.Token = curToken}

        If peekTokenIs(TokenType.DOT) Then
            Dim dbl_lit = New DoubleLiteral With {.Token = curToken}
            nextToken()
            nextToken()

            If curTokenIs(TokenType.INTNUMBER) Then
                Dim dbl_str = $"{dbl_lit.Token.Value.ToString.Replace(vbCr, "")}.{curToken.Value.ToString.Replace(vbCr, "")}"

                Try
                    dbl_lit.Value = CDec(dbl_str)
                Catch err As Exception
                    Dim message = $"无法转换数值为小数, 数值: {CStr(curToken.Value)}"
                    If NumberError.ErrorDictionary.ContainsKey(err.GetType) Then
                        message = $"无法转换数值为小数, 因为""{NumberError.ErrorDictionary(err.GetType())}"" 数值: {CStr(curToken.Value)}"
                    End If
                    errors.Add(message)

                    Return Nothing
                End Try

                Dim double_additionalExp = parseAdditionalExpressions(dbl_lit)
                Return If(double_additionalExp IsNot Nothing, double_additionalExp, dbl_lit)
            End If
        End If

        Try
            lit.Value = BigInteger.Parse(curToken.Value.ToString.Replace(vbCr, ""))
        Catch err As Exception
            Dim message = $"无法转换数值为整数, 数值: {CStr(curToken.Value)}"
            If NumberError.ErrorDictionary.ContainsKey(err.GetType) Then
                message = $"无法转换数值为整数, 因为""{NumberError.ErrorDictionary(err.GetType())}"" 数值: {CStr(curToken.Value)}"
            End If
            errors.Add(message)

            Return Nothing
        End Try

        Dim additionalExp = parseAdditionalExpressions(lit)
        Return If(additionalExp IsNot Nothing, additionalExp, lit)
    End Function

    '...
    Public Sub nextToken()
        '若超过索引
        If tknPos >= (tokens.Count - 1) Then
            '设置TokenType 为 EOF
            curToken = New Token(TokenType.EOF, vbNullChar)
            peekToken = New Token(TokenType.EOF, vbNullChar)
        Else '否则

            '设置当前token为 tokens(tknPos)
            curToken = tokens(tknPos)
            peekToken = tokens(tknPos + 1)

            '索引 + 1
            tknPos += 1
        End If
    End Sub
    Public Sub backToken()
        '请不要滥用此函数！！！

        '索引 - 1
        tknPos -= 1

        curToken = tokens(tknPos - 1)
        peekToken = tokens(tknPos)
    End Sub

    '解析程序 返回一个Program类型的对象
    Public Function ParseProgram() As Program
        '初始化
        Dim program As New Program
        program.Statements = New List(Of Statement)

        '重复执行直到curToken.TokenType = TokenType.EOF
        While curToken.TokenType <> TokenType.EOF
            '解析语句...
            Dim stmt = parseStatement()
            If stmt IsNot Nothing Then '判空
                program.Statements.Add(stmt) '新增stmt至 program 的 Statements列表
            End If

            '继续下一个Token
            nextToken()
        End While

        Return program
    End Function

    '解析语句 返回一个 Statement 类型的 对象
    Public Function parseStatement() As Statement
        Select Case curToken.TokenType
            Case TokenType.DIM_, TokenType.LET_ 'dim语句
                Return parseDimStatement() '解析dim语句
            Case TokenType.RETURN_ 'return 语句
                Return parseReturnStatement() '解析return语句
            Case TokenType.FOR_
                Return parseForStatement()
            Case TokenType.WHILE_
                Return parseWhileStatement()
            Case TokenType.CLASS_
                Return parseClassStatement()
            Case TokenType.EOL 'EOL 没啥大用
                Return New EOLStatement With {.Token = curToken}
            Case Else '都不是
                Return parseExpressionStatement() '解析表达式语句
        End Select
    End Function

    '获取所有错误 返回一个 List (Of String)的错误列表
    Public Function GetErrors()
        Return errors
    End Function

    Public Sub noPrefixParseFnError(tkn As TokenType)
        Dim msg = $"no prefix parse Function For {tkn.ToString} found"
        'Console.WriteLine(msg)
    End Sub

    '探查错误
    Public Sub peekError(tkn As TokenType)
        Dim msg_cn = $"错误！ 下一个Token应为 {tkn}, 但却是 {peekToken.TokenType},{vbCrLf}内容:{peekToken.Value.ToString}"
        errors.Add(msg_cn)
    End Sub

    Public Sub curError(tkn As TokenType)
        Dim msg_cn = $"错误！ 当前Token应为 {tkn}, 但却是 {curToken.TokenType},{vbCrLf}内容:{curToken.Value.ToString}"
        errors.Add(msg_cn)
    End Sub


    Public Function checkParserErrors() As Boolean

        If errors.Count = 0 Then
            Return False '无错误 返回False
        End If
        Console.WriteLine($"parser has {errors.Count} errors")
        For Each err As String In errors
            Console.WriteLine($"parser error:{err}")
        Next

        '有错误，返回True
        Return True
    End Function

    '解析表达式语句
    Public Function parseExpressionStatement() As ExpressionStatement
        Dim stmt = New ExpressionStatement() With {.Token = curToken}
        stmt.Expression = parseExpression(优先级.LOWEST)

        If peekTokenIs(TokenType.EOL) Then
            nextToken()
        End If

        Return stmt
    End Function


    '...
    Public Function parseExpression(precedence As Integer) As Expression
        If Not prefixParseFns.ContainsKey(curToken.TokenType) Then
            noPrefixParseFnError(curToken.TokenType)
            Return Nothing
        End If
        Dim prefix = prefixParseFns(curToken.TokenType)
        Dim leftExp = prefix()


        While Not peekTokenIs(TokenType.EOL) AndAlso peekPrecedence() > precedence
            Dim infix = infixParseFns(peekToken.TokenType)

            If infix Is Nothing Then
                Return leftExp
            End If

            nextToken()

            leftExp = infix(leftExp)
        End While

        Return leftExp

    End Function

    '解析Dim语句 返回一个DimStatement类型的对象
    Public Function parseDimStatement() As DimStatement
        '初始化
        Dim stmt = New DimStatement With {
            .Token = curToken
        }

        Dim IsReadOnly = False

        If peekTokenIs(TokenType.READONLY_) Then
            IsReadOnly = True
            nextToken()
        End If

        '检查TokenType是否为Ident （标识符）
        If Not expectPeek(TokenType.IDENT) Then
            Return Nothing '返回空
        End If

        '设置变量名为 一个Identifier类型的对象
        stmt.Name = New Identifier With {.Token = curToken, .Value = curToken.Value, .IsReadOnly = IsReadOnly}

        '检查TokenType是否为ASSIGN （等于号）
        If peekTokenIs(TokenType.ASSIGN) Then
            nextToken()
            nextToken()

            ' 解析赋值表达式的右侧部分
            stmt.Value = parseExpression(优先级.LOWEST)

        Else
            stmt.Value = New CallExpression With {
                .Func = New Identifier With {.Value = "CNothing"},
                .Arguments = New List(Of Expression) From {
                    New IntegerLiteral With {.Value = New BigInteger(0)}
                },
                .Token = curToken
            }

            nextToken()
        End If

        ' 检查是否以换行符结束声明
        If peekTokenIs(TokenType.EOL) Then
            nextToken()
        End If

        Return stmt
    End Function

    '解析Return语句 返回一个 ReturnStatement 类型的对象
    Public Function parseReturnStatement() As ReturnStatement
        '初始化
        Dim stmt = New ReturnStatement With {
            .Token = curToken
        }

        nextToken() '继续下一个Token

        '解析表达式并设置为返回值
        stmt.ReturnValue = parseExpression(优先级.LOWEST)

        '如果下一个TokenType 为 EOL (End Of Line)
        If peekTokenIs(TokenType.EOL) Then
            nextToken() '继续下一个Token
        End If

        Return stmt
    End Function

    '检查当前Token的TokenType是否符合实参TokenType
    Public Function curTokenIs(t As TokenType) As Boolean
        Debug.WriteLine(curToken.TokenType.ToString)
        Return curToken.TokenType = t
    End Function

    '检查下一个Token的TokenType是否符合实参TokenType
    Public Function peekTokenIs(t As TokenType) As Boolean
        Return peekToken.TokenType = t
    End Function

    '懒得写了
    Public Function expectPeek(t As TokenType) As Boolean
        If peekTokenIs(t) Then
            nextToken()
            Return True
        Else
            peekError(t)
            Return False
        End If
    End Function



    Public Function expectCur(t As TokenType) As Boolean
        If curTokenIs(t) Then
            Return True
        Else
            curError(t)
            Return False
        End If
    End Function

End Class