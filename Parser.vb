Imports System.IO
Imports System.Numerics

Public Enum 优先级
    无意义
    LOWEST
    ASSIGNMENT
    AND_OR      '// Or | And
    EQUALS      '//== 
    LESSGREATER '// > | <
    SUM         '// + 
    PRODUCT    ' // * 
    PREFIX     ' // -X | !X 
    CALL_       ' // myFunction(X)
    INDEX '// array[index] 
    OBJ_MEMBER ' person.Name    
End Enum

Public Class Parser
    Public 优先级字典 As New Dictionary(Of TokenType, 优先级) From
    {
        {TokenType.EQ, 优先级.EQUALS},
        {TokenType.ASSIGN, 优先级.ASSIGNMENT},
        {TokenType.DOT, 优先级.OBJ_MEMBER},
        {TokenType.NOT_EQ, 优先级.EQUALS},
        {TokenType.LT, 优先级.LESSGREATER},
        {TokenType.BOOL_AND, 优先级.AND_OR},
        {TokenType.BOOL_OR, 优先级.AND_OR},
        {TokenType.GT, 优先级.LESSGREATER},
        {TokenType.PLUS, 优先级.SUM},
        {TokenType.MINUS, 优先级.SUM},
        {TokenType.SLASH, 优先级.PRODUCT},
        {TokenType.ASTERISK, 优先级.PRODUCT},
        {TokenType.LPAREN, 优先级.CALL_},
        {TokenType.LBRACKET, 优先级.INDEX}
    }


    Private ReadOnly l As Lexer
    Dim curToken As Token
    Dim peekToken As Token
    Dim tknPos As Integer = 0
    Private ReadOnly tokens As List(Of Token)
    Private ReadOnly errors As New List(Of String)

    Public Delegate Function PrefixParseFunction() As Expression
    Public Delegate Function InfixParseFunction(left As Expression) As Expression

    ' 声明委托字段
    Private ReadOnly prefixParseFns As New Dictionary(Of TokenType, PrefixParseFunction)
    Private ReadOnly infixParseFns As New Dictionary(Of TokenType, InfixParseFunction)

    Private CountDictionary As New Dictionary(Of Type, Object) From {
        {GetType(IfExpression), 0},
        {GetType(ForStatement), 0},
        {GetType(WhileStatement), 0},
        {GetType(ClassStatement), 0},
        {GetType(TryCatchStatement), 0},
        {GetType(FunctionLiteral), 0}
    }

    Public Sub New(lexer As Lexer)
        l = lexer
        '初始化
        curToken = New Token(TokenType.无意义, vbNullChar, -1)
        peekToken = New Token(TokenType.无意义, vbNullChar, -1)

        prefixParseFns.Add(TokenType.无意义, AddressOf 无意义)
        prefixParseFns.Add(TokenType.ILLEGAL, AddressOf ErrorChar)
        infixParseFns.Add(TokenType.无意义, AddressOf 无意义)
        infixParseFns.Add(TokenType.ILLEGAL, AddressOf ErrorChar)

        prefixParseFns.Add(TokenType.ENDFUNC, AddressOf 无意义)
        prefixParseFns.Add(TokenType.ENDIF_, AddressOf 无意义)
        prefixParseFns.Add(TokenType.IN_, AddressOf 无意义)

        prefixParseFns.Add(TokenType.SLASH, AddressOf ParseError_Token_SLASH)
        prefixParseFns.Add(TokenType.PLUS, AddressOf ParseError_Token_PLUS)


        '获取所有Token
        tokens = l.Lexer

        '注册前缀符
        prefixParseFns.Add(TokenType.THROW_, AddressOf ParseThrowErrorExpression)
        prefixParseFns.Add(TokenType.SingleQuote, AddressOf ParseComment)
        prefixParseFns.Add(TokenType.IMPORT, AddressOf ParseModuleImpprtExpression)
        prefixParseFns.Add(TokenType.FROM, AddressOf ParseFromModuleImpprtExpression)
        prefixParseFns.Add(TokenType.IDENT, AddressOf ParseIdentifier)
        prefixParseFns.Add(TokenType.INCLUDE, AddressOf ParseFileImpprtExpression)
        prefixParseFns.Add(TokenType.NEW_, AddressOf ParseObjectCreateExpression)
        prefixParseFns.Add(TokenType.LBRACE, AddressOf ParseDictionaryLiteral)
        prefixParseFns.Add(TokenType.LBRACKET, AddressOf ParseArrayLiteral)
        prefixParseFns.Add(TokenType.IF_, AddressOf ParseIfExpression)
        prefixParseFns.Add(TokenType.BOOL_NOT, AddressOf ParseNotExpression)
        prefixParseFns.Add(TokenType.INTNUMBER, AddressOf ParseNumberLiteral)
        prefixParseFns.Add(TokenType.MINUS, AddressOf ParsePrefixExpression)
        prefixParseFns.Add(TokenType.BANG, AddressOf ParsePrefixExpression)
        prefixParseFns.Add(TokenType.BOOL_TRUE, AddressOf ParseBoolean)
        prefixParseFns.Add(TokenType.BOOL_FALSE, AddressOf ParseBoolean)
        prefixParseFns.Add(TokenType.LPAREN, AddressOf ParseGroupedExpression)
        prefixParseFns.Add(TokenType.FUNC, AddressOf ParseFunctionLiteral)
        prefixParseFns.Add(TokenType.STRING_, AddressOf ParseStringLiteral)

        '注册中缀符
        infixParseFns.Add(TokenType.PLUS, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.MINUS, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.SLASH, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.ASTERISK, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.EQ, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.NOT_EQ, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.LT, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.GT, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.LPAREN, AddressOf ParseCallExpression)
        infixParseFns.Add(TokenType.SingleQuote, AddressOf ParseComment)
        infixParseFns.Add(TokenType.LBRACKET, AddressOf ParseIndexExpression)
        infixParseFns.Add(TokenType.BOOL_AND, AddressOf ParseLogicalExpression)
        infixParseFns.Add(TokenType.BOOL_OR, AddressOf ParseLogicalExpression)
        infixParseFns.Add(TokenType.DOT, AddressOf ParseObjectMemberExpression)
        infixParseFns.Add(TokenType.ASSIGN, AddressOf ParseAssignmentExpression)
    End Sub

    Public Function ParseError_Token_RBRACKET()
        errors.Add($"缺少""["" 在 第{curToken.Line}行")
        Return Nothing
    End Function
    Public Function ParseError_Token_SLASH()
        errors.Add($"错误的 ""/"" 在 第{curToken.Line}行")
        Return Nothing
    End Function

    Public Function ParseError_Token_PLUS()
        errors.Add($"错误的 ""+"" 在 第{curToken.Line}行")
        Return Nothing
    End Function
    Public Function ParseError_Token_ASTERISK()
        errors.Add($"错误的 ""*"" 在 第{curToken.Line}行")
        Return Nothing
    End Function

    '解析引发异常表达式
    Public Function ParseTryCatchStatement() As Statement
        Dim tryCatchStmt = New TryCatchStatement With {.Token = curToken}
        NextToken()

        If Not ExpectCur(TokenType.EOL) Then
            Return Nothing
        End If

        NextToken()
        tryCatchStmt.TryBlock = ParseBlockStatement(tryCatchStmt.GetType)

        NextToken()
        tryCatchStmt.CatchVar = ParseExpression(优先级.LOWEST)

        NextToken()
        NextToken()
        tryCatchStmt.CatchClass = ParseExpression(优先级.LOWEST)

        NextToken()
        If Not ExpectCur(TokenType.EOL) Then
            Return Nothing
        End If

        NextToken()
        tryCatchStmt.CatchBlock = ParseBlockStatement(tryCatchStmt.GetType)

        If Not ExpectCur(TokenType.ENDTRY) Then
            Return Nothing
        End If

        Return tryCatchStmt
    End Function

    '解析引发异常表达式
    Public Function ParseThrowErrorExpression() As Expression
        Dim throwErrorExp = New ThrowErrorExpression With {.Token = curToken}
        NextToken()

        throwErrorExp.ErrorObject = ParseExpression(优先级.LOWEST)
        Return throwErrorExp
    End Function

    '解析模块导入表达式
    Public Function ParseModuleImpprtExpression() As Expression
        Dim moduleImportExp = New ModuleImportExpression With {.Token = curToken}

        NextToken()
        If CurTokenIs(TokenType.EOL) Then
            errors.Add($"缺少模块名 在第{curToken.Line}行")
            Return Nothing
        End If

        If CurTokenIs(TokenType.DOT) Then
            moduleImportExp.ModuleName = New Identifier(".")
        Else
            moduleImportExp.ModuleName = ParseExpression(优先级.LOWEST)
        End If

        NextToken()
        If Not CurTokenIs(TokenType.AS_) Then
            Return moduleImportExp
        End If

        NextToken()
        moduleImportExp.AliasName = ParseExpression(优先级.LOWEST)

        If moduleImportExp.AliasName Is Nothing Then
            errors.Add($"缺少别名 在第{curToken.Line}行")
            Return Nothing
        End If

        Return moduleImportExp
    End Function

    '解析模块导入表达式 | From ... Import ...
    Public Function ParseFromModuleImpprtExpression() As Expression
        Dim fromModuleImportExp = New FromModuleImportExpression With {.Token = curToken}

        NextToken()
        If CurTokenIs(TokenType.EOL) Then
            errors.Add($"缺少模块名 在第{curToken.Line}行")
            Return Nothing
        End If


        If CurTokenIs(TokenType.DOT) Then
            fromModuleImportExp.ModuleName = New Identifier(".")
        Else
            fromModuleImportExp.ModuleName = ParseExpression(优先级.LOWEST)
        End If

        NextToken()
        If Not ExpectCur(TokenType.IMPORT) Then
            Return Nothing
        End If

        NextToken()
        fromModuleImportExp.ImportItem = ParseExpression(优先级.LOWEST)

        If CurTokenIs(TokenType.ASTERISK) Then
            fromModuleImportExp.ImportItem = New Identifier With {.Token = curToken, .Value = curToken.Value}
            Return fromModuleImportExp
        End If

        If fromModuleImportExp.ImportItem Is Nothing Then
            errors.Add($"缺少导入项 在第{curToken.Line}行")
            Return Nothing
        End If

        Return fromModuleImportExp
    End Function


    '解析注释
    Public Function ParseComment() As Expression
        Dim comment = New Comment With {
            .Token = curToken
        }

        While Not (curToken.Value <> vbCr OrElse curToken.Value <> vbLf OrElse curToken.Value <> vbCrLf)
            NextToken()
        End While

        NextToken()
        Return comment
    End Function


    '解析while语句
    Public Function ParseWhileStatement() As Statement
        '初始化
        Dim stmt = New WhileStatement With {.Token = curToken}

        '下一个Token
        NextToken()

        '解析表达式设置为循环条件
        stmt.LoopCondition = ParseExpression(优先级.LOWEST)

        If Not ExpectPeek(TokenType.EOL) Then
            Return Nothing
        End If

        '下一个Token
        NextToken()

        '解析代码块语句并设置为 循环块
        stmt.LoopBlock = ParseBlockStatement(stmt.GetType)

        If Not ExpectCur(TokenType.ENDWHILE) Then
            Return Nothing
        End If

        Return stmt
    End Function

    '解析class语句
    Public Function ParseClassStatement() As Statement
        '初始化
        Dim stmt = New ClassStatement With {.Token = curToken}

        '下一个Token
        NextToken()

        '解析表达式设置为类名
        stmt.Name = ParseExpression(优先级.LOWEST)
        If PeekTokenIs(TokenType.COLON) Then
            NextToken()
            NextToken()
            Dim baseClassName = ParseExpression(优先级.LOWEST)
            If baseClassName Is Nothing Then
                ExpectCur(-1, $"冒号（:）后面应为表达式")
                Return Nothing
            End If

            stmt.BaseClass = baseClassName
        End If

        If Not ExpectPeek(TokenType.EOL) Then
            Return Nothing
        End If

        '下一个Token
        NextToken()

        '解析代码块语句并设置为类的块
        stmt.Body = ParseBlockStatement(stmt.GetType)

        If Not ExpectCur(TokenType.ENDCLASS) Then
            Return Nothing
        End If

        Return stmt
    End Function


    '解析for语句
    Public Function ParseForStatement() As Statement
        '初始化
        Dim stmt = New ForStatement With {.Token = curToken}

        '下一个Token
        NextToken()

        If Not ExpectCur(TokenType.IDENT) Then
            Return Nothing
        End If

        '解析表达式设置为迭代变量
        stmt.ItemVar = ParseExpression(优先级.LOWEST)

        If Not ExpectPeek(TokenType.IN_) Then
            Return Nothing
        End If

        '下一个Token
        NextToken()

        '解析表达式设置为 待遍历的列表
        stmt.Items = ParseExpression(优先级.LOWEST)


        If Not ExpectPeek(TokenType.EOL) Then
            Return Nothing
        End If

        '解析代码块语句并设置为 循环块
        stmt.LoopBlock = ParseBlockStatement(stmt.GetType)

        If Not ExpectCur(TokenType.NEXT_) Then
            Return Nothing
        End If

        Return stmt
    End Function


    Public Function ParseDictionaryLiteral() As Expression
        Dim dict = New DictionaryLiteral With {
            .Token = curToken,
            .Pairs = New Dictionary(Of Expression, Expression)
        }

        While Not PeekTokenIs(TokenType.RBRACE)
            NextToken()
            Dim key = ParseExpression(优先级.LOWEST)

            If Not ExpectPeek(TokenType.COLON) Then
                Return Nothing
            End If

            NextToken()
            Dim value = ParseExpression(优先级.LOWEST)

            dict.Pairs(key) = value
            If Not PeekTokenIs(TokenType.RBRACE) AndAlso Not ExpectPeek(TokenType.COMMA) Then
                Return Nothing
            End If
        End While

        NextToken()
        Return dict
    End Function

    Public Function ParseIndexExpression(left As Expression) As Expression
        Dim exp = New IndexExpression With {.Token = curToken, .Left = left}
        NextToken()
        exp.Index = ParseExpression(优先级.LOWEST)

        If PeekTokenIs(TokenType.COLON) Then
            Dim slice_exp = New SliceExpression With {.StartIndex = exp.Index, .Token = curToken}

            Return ParseSliceExpression(left, slice_exp)
        ElseIf CurTokenIs(TokenType.COLON) Then
            Dim slice_exp = New SliceExpression With {.StartIndex = exp.Index, .Token = curToken}
            If exp.Index Is Nothing Then
                slice_exp.StartIndex = New IntegerLiteral With {.Token = curToken, .Value = 0}
            End If

            BackToken()
            Return ParseSliceExpression(left, slice_exp)
        End If

        If Not ExpectPeek(TokenType.RBRACKET) Then
            Return Nothing
        End If

        Return exp
    End Function

    Public Function ParseSliceExpression(left As Expression, exp As SliceExpression) As Expression
        exp.Left = left

        NextToken()
        NextToken()
        If CurTokenIs(TokenType.RBRACKET) Then
            exp.StopIndex = Nothing
            Return exp
        End If

        exp.StopIndex = ParseExpression(优先级.LOWEST)

        NextToken()
        If CurTokenIs(TokenType.COLON) Then
            NextToken()
            exp.IndexStep = ParseExpression(优先级.LOWEST)
            NextToken()
        End If

        If Not ExpectCur(TokenType.RBRACKET) Then
            Return Nothing
        End If

        Return exp
    End Function

    Public Function ParseArrayLiteral() As Expression
        Dim array = New ArrayLiteral With {
            .Token = curToken,
            .Elements = ParseExpressionList(TokenType.RBRACKET)
        }
        Return array
    End Function


    Public Function ParseExpressionList(end_ As TokenType) As List(Of Expression)
        Dim list = New List(Of Expression)

        If CurTokenIs(end_) Then
            Return list
        End If

        If PeekTokenIs(end_) Then
            NextToken()
            Return list
        End If

        NextToken()
        list.Add(ParseExpression(优先级.LOWEST))

        While PeekTokenIs(TokenType.COMMA)
            NextToken()
            NextToken()
            list.Add(ParseExpression(优先级.LOWEST))
        End While

        If Not CurTokenIs(end_) Then
            If PeekTokenIs(end_) Then
                NextToken()
                Return list
            End If
            ExpectCur(end_)
            Return Nothing
        End If

        NextToken()
        Return list
    End Function


    Public Function ParseStringLiteral() As Expression
        Dim exp = New StringLiteral With {.Token = curToken, .Value = curToken.Value}
        Return exp
    End Function

    '解析函数调用表达式
    Public Function ParseCallExpression(func As Expression) As Expression
        '创建一个函数调用表达式
        Dim exp = New CallExpression With {.Token = curToken, .Func = func, .Arguments = ParseExpressionList(TokenType.RPAREN)}
        Return exp
    End Function

    '没啥意义
    Public Function 无意义()
        Return Nothing
    End Function

    Public Function ErrorChar()
        If Char.IsWhiteSpace(curToken.Value) Then Return Nothing
        errors.Add($"无效的字符 ""{curToken.Value}""")

        Return Nothing
    End Function


    '解析调用参数
    Public Function ParseCallArguments() As List(Of Expression)
        Dim args = New List(Of Expression)

        '判断下一个词法单元是否为 右括号
        If PeekTokenIs(TokenType.RPAREN) Then
            '如果是 那么跳转至下一个Token
            NextToken()

            '返回参数列表
            Return args
        End If

        '跳转到下一个词法单元
        NextToken()

        '将解析到的表达式添加到实参列表
        args.Add(ParseExpression(优先级.LOWEST))

        '如果下一个词法单元为逗号 "," 重复执行
        While PeekTokenIs(TokenType.COMMA)
            '跳转词法单元
            NextToken()
            NextToken()

            '将解析到的表达式添加到实参列表
            args.Add(ParseExpression(优先级.LOWEST))
        End While

        If Not ExpectPeek(TokenType.RPAREN) Then
            Return Nothing
        End If

        Return args
    End Function

    '解析if表达式  
    Public Function ParseIfExpression() As Expression
        Dim Expression = New IfExpression With {.Token = curToken}

        Try
            '初始化

            '下一个Token
            NextToken()

            '解析表达式并设置为条件
            Expression.Condition = ParseExpression(优先级.LOWEST)

            If Not ExpectPeek(TokenType.THEN_, $"缺少 Then") OrElse Not ExpectCur(TokenType.THEN_, $"缺少 Then") Then
                Throw New Fail
            End If

            '解析代码块语句并设置为 默认的块（if条件的块）
            Expression.Consequence = ParseBlockStatement(Expression.GetType)

            '如果当前Token是ElseIf
            If CurTokenIs(TokenType.ELSEIF_) Then
                '解析代码块语句并设置为ElseIf条件的块

                Expression.ElseIf_List = New List(Of ElseIfExpression)
                While Not CurTokenIs(TokenType.THEN_) AndAlso Not CurTokenIs(TokenType.ELSE_) AndAlso Not CurTokenIs(TokenType.ENDIF_) AndAlso Not CurTokenIs(TokenType.EOF)
                    Dim elseif_exp = ParseElseIfExpression()
                    Expression.ElseIf_List.Add(elseif_exp)
                End While
            End If

            '如果当前Token是Else
            If CurTokenIs(TokenType.ELSE_) Then
                '解析代码块语句并设置为Else条件的块
                Expression.Alternative = ParseBlockStatement(Expression.GetType)
            End If

            If Not ExpectCur(TokenType.ENDIF_, $"缺少 Endif") Then
                Throw New Fail
            End If

            CountDictionary(Expression.GetType) += 1
            Return Expression
        Catch ex As Fail
            CountDictionary(Expression.GetType) += 1
            Return Nothing
        End Try
    End Function

    '解析not表达式  
    Public Function ParseNotExpression() As Expression
        '初始化
        Dim Expression = New NotExpression With {.Token = curToken}

        '下一个Token
        NextToken()

        '解析表达式并设置为右侧表达式
        Expression.Right = ParseExpression(优先级.LOWEST)
        Return Expression
    End Function


    '解析elseif表达式  
    Public Function ParseElseIfExpression() As Expression
        '初始化
        Dim Expression = New ElseIfExpression With {.Token = curToken}

        '下一个Token
        NextToken()

        '解析表达式并设置为条件
        Expression.Condition = ParseExpression(优先级.LOWEST)

        Dim No_THEN_Message = $"缺少 Then"
        If Not ExpectPeek(TokenType.THEN_, No_THEN_Message) AndAlso Not ExpectCur(TokenType.THEN_, No_THEN_Message) Then
            Return Nothing
        End If

        '解析代码块语句并设置为 默认的块（elseif条件的块）
        Expression.Consequence = ParseBlockStatement(Expression.GetType)

        Return Expression
    End Function

    '解析函数头
    Public Function ParseFunctionLiteral() As Expression
        '初始化
        Dim lit = New FunctionLiteral With {.Token = curToken}

        '下一个Token
        NextToken()

        lit.Name = New Identifier() With {.Token = curToken, .Value = curToken.Value}

        '解析函数实参并设置
        lit.Parameters = ParseFunctionParameters()

        '解析代码块并设置为函数的代码块
        lit.Body = ParseBlockStatement(lit.GetType)
        Return lit
    End Function

    '解析函数实参
    Public Function ParseFunctionParameters() As List(Of Identifier)
        '初始化
        Dim identifiers As New List(Of Identifier)

        NextToken()

        '如果下一个Token是右括号
        If PeekTokenIs(TokenType.RPAREN) Then
            '下一个Token
            NextToken()

            '返回
            Return identifiers
        Else
            BackToken()
        End If


        '继续下一个Token
        NextToken()

        '新建Ident
        Dim ident = New Identifier With {.Token = peekToken, .Value = peekToken.Value}

        '添加至列表
        identifiers.Add(ident)

        '继续下一个Token
        NextToken()

        '重复执行直到下一个Token不是逗号
        While PeekTokenIs(TokenType.COMMA)
            '继续下一个Token
            NextToken()
            NextToken()

            '创建ident_并添加至列表
            Dim ident_ = New Identifier With {.Token = curToken, .Value = curToken.Value}
            identifiers.Add(ident_)
        End While

        NextToken()
        '如果不是右括号 
        If ExpectCur(TokenType.RPAREN) Then
            Return identifiers
        End If

        If ExpectCur(TokenType.RPAREN) Then
            Return identifiers
        End If

        Return Nothing '返回空
    End Function


    ' 检查开始标记和结束标记是否匹配
    Private Function IsMatchingEndToken(startTokenType As TokenType, endTokenType As TokenType) As Boolean
        Dim Start_End_TokenTypeDictionary As New Dictionary(Of TokenType, TokenType) From {
            {TokenType.IF_, TokenType.ENDIF_},
            {TokenType.WHILE_, TokenType.ENDWHILE},
            {TokenType.FUNC, TokenType.ENDFUNC},
            {TokenType.CLASS_, TokenType.ENDCLASS}
        }

        If Start_End_TokenTypeDictionary.ContainsKey(startTokenType) Then
            Return endTokenType = Start_End_TokenTypeDictionary(startTokenType)
        End If

        Return False
    End Function

    '解析代码块
    Public Function ParseBlockStatement(type As Type) As BlockStatement
        '初始化
        Dim block = New BlockStatement With {
            .Token = curToken,
            .Statements = New List(Of Statement)
        }

        '继续下一个Token
        NextToken()


        For i = 0 To CountDictionary(type)
            If CountDictionary(type) = 0 Then Continue For
            NextToken()
        Next

        While Not CurTokenIs(TokenType.CATCH_) AndAlso Not CurTokenIs(TokenType.ENDTRY) AndAlso Not CurTokenIs(TokenType.NEXT_) AndAlso Not CurTokenIs(TokenType.ENDWHILE) AndAlso Not CurTokenIs(TokenType.ENDFUNC) AndAlso Not CurTokenIs(TokenType.ENDIF_) AndAlso Not CurTokenIs(TokenType.ELSEIF_) AndAlso Not CurTokenIs(TokenType.ELSE_) AndAlso Not CurTokenIs(TokenType.ENDCLASS) AndAlso Not CurTokenIs(TokenType.EOF)
            '解析语句
            Dim stmt = ParseStatement()
            If stmt IsNot Nothing Then '判空
                block.Statements.Add(stmt) '添加stmt至列表
            End If

            '继续下一个Token
            NextToken()
        End While

        CountDictionary(type) -= 1

        Return block
    End Function

    '解析分组表达式
    Public Function ParseGroupedExpression() As Expression
        '继续下一个Token
        NextToken()

        '...
        Dim exp = ParseExpression(优先级.LOWEST)

        '检查下一个Token是否右括号
        If Not ExpectPeek(TokenType.RPAREN) Then
            Return Nothing
        End If
        Return exp
    End Function

    '解析中缀表达式
    Public Function ParseInfixExpression(left As Expression) As Expression
        '初始化
        Dim Expression = New InfixExpression With {
            .Token = curToken,
            .Operator_ = curToken.Value,
            .Left = left
        }
        '...
        Dim precedence = CurPrecedence()

        NextToken()

        If Expression.Operator_ = "+" Then
            precedence -= 1
        End If

        Expression.Right = ParseExpression(precedence)
        Return Expression
    End Function

    '...
    Public Function PeekPrecedence() As Integer
        If 优先级字典.ContainsKey(peekToken.TokenType) Then
            Return 优先级字典(peekToken.TokenType)
        End If

        Return 优先级.LOWEST
    End Function

    '...
    Public Function CurPrecedence() As Integer
        If 优先级字典.ContainsKey(curToken.TokenType) Then
            Return 优先级字典(curToken.TokenType)
        End If

        Return 优先级.LOWEST
    End Function


    '解析前缀表达式
    Public Function ParsePrefixExpression() As Expression
        Dim Expression = New PrefixExpression With {
            .Token = curToken,
            .Operator_ = curToken.Value
        }
        NextToken()
        Expression.Right = ParseExpression(优先级.PREFIX)
        Return Expression
    End Function


    '解析标识符
    Public Function ParseIdentifier() As Expression
        Dim ident = New Identifier With {.Token = curToken, .Value = curToken.Value}
        Return ident
    End Function


    '解析对象创建表达式
    Public Function ParseObjectCreateExpression() As Expression
        Dim objectCreateExpr = New ObjectCreateExpression With {.Token = curToken}

        NextToken()
        objectCreateExpr.ObjType = ParseExpression(优先级.OBJ_MEMBER - 1)

        NextToken()
        objectCreateExpr.Arguments = ParseExpressionList(TokenType.RPAREN)
        Return objectCreateExpr
    End Function

    '解析文件导入表达式
    Public Function ParseFileImpprtExpression() As Expression
        Dim fileImportExp = New FileImportExpression With {.Token = curToken}

        NextToken()
        If CurTokenIs(TokenType.EOL) Then
            errors.Add($"缺少文件路径 在第{curToken.Line}行")
            Return Nothing
        End If

        fileImportExp.FilePath = ParseExpression(优先级.LOWEST)

        NextToken()
        NextToken()

        If CurTokenIs(TokenType.EOL) Then
            errors.Add($"缺少别名 在第{curToken.Line}行")
            Return Nothing
        End If
        fileImportExp.AliasName = ParseExpression(优先级.LOWEST)

        Return fileImportExp
    End Function

    Public Function ParseObjectMemberExpression(leftExp As Expression) As Expression
        Dim exp As New ObjectMemberExpression With {
            .Token = curToken,
            .Left = leftExp
        }

        '下一个Token
        NextToken()

        exp.Right = ParseExpression(优先级.OBJ_MEMBER)

        Return exp
    End Function

    Public Function ParseAssignmentExpression(leftExp As Expression)
        If CurTokenIs(TokenType.ASSIGN) Then
            Dim exp As New AssignmentExpression With {.Token = curToken, .SetExp = leftExp}

            '下一个Token
            NextToken()

            '解析表达式设置为标识符的值
            exp.Value = ParseExpression(优先级.LOWEST)

            Return exp
        End If

        Return Nothing
    End Function

    Public Function ParseLogicalExpression(leftExp As Expression)
        If CurTokenIs(TokenType.BOOL_AND) Then
            Dim andExp As New AndExpression With {.Token = peekToken, .Left = leftExp}

            '前进词法单元
            NextToken()

            Dim rightExp = ParseExpression(优先级.LOWEST)
            andExp.Right = rightExp

            Return andExp
        End If

        If CurTokenIs(TokenType.BOOL_OR) Then
            Dim orExp As New OrExpression With {.Token = peekToken, .Left = leftExp}

            '前进词法单元
            NextToken()

            Dim rightExp = ParseExpression(优先级.LOWEST)
            orExp.Right = rightExp

            Return orExp
        End If

        Return Nothing
    End Function

    '解析Bool表达式
    Public Function ParseBoolean() As Expression
        Dim exp = New Bool With {.Token = curToken, .Value = CurTokenIs(TokenType.BOOL_TRUE)}
        Return exp
    End Function

    '解析整数表达式
    Public Function ParseNumberLiteral() As Expression
        Dim lit As Object = New IntegerLiteral With {.Token = curToken}
        lit.Token.Value = Trim(lit.Token.Value)

        If PeekTokenIs(TokenType.DOT) Then
            Dim dbl_lit = New DoubleLiteral With {.Token = curToken}
            NextToken()
            NextToken()

            If CurTokenIs(TokenType.INTNUMBER) Then
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

                Return dbl_lit
            End If
        End If

        Try
            If lit.Token.Value.StartsWith("&H") OrElse lit.Token.Value.StartsWith("&h") OrElse lit.Token.Value.StartsWith("0x") Then
                lit.Token.Value = Trim(lit.Token.Value.ToString.Substring(2))
                lit.Value = Convert.ToInt64(lit.Token.Value, 16)

                Return lit
            End If

            lit.Value = BigInteger.Parse(lit.Token.Value)
        Catch err As Exception
            Dim message = $"无法转换数值为整数, 数值: {CStr(curToken.Value)}"
            If NumberError.ErrorDictionary.ContainsKey(err.GetType) Then
                message = $"无法转换数值为整数, 因为""{NumberError.ErrorDictionary(err.GetType())}"" 数值: {CStr(curToken.Value)}"
            End If
            errors.Add(message)

            Return Nothing
        End Try
        Return lit
    End Function

    '...
    Public Sub NextToken()
        '若超过索引
        If tknPos >= (tokens.Count - 1) Then
            '设置TokenType 为 EOF
            curToken = New Token(TokenType.EOF, vbNullChar, -1)
            peekToken = New Token(TokenType.EOF, vbNullChar, -1)
        Else '否则

            '设置当前token为 tokens(tknPos)
            curToken = tokens(tknPos)
            peekToken = tokens(tknPos + 1)

            '索引 + 1
            tknPos += 1
        End If
    End Sub
    Public Sub BackToken()
        '请不要滥用此函数！！！

        '索引 - 1
        tknPos -= 1

        curToken = tokens(tknPos - 1)
        peekToken = tokens(tknPos)
    End Sub

    '解析程序 返回一个Program类型的对象
    Public Function ParseProgram() As Program
        '初始化
        Dim program As New Program With {
            .Statements = New List(Of Statement)
        }

        '重复执行直到curToken.TokenType = TokenType.EOF
        While curToken.TokenType <> TokenType.EOF
            '解析语句...
            Dim stmt = ParseStatement()
            If stmt IsNot Nothing Then '判空
                program.Statements.Add(stmt) '新增stmt至 program 的 Statements列表
            End If

            '继续下一个Token
            NextToken()
        End While

        Return program
    End Function

    '解析语句 返回一个 Statement 类型的 对象
    Public Function ParseStatement() As Statement
        Select Case curToken.TokenType
            Case TokenType.DIM_, TokenType.LET_ 'dim语句
                Return ParseDimStatement() '解析dim语句
            Case TokenType.RETURN_ 'return 语句
                Return ParseReturnStatement() '解析return语句
            Case TokenType.FOR_
                Return ParseForStatement()
            Case TokenType.WHILE_
                Return ParseWhileStatement()
            Case TokenType.CLASS_
                Return ParseClassStatement()
            Case TokenType.TRY_
                Return ParseTryCatchStatement()
            Case TokenType.EOL 'EOL 没啥大用
                Return New EOLStatement With {.Token = curToken}
            Case Else '都不是
                Return ParseExpressionStatement() '解析表达式语句
        End Select
    End Function

    '获取所有错误 返回一个 List (Of String)的错误列表
    Public Function GetErrors()
        Return errors
    End Function

    Public Sub NoPrefixParseFnError(tkn As TokenType)
        Dim msg = $"no prefix parse Function For {tkn} found"
        'Console.WriteLine(msg)
    End Sub

    '探查错误
    Public Sub PeekError(tkn As TokenType)
        If Token.StringDict.ContainsKey(tkn) AndAlso Token.StringDict.ContainsKey(peekToken.TokenType) Then
            Dim msg_cn = $"错误！ 下一个词法单元类型应为 {Token.StringDict(tkn)}, 但却是 {Token.StringDict(peekToken.TokenType)},{vbCrLf}内容:{peekToken.Value}"
            errors.Add(msg_cn)
        Else
            Dim msg_cn = $"错误！ 下一个词法单元类型应为 {tkn}, 但却是 {peekToken.TokenType},{vbCrLf}内容:{peekToken.Value}"
            errors.Add(msg_cn)
        End If
    End Sub

    Public Sub CurError(tkn As TokenType)
        If Token.StringDict.ContainsKey(tkn) AndAlso Token.StringDict.ContainsKey(curToken.TokenType) Then
            Dim msg_cn = $"错误！ 当前词法单元类型应为 {Token.StringDict(tkn)}, 但却是 {Token.StringDict(curToken.TokenType)},{vbCrLf}内容:{curToken.Value}"
            errors.Add(msg_cn)
        Else
            Dim msg_cn = $"错误！ 当前词法单元类型应为 {tkn}, 但却是 {curToken.TokenType},{vbCrLf}内容:{curToken.Value}"
            errors.Add(msg_cn)
        End If
    End Sub


    Public Function CheckParserErrors() As Boolean

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
    Public Function ParseExpressionStatement() As ExpressionStatement
        Dim stmt = New ExpressionStatement With {
            .Token = curToken,
            .Expression = ParseExpression(优先级.LOWEST)
        }

        If PeekTokenIs(TokenType.EOL) Then
            NextToken()
        End If

        Return stmt
    End Function


    '...
    Public Function ParseExpression(precedence As Integer) As Expression
        If Not prefixParseFns.ContainsKey(curToken.TokenType) Then
            NoPrefixParseFnError(curToken.TokenType)
            Return Nothing
        End If
        Dim prefix = prefixParseFns(curToken.TokenType)
        Dim leftExp = prefix()


        While Not PeekTokenIs(TokenType.EOL) AndAlso PeekPrecedence() > precedence
            Dim infix = infixParseFns(peekToken.TokenType)

            If infix Is Nothing Then
                Return leftExp
            End If

            NextToken()

            leftExp = infix(leftExp)
        End While

        Return leftExp
    End Function

    '解析Dim语句 返回一个DimStatement类型的对象
    Public Function ParseDimStatement() As DimStatement
        '初始化
        Dim stmt = New DimStatement With {
            .Token = curToken
        }


        If PeekTokenIs(TokenType.READONLY_) Then
            stmt.IsReadOnly = True
            NextToken()
        End If

        '检查TokenType是否为Ident （标识符）
        If Not ExpectPeek(TokenType.IDENT, "缺少标识符") Then
            Return Nothing '返回空
        End If

        '设置变量名为 一个Identifier类型的对象
        stmt.Name = New Identifier With {.Token = curToken, .Value = curToken.Value}

        '检查TokenType是否为ASSIGN （等于号）
        If PeekTokenIs(TokenType.ASSIGN) Then
            NextToken()
            NextToken()

            stmt.Value = ParseExpression(优先级.LOWEST)
        Else
            stmt.Value = New CallExpression With {
                .Func = New Identifier With {.Value = "CNothing"},
                .Arguments = New List(Of Expression) From {
                    New IntegerLiteral With {.Value = New BigInteger(0)}
                },
                .Token = curToken
            }

            NextToken()
        End If

        ' 检查是否以换行符结束声明
        If PeekTokenIs(TokenType.EOL) Then
            NextToken()
        End If

        Return stmt
    End Function

    '解析Return语句 返回一个 ReturnStatement 类型的对象
    Public Function ParseReturnStatement() As ReturnStatement
        '初始化
        Dim stmt = New ReturnStatement With {
            .Token = curToken
        }

        NextToken() '继续下一个Token

        '解析表达式并设置为返回值
        stmt.ReturnValue = ParseExpression(优先级.LOWEST)

        '如果下一个TokenType 为 EOL (End Of Line)
        If PeekTokenIs(TokenType.EOL) Then
            NextToken() '继续下一个Token
        End If

        Return stmt
    End Function

    '检查当前Token的TokenType是否符合实参TokenType
    Public Function CurTokenIs(t As TokenType) As Boolean
        Return curToken.TokenType = t
    End Function

    '检查下一个Token的TokenType是否符合实参TokenType
    Public Function PeekTokenIs(t As TokenType) As Boolean
        Return peekToken.TokenType = t
    End Function

    Public Function PeekTokenIs(t As TokenType， offset As Integer) As Boolean
        Return tokens(tknPos + 1 + offset).TokenType = t
    End Function


    '懒得写了
    Public Function ExpectPeek(t As TokenType) As Boolean
        If PeekTokenIs(t) Then
            NextToken()
            Return True
        Else
            PeekError(t)
            Return False
        End If
    End Function


    Public Function ExpectPeek(t As TokenType, custom_message As String) As Boolean
        If PeekTokenIs(t) Then
            NextToken()
            Return True
        Else
            errors.Add(custom_message)
            Return False
        End If
    End Function

    Public Function ExpectCur(t As TokenType) As Boolean
        If CurTokenIs(t) Then
            Return True
        Else
            CurError(t)
            Return False
        End If
    End Function

    Public Function ExpectCur(t As TokenType, custom_message As String) As Boolean
        If CurTokenIs(t) Then
            Return True
        Else
            errors.Add(custom_message)
            Return False
        End If
    End Function
End Class