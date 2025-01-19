Imports System.IO
Imports System.Numerics
Imports System.Collections.Concurrent

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
    ' 错误处理相关
    Private Class ParseError
        Public Enum ErrorLevel
            Warning
            [Error]
            Critical
        End Enum

        Public Property Code As String
        Public Property Message As String
        Public Property Level As ErrorLevel
        Public Property LineNumber As Integer
        Public Property Context As String

        Public Sub New(code As String, message As String, level As ErrorLevel, lineNumber As Integer, context As String)
            Me.Code = code
            Me.Message = message
            Me.Level = level
            Me.LineNumber = lineNumber
            Me.Context = context
        End Sub
    End Class

    Private ReadOnly errors As New List(Of ParseError)
    Private Sub AddError(code As String, message As String, level As ParseError.ErrorLevel)
        AddError(New ParseError(
            code,
            message,
            level,
            curToken.Line,
            $"Current token: {curToken}, Next token: {peekToken}"
        ))
    End Sub

    ' 其他成员变量
    Public 优先级字典 As New Dictionary(Of TokenType, 优先级) From {
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
    Private curToken As Token
    Private peekToken As Token
    Private tknPos As Integer = 0
    Private ReadOnly tokens As List(Of Token)

    Public Delegate Function PrefixParseFunction() As Expression
    Public Delegate Function InfixParseFunction(left As Expression) As Expression

    ' 声明委托字段
    Private ReadOnly prefixParseFns As New Dictionary(Of TokenType, PrefixParseFunction)
    Private ReadOnly infixParseFns As New Dictionary(Of TokenType, InfixParseFunction)
    Private ReadOnly CountDictionary As New ConcurrentDictionary(Of Type, Integer) From {
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

        InitializeParseFunctions()
        tokens = l.Lexer
    End Sub

    Private Sub InitializeParseFunctions()
        ' 前缀解析函数
        prefixParseFns.Add(TokenType.无意义, AddressOf 无意义)
        prefixParseFns.Add(TokenType.ILLEGAL, AddressOf ErrorChar)
        prefixParseFns.Add(TokenType.ENDFUNC, AddressOf 无意义)
        prefixParseFns.Add(TokenType.ENDIF_, AddressOf 无意义)
        prefixParseFns.Add(TokenType.IN_, AddressOf 无意义)
        prefixParseFns.Add(TokenType.SLASH, AddressOf ParseError_Token_SLASH)
        prefixParseFns.Add(TokenType.PLUS, AddressOf ParseError_Token_PLUS)
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

        ' 中缀解析函数
        infixParseFns.Add(TokenType.PLUS, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.MINUS, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.SLASH, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.ASTERISK, AddressOf ParseInfixExpression)
        infixParseFns.Add(TokenType.EQ, AddressOf ParseInfixExpression)
    End Sub

    Private Function ParseCommonExpression(endToken As TokenType) As Expression
        Dim exp = ParseExpression(优先级.LOWEST)
        If Not ExpectPeek(endToken) Then
            Return Nothing
        End If
        Return exp
    End Function

    Private Sub SkipTokens(Optional count As Integer = 1)
        For i = 1 To count
            NextToken()
        Next
    End Sub

    Public Function ParseReturnStatement() As ReturnStatement
        Dim stmt = New ReturnStatement With {.Token = curToken}
        NextToken()
        stmt.ReturnValue = ParseExpression(优先级.LOWEST)
        If PeekTokenIs(TokenType.EOL) Then NextToken()
        Return stmt
    End Function

    Public Function ExpectPeek(t As TokenType, Optional customMessage As String = Nothing) As Boolean
        If PeekTokenIs(t) Then
            NextToken()
            Return True
        End If
        
        If customMessage IsNot Nothing Then
            AddError("EXPECTED_TOKEN", customMessage, ParseError.ErrorLevel.Error)
        Else
            PeekError(t)
        End If
        Return False
    End Function

    '解析if表达式  
    Public Function ParseIfExpression() As Expression
        Dim ifExp = New IfExpression With {.Token = curToken}
        NextToken()
        ifExp.Condition = ParseExpression(优先级.LOWEST)
        
        If Not ExpectPeek(TokenType.THEN_) Then
            AddError("MISSING_THEN", "缺少THEN关键字", ParseError.ErrorLevel.Error)
            Return Nothing
        End If
        
        ifExp.Consequence = ParseBlockStatement(GetType(IfExpression))
        
        If PeekTokenIs(TokenType.ELSE_) Then
            NextToken()
            ifExp.Alternative = ParseBlockStatement(GetType(IfExpression))
        End If
        
        Return ifExp
    End Function

    ' 修改后的ParseBlockStatement
    Public Function ParseBlockStatement(blockType As Type) As BlockStatement
        Dim block = New BlockStatement With {.Token = curToken}
        NextToken()
            '解析语句
        
        While Not CurTokenIs(TokenType.ENDIF_) AndAlso Not CurTokenIs(TokenType.ENDFUNC)
            Dim stmt = ParseStatement()
            If stmt IsNot Nothing Then '判空
                block.Statements.Add(stmt) '添加stmt至列表
            If stmt IsNot Nothing Then
                block.Statements.Add(stmt)
            End If
            NextToken()
        End While
        
        Return block
    End Function


    ' 解析分组表达式
    Public Function ParseGroupedExpression() As Expression
        NextToken()
        Return ParseCommonExpression(TokenType.RPAREN)
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
            AddError($"缺少文件路径 在第{curToken.Line}行")
            Return Nothing
        End If

        fileImportExp.FilePath = ParseExpression(优先级.LOWEST)

        NextToken()
        NextToken()

        If CurTokenIs(TokenType.EOL) Then
            AddError($"缺少别名 在第{curToken.Line}行")
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
                    AddError(message)

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
            AddError(message)

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
            AddError(msg_cn)
        Else
            Dim msg_cn = $"错误！ 下一个词法单元类型应为 {tkn}, 但却是 {peekToken.TokenType},{vbCrLf}内容:{peekToken.Value}"
            AddError(msg_cn)
        End If
    End Sub

    Public Sub CurError(tkn As TokenType)
        If Token.StringDict.ContainsKey(tkn) AndAlso Token.StringDict.ContainsKey(curToken.TokenType) Then
            Dim msg_cn = $"错误！ 当前词法单元类型应为 {Token.StringDict(tkn)}, 但却是 {Token.StringDict(curToken.TokenType)},{vbCrLf}内容:{curToken.Value}"
            AddError(msg_cn)
        Else
            Dim msg_cn = $"错误！ 当前词法单元类型应为 {tkn}, 但却是 {curToken.TokenType},{vbCrLf}内容:{curToken.Value}"
            AddError(msg_cn)
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
            AddError(custom_message)
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
            AddError(custom_message)
            Return False
        End If
    End Function
End Class