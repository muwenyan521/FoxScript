Public Class Lexer
    Private ReadOnly _code As String ' 要分析的文本
    Private _pos As Integer = 0 ' 当前分析的位置
    Private _nextPos As Integer = 0 ' 下一个分析的位置
    Public _readingChar As Char  '当前读取的字符
    Private _line As Long = 1
    Private ReadOnly invaild_chars As New List(Of String)
    Private ReadOnly Letters() As Char = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".ToCharArray
    Private ReadOnly Numbers() As Char = "1234567890".ToCharArray

    ' Lexer构造函数，初始化文本和位置
    Public Sub New(text As String)
        '初始化
        _code = text
        _pos = 0

        Dim chars = Token.TokenTypeDict.Keys.Where(Function(item As String) item.Count = 1).ToList
        invaild_chars = chars

        '这里得readChar一下，不然报空异常就老实了
        ReadChar()

    End Sub

    Public Function Lexer() As List(Of Token)
        Dim tkns As New List(Of Token)

        '读取代码中所有Token.直到读取到文件末尾
        While _readingChar <> vbNullChar
            Dim tkn = GetNextToken()
            tkns.Add(tkn)
        End While
        tkns.Add(New Token(TokenType.EOF, vbNullChar, _line))

        Return tkns
    End Function

    Public Sub ReadChar()
        '索引 超过或等于 长度 那么读取的字符修改为 vbNullChar .上面的lexer函数遇到 vbNullChar会停下
        If _pos >= _code.Length OrElse _nextPos > _code.Length Then
            _readingChar = vbNullChar
        Else
            '当前索引 = 下一个的索引
            _pos = _nextPos
            '下一个的索引 + 1
            _nextPos += 1

            'PySubString是AI写的,vb.net原生的太麻烦了
            '将当前读取的字符修改为下一个字符
            _readingChar = PySubString(_code, _pos, _nextPos)

            If _readingChar = vbCrLf Then
                _line += 1 ' 当读取到换行符时，行号加1
            End If
        End If


    End Sub

    ' 获取下一个Token
    Public Function GetNextToken() As Token
        Dim tkn As New Token(TokenType.ILLEGAL, _readingChar, _line)

        '跳过空白字符
        SkipWhiteSpace()

        '经典的 Select Case 屎山，这里不多做解释
        Select Case _readingChar
            Case ";"c
                tkn = New Token(TokenType.SEMICOLON, _readingChar, _line)
            Case "+"c
                tkn = New Token(TokenType.PLUS, _readingChar, _line)
            Case "-"c


                tkn = New Token(TokenType.MINUS, _readingChar, _line)
            Case "*"c
                tkn = New Token(TokenType.ASTERISK, _readingChar, _line)
            Case "/"c
                tkn = New Token(TokenType.SLASH, _readingChar, _line)
            Case "="c
                '获取下一个字符
                Dim next_char = PeekChar()

                '如果下一个字符 等于 "=" 组成 ==
                If next_char = "=" Then
                    '新建 Token . 类型 TokenType.EQ 
                    tkn = New Token(GetIdentTokenType(_readingChar & next_char), _readingChar & next_char, _line)
                    ReadChar() ' 到下一个字符，即"="
                Else
                    tkn = New Token(TokenType.ASSIGN, _readingChar, _line)
                End If

            Case "!"c
                '获取下一个字符
                Dim next_char = PeekChar()

                '如果下一个字符 等于 "=" 组成 !=
                If next_char = "=" Then
                    '新建 Token . 类型 TokenType.NOT_EQ 
                    tkn = New Token(GetIdentTokenType(_readingChar & next_char), _readingChar & next_char, _line)
                    ReadChar() ' 到下一个字符，即"="
                Else
                    tkn = New Token(GetIdentTokenType(_readingChar), _readingChar, _line)
                End If
            Case "<"c
                ' 与上面感叹号的差不多，不做介绍

                Dim next_char = PeekChar()

                If next_char = ">" Then '组成  <> . （ "<>"是vb.net中的不等于号,作者喜欢这种,加了上去 ） 
                    tkn = New Token(GetIdentTokenType(_readingChar & next_char), _readingChar & next_char, _line)
                    ReadChar() ' 到下一个字符，即">"
                Else
                    tkn = New Token(TokenType.LT, _readingChar, _line)
                End If

            Case vbCrLf
                tkn = New Token(TokenType.EOL, vbCrLf, _line)
            Case ">"c
                tkn = New Token(TokenType.GT, _readingChar, _line)
            Case "("c
                tkn = New Token(TokenType.LPAREN, _readingChar, _line)
            Case ")"c
                tkn = New Token(TokenType.RPAREN, _readingChar, _line)
            Case ","c
                tkn = New Token(TokenType.COMMA, _readingChar, _line)
            Case "["c
                tkn = New Token(TokenType.LBRACKET, _readingChar, _line)
            Case "]"c
                tkn = New Token(TokenType.RBRACKET, _readingChar, _line)
            Case "{"c
                tkn = New Token(TokenType.LBRACE, _readingChar, _line)
            Case "}"c
                tkn = New Token(TokenType.RBRACE, _readingChar, _line)
            Case ":"c
                tkn = New Token(TokenType.COLON, _readingChar, _line)
            Case "."c
                tkn = New Token(TokenType.DOT, _readingChar, _line)
            Case """"
                tkn.TokenType = TokenType.STRING_
                tkn.Value = ReadString().Replace(vbCr, "")
            Case "'"
                tkn.TokenType = TokenType.SingleQuote
                tkn.Value = ReadNote.Replace(vbCr, "")
            Case Else
                '如果当前字符为数字
                If Char.IsDigit(_readingChar) Then
                    '读取
                    Dim num = ReadIntNumber()
                    tkn = New Token(TokenType.INTNUMBER, num, _line)
                    Return tkn
                End If

                '如果当前字符为字母
                If Letters.Contains(_readingChar) OrElse _readingChar = "_" Then
                    '读取 . Replace(vbCr,"")是为了把换行符去掉
                    Dim ident = ReadIdentifier().Replace(vbCr, "")
                    tkn = New Token(GetIdentTokenType(ident), ident, _line)
                    Return tkn
                End If

        End Select

        ReadChar()
        Return tkn
    End Function

    Public Function ReadNote() As String
        Dim p = _pos

        Dim exit_while = False
        While Not exit_while
            ReadChar()
            If _readingChar = "'" OrElse _readingChar = vbNullChar Then
                exit_while = True
            End If
        End While
        Return PySubString(_code, p + 1, _pos)
    End Function

    Public Sub CheckChar()
        If invaild_chars.Contains(_readingChar) Then
            _pos -= 1
        End If
    End Sub
    Public Function GetIdentTokenType(Ident As String) As TokenType
        '如果Ident在TokenTypeDict字典
        If Token.TokenTypeDict.ContainsKey(Ident.Replace(" ", "").Replace(vbCrLf, "").ToUpper) Then
            '返回对应TokenType
            Return Token.TokenTypeDict(Ident.Replace(" ", "").Replace(vbCrLf, "").ToUpper)
        End If
        '不在 那就返回 TokenType.Ident
        Return TokenType.IDENT
    End Function

    Public Sub SkipWhiteSpace()
        ' OrElse _readingChar = vbCr OrElse _readingChar = vbLf OrElse _readingChar = vbCrLf
        While _readingChar = " "c
            ReadChar()
        End While
    End Sub

    Public Function PeekChar() As Char
        Return PySubString(_code, _nextPos, _nextPos + 1)
    End Function

    Public Function ReadIntNumber() As String
        '记录当前索引
        Dim p = _pos

        '重复读取直到字符不是数字
        While Char.IsDigit(_readingChar) OrElse _readingChar = "&"c OrElse _readingChar = "H"c OrElse _readingChar = "x"
            ReadChar()
        End While

        '检查
        CheckChar()

        '裁剪
        Return PySubString(_code, p, _pos + 1)
    End Function
    Public Function ReadString() As String
        Dim p = _pos

        Dim exit_while = False
        While Not exit_while
            ReadChar()
            If _readingChar = """" OrElse _readingChar = vbCrLf Then
                exit_while = True
            End If
        End While
        Return PySubString(_code, p + 1, _pos)
    End Function

    Public Function IsChineseCharacter(charToCheck As Char) As Boolean
        Dim code As Integer = AscW(charToCheck)
        Return (code >= &H4E00 AndAlso code <= &H9FFF) OrElse
           (code >= &H3400 AndAlso code <= &H4DBF) OrElse
           (code >= &H20000 AndAlso code <= &H2A6DF) OrElse
           (code >= &H2A700 AndAlso code <= &H2B73F) OrElse
           (code >= &H2B740 AndAlso code <= &H2B81F) OrElse
           (code >= &H2B820 AndAlso code <= &H2CEAF) OrElse
           (code >= &H2CEB0 AndAlso code <= &H2EBEF) OrElse
           (code >= &H30000 AndAlso code <= &H3134F)
    End Function


    Public Function ReadIdentifier() As String
        '记录当前索引
        Dim p = _pos

        '坑爹vb.net (艹皿艹 )
        '重复读取直到字符不是字母 或者 下划线 
        While (Char.IsLetter(_readingChar) OrElse _readingChar = "_" OrElse IsChineseCharacter(_readingChar) OrElse Char.IsDigit(_readingChar))
            ReadChar()
        End While

        '检查
        CheckChar()


        '裁剪
        Return PySubString(_code, p, _pos + 1)
    End Function

    Shared Function PySubString(ByVal str As String, ByVal start As Integer, ByVal end_ As Integer, Optional ByVal step_ As Integer = 1) As String
        Dim result As New System.Text.StringBuilder()
        ' 处理负索引
        Dim startIndex As Integer = If(start < 0, str.Length + start, start)
        Dim endIndex As Integer = If(end_ < 0, str.Length + end_, end_)

        ' 确保索引在字符串长度范围内
        startIndex = Math.Max(0, startIndex)
        endIndex = Math.Min(str.Length, endIndex)

        ' 确保起始索引小于结束索引
        If startIndex >= endIndex Then
            Return String.Empty
        End If

        ' 根据步长进行切片
        For i As Integer = startIndex To endIndex - 1 Step step_
            result.Append(str(i))
        Next

        Return result.ToString()
    End Function
End Class

