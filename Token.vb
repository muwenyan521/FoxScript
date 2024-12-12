Imports System.Drawing
Imports System.Windows.Forms

Public Enum TokenType
    无意义
    PLUS ' +
    MINUS ' - 
    ASTERISK ' *
    SLASH ' / 
    ASSIGN ' =
    COMMA ' 逗号(,)
    EQ '==
    NOT_EQ ' !=
    AMPERSAND ' &
    IDENT
    ILLEGAL
    BANG '符号 !
    LT ' "<" 小于
    GT ' ">" 大于
    DIM_ 'Dim 定义变量
    RETURN_
    CR ' \r
    LF ' \n
    CRLF '\r\n
    SEMICOLON ' 分号(;)
    EOL ' End of line
    LPAREN '左括号
    RPAREN '右括号
    'Type
    BOOL_NOT ' Not
    BOOL_AND ' And （AndAlso）
    BOOL_OR ' Or (OrElse)
    BOOL_TRUE
    BOOL_FALSE
    INTNUMBER ' integer 123456
    DOBLENUMBER ' double 3.14
    STRING_ ' "aaa"
    'if
    IF_
    THEN_
    ELSEIF_
    ELSE_
    ENDIF_
    EOF
    'func
    FUNC
    ENDFUNC
    'class
    CLASS_
    ENDCLASS

    LBRACKET '[
    RBRACKET ']
    LBRACE '{
    RBRACE '}
    COLON '冒号 ":"
    DOT '点 ","

    PUBLIC_ 'Public
    PRIVATE_ 'Private

    FOR_
    IN_
    NEXT_

    WHILE_
    ENDWHILE

    NOTHING_

    NEW_

    READONLY_

    IMPORT
    AS_
    SingleQuote

    LET_
End Enum

Public Class Token
    '对照表

    Public Shared TokenTypeDict As New Dictionary(Of String, TokenType) From
    {
        {":", TokenType.COLON},
        {".", TokenType.DOT},
        {"[", TokenType.LBRACKET},
        {"]", TokenType.RBRACKET},
        {"{", TokenType.LBRACE},
        {"}", TokenType.LBRACE},
        {"+", TokenType.PLUS},
        {"-", TokenType.MINUS},
        {"*", TokenType.ASTERISK},
        {"/", TokenType.SLASH},
        {"=", TokenType.ASSIGN},
        {"==", TokenType.EQ},
        {"!=", TokenType.NOT_EQ},
        {"<>", TokenType.NOT_EQ},
        {"DIM", TokenType.DIM_},
        {"!", TokenType.BANG},
        {"NOT", TokenType.BOOL_NOT},
        {"<", TokenType.LT},
        {">", TokenType.GT},
        {";", TokenType.SEMICOLON},
        {vbCrLf, TokenType.EOL},
        {"TRUE", TokenType.BOOL_TRUE},
        {"FALSE", TokenType.BOOL_FALSE},
        {"RETURN", TokenType.RETURN_},
        {"(", TokenType.LPAREN},
        {")", TokenType.RPAREN},
        {"IF", TokenType.IF_},
        {"AND", TokenType.BOOL_AND},
        {"OR", TokenType.BOOL_OR},
        {"THEN", TokenType.THEN_},
        {"ELSEIF", TokenType.ELSEIF_},
        {"ELSE", TokenType.ELSE_},
        {"ENDIF", TokenType.ENDIF_},
        {"FUNC", TokenType.FUNC},
        {"ENDFUNC", TokenType.ENDFUNC},
        {",", TokenType.COMMA},
        {"CLASS", TokenType.CLASS_},
        {"ENDCLASS", TokenType.ENDCLASS},
        {"PUBLIC", TokenType.PUBLIC_},
        {"PRIVATE", TokenType.PRIVATE_},
        {"FOR", TokenType.FOR_},
        {"IN", TokenType.IN_},
        {"NEXT", TokenType.NEXT_},
        {"WHILE", TokenType.WHILE_},
        {"ENDWHILE", TokenType.ENDWHILE},
        {"NEW", TokenType.NEW_},
        {"IMPORT", TokenType.IMPORT},
        {"AS", TokenType.AS_},
        {"LET", TokenType.LET_},
        {vbNullChar, TokenType.EOF}
    }


    Public TokenType As TokenType
    Public Value As Object
    Public Line As Long
    Public Sub New()
    End Sub

    Public Sub New(arg_tokenType As TokenType, arg_value As String, arg_line As Long)
        TokenType = arg_tokenType
        Value = arg_value
        Line = arg_line
    End Sub

End Class