Imports System.Drawing
Imports System.Windows.Forms

Public Enum TokenType
    无意义
    PLUS ' +
    MINUS ' - 
    ASTERISK ' *
    SLASH ' / 

    ASSIGN ' =
    EQ '==
    NOT_EQ ' !=

    IDENT
    ILLEGAL

    COMMA ' 逗号(,)
    AMPERSAND ' &
    LT ' "<" 小于
    GT ' ">" 大于
    SEMICOLON ' 分号(;)

    DIM_ 'Dim 定义变量
    LET_
    RETURN_

    CR ' \r
    LF ' \n
    CRLF '\r\n
    EOL ' End of line


    BOOL_NOT ' Not
    BOOL_AND ' And （AndAlso）
    BOOL_OR ' Or (OrElse)
    BANG '符号 !
    BOOL_TRUE
    BOOL_FALSE

    INTNUMBER ' integer 123456
    DOBLENUMBER ' double 3.14
    STRING_ ' "aaa"

    IF_
    THEN_
    ELSEIF_
    ELSE_
    ENDIF_

    FUNC
    ENDFUNC

    NEW_
    CLASS_
    ENDCLASS

    LPAREN '左括号 (
    RPAREN '右括号 )
    LBRACKET '[
    RBRACKET ']
    LBRACE '{
    RBRACE '}

    COLON '冒号 ":"
    DOT '点 ","

    PUBLIC_ 'Public
    PRIVATE_ 'Private
    READONLY_

    FOR_
    IN_
    NEXT_

    WHILE_
    ENDWHILE

    NOTHING_
    AS_
    SingleQuote

    INCLUDE
    IMPORT
    FROM

    EOF
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
        {"READONLY", TokenType.READONLY_},
        {"FOR", TokenType.FOR_},
        {"IN", TokenType.IN_},
        {"NEXT", TokenType.NEXT_},
        {"WHILE", TokenType.WHILE_},
        {"ENDWHILE", TokenType.ENDWHILE},
        {"NEW", TokenType.NEW_},
        {"INCLUDE", TokenType.INCLUDE},
        {"IMPORT", TokenType.IMPORT},
        {"FROM", TokenType.FROM},
        {"AS", TokenType.AS_},
        {"LET", TokenType.LET_},
        {vbNullChar, TokenType.EOF}
    }

    Public Shared StringDict As New Dictionary(Of TokenType, String) From
    {
        {TokenType.COLON, ":"},
        {TokenType.DOT, "."},
        {TokenType.LBRACKET, "["},
        {TokenType.RBRACKET, "]"},
        {TokenType.LBRACE, "{"},
        {TokenType.RBRACE, "}"},
        {TokenType.PLUS, "+"},
        {TokenType.MINUS, "-"},
        {TokenType.ASTERISK, "*"},
        {TokenType.SLASH, "/"},
        {TokenType.ASSIGN, "="},
        {TokenType.EQ, "=="},
        {TokenType.NOT_EQ, "!="},
        {TokenType.DIM_, "Dim"},
        {TokenType.BANG, "!"},
        {TokenType.BOOL_NOT, "Not"},
        {TokenType.LT, "<"},
        {TokenType.GT, ">"},
        {TokenType.SEMICOLON, ";"},
        {TokenType.EOL, "换行符"},
        {TokenType.IDENT, "标识符"},
        {TokenType.BOOL_TRUE, "True"},
        {TokenType.BOOL_FALSE, "False"},
        {TokenType.RETURN_, "Return"},
        {TokenType.LPAREN, "("},
        {TokenType.RPAREN, ")"},
        {TokenType.IF_, "If"},
        {TokenType.BOOL_AND, "And"},
        {TokenType.BOOL_OR, "Or"},
        {TokenType.THEN_, "Then"},
        {TokenType.ELSEIF_, "Elseif"},
        {TokenType.ELSE_, "Else"},
        {TokenType.ENDIF_, "EndIf"},
        {TokenType.FUNC, "Func"},
        {TokenType.ENDFUNC, "EndFunc"},
        {TokenType.COMMA, ","},
        {TokenType.CLASS_, "Class"},
        {TokenType.ENDCLASS, "EndClass"},
        {TokenType.PUBLIC_, "PUBLIC"},
        {TokenType.PRIVATE_, "PRIVATE"},
        {TokenType.READONLY_， "READONLY"},
        {TokenType.FOR_, "For"},
        {TokenType.IN_, "In"},
        {TokenType.NEXT_, "Next"},
        {TokenType.WHILE_, "While"},
        {TokenType.ENDWHILE, "EndWhile"},
        {TokenType.NEW_, "New"},
        {TokenType.INCLUDE, "Include"},
        {TokenType.IMPORT, "Import"},
        {TokenType.FROM, "From"},
        {TokenType.AS_, "As"},
        {TokenType.LET_, "Let"},
        {TokenType.EOF, "文件结束"}
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