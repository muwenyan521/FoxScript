
Imports System.Text

Public Interface Node
    Function TokenLiteral() As String
    Function ToString() As String
End Interface

Public Interface Statement : Inherits Node
    Sub statementNode()
End Interface

'表达式
Public Interface Expression : Inherits Node
    Sub ExpressionNode()
End Interface

'程序
Public Class Program
    Implements Node
    Public Statements As List(Of Statement) 'Statement列表
    Public Function TokenLiteral() As String Implements Node.TokenLiteral '这个不知道意义是啥
        If Len(Statements) > 0 Then
            Return Statements(0).TokenLiteral()
        End If
        Return ""
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString '获取所有Statement的字符串
        Dim out As New StringBuilder()
        For Each s As Statement In Statements
            out.Append(s.ToString)
        Next
        Return out.ToString()
    End Function
End Class

'标识符
Public Class Identifier
    Implements Expression '为了继承而继承
    Public Token As Token ' token.IDENT 词法单元 
    Public Value As String

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    '下面这俩没啥区别
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Return Value
    End Function
End Class

'整数
Public Class IntegerLiteral
    Implements Expression '为了继承而继承
    Public Token As Token ' token.IDENT 词法单元 
    Public Value As Long

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub
    '下面这俩没啥区别
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Return Token.Value
    End Function
End Class

'双精度浮点数
Public Class DoubleLiteral
    Implements Expression '为了继承而继承
    Public Token As Token ' token.Dot 词法单元 
    Public Value As Double

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub
    '下面这俩没啥区别
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Return Token.Value
    End Function
End Class

'字符串
Public Class StringLiteral
    Implements Expression '为了继承而继承
    Public Token As Token ' token.STRING_ 词法单元 
    Public Value As String

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub
    '下面这俩没啥区别
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Return Token.Value
    End Function
End Class
'数组
Public Class ArrayLiteral
    Implements Expression '为了继承而继承
    Public Token As Token
    Public Elements As List(Of Expression)
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Dim sb As New StringBuilder
        Dim elements_ As New List(Of String)
        For Each el As Expression In Elements
            elements_.Add(el.ToString)
        Next

        sb.Append("[")
        sb.Append(Strings.Join(Elements.ToArray, ", "))
        sb.Append("]")


        Return sb.ToString()
    End Function
End Class

'字典
Public Class DictionaryLiteral
    Implements Expression '为了继承而继承
    Public Token As Token
    Public Pairs As Dictionary(Of Expression, Expression)
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Dim sb As New StringBuilder
        Dim paris_ As New List(Of String)
        For Each p As KeyValuePair(Of Expression, Expression) In Pairs
            paris_.Add(p.Key.ToString & ":" & p.Value.ToString)
        Next

        sb.Append("{")
        sb.Append(Strings.Join(paris_.ToArray, ", "))
        sb.Append("}")

        Return sb.ToString()
    End Function
End Class


'布尔类型
Public Class Bool
    Implements Expression '为了继承而继承
    Public Token As Token ' token.IDENT 词法单元 
    Public Value As Boolean

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    '下面这俩没啥区别
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Return Token.Value
    End Function
End Class


Public Class DimStatement
    Implements Statement
    Public Token As Token ' TokenType | DIM_ 词法单元 
    Public Name As Identifier '变量名
    Public Value As Expression '表达式 

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value '返回Token的字符串值
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '没啥好解释的
        '返回值大概是这样 Dim [变量名] = [表达式]

        Dim sb As New StringBuilder

        sb.Append(TokenLiteral() & " ")
        sb.Append(Name.ToString)
        sb.Append(" = ")

        If Value IsNot Nothing Then
            sb.Append(Value.ToString())
        End If

        sb.Append(vbCrLf)

        Return sb.ToString()
    End Function
End Class

Public Class EOLStatement '没啥意义 End Of Line
    Implements Statement
    Public Token As Token ' TokenType | EOL 词法单元 
    Public Const Value = vbCrLf

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Return vbCrLf
    End Function
End Class

'返回
Public Class ReturnStatement
    Implements Statement
    Public Token As Token ' TokenType | RETURN_  词法单元 
    Public ReturnValue As Expression

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '同Dim一样没啥好解释的
        '返回值大概是这样 Return [表达式]

        Dim sb As New StringBuilder
        sb.Append(TokenLiteral() & " ")
        If ReturnValue IsNot Nothing Then
            sb.Append(ReturnValue.ToString)
        End If
        sb.Append(vbCrLf)
        Return sb.ToString
    End Function
End Class

'表达式语句
Public Class ExpressionStatement
    Implements Statement
    Public Token As Token '词法单元
    Public Expression As Expression '表达式

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        If Expression IsNot Nothing Then '判空
            Return Expression.ToString() '转换表达式为字符串
        End If
        Return ""
    End Function
End Class


Public Class IndexExpression '索引表达式 比如 [1]
    Implements Expression
    Public Token As Token ' "["词法单元
    Public Left As Expression
    Public Index As Expression
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Dim sb As New StringBuilder
        sb.Append("(")
        sb.Append(Left.ToString)
        sb.Append("[")
        sb.Append(Index.ToString)
        sb.Append("])")

        Return sb.ToString()
    End Function
End Class


Public Class PrefixExpression '前缀表达式 比如 !a ++a 
    Implements Expression
    Public Token As Token ' 前缀词法单元，如! 
    Public Operator_ As String
    Public Right As Expression

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        '返回的大概字符串:
        '([操作符][任意内容])

        Dim sb As New StringBuilder

        sb.Append("(")
        sb.Append(Operator_)
        sb.Append(Right.ToString().Replace(vbCr, ""))
        sb.Append(")")

        Return sb.ToString()
    End Function
End Class

Public Class InfixExpression '中缀表达式 比如 a / a
    Implements Expression
    Public Token As Token ' 中缀词法单元 如 + - * /
    Public Operator_ As String
    Public Right As Expression
    Public Left As Expression

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        '返回的大概字符串:
        '([任意内容] [操作符] [任意内容])

        Dim sb As New StringBuilder

        sb.Append("(")
        sb.Append(Left.ToString())
        sb.Append(" " & Operator_ & " ")
        sb.Append(Right.ToString().Replace(vbCr, ""))
        sb.Append(")")

        Return sb.ToString()
    End Function
End Class

Public Class ForStatement
    Implements Statement
    Public Token As Token   'For 词法单元 
    Public ItemVar As Identifier '迭代变量
    Public Items As Expression '迭代变量
    Public LoopBlock As BlockStatement '默认的块 

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append("for")
        sb.Append(" ")
        sb.Append(ItemVar.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append("in")
        sb.Append(" ")
        sb.Append(Items.ToString)
        sb.Append(LoopBlock.ToString.Replace(vbCr, ""))
        sb.Append("next")

        Return sb.ToString
    End Function
End Class


Public Class IfExpression
    Implements Expression
    Public Token As Token   'if'词法单元 
    Public Condition As Expression
    Public Consequence As BlockStatement '默认的块
    Public Alternative As BlockStatement '其他条件的块
    Public ElseIf_List As List(Of ElseIfExpression)

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append("if")
        sb.Append(Condition.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append(Consequence.ToString.Replace(vbCr, ""))

        If Alternative IsNot Nothing Then
            sb.Append("else ")
            sb.Append(Alternative.ToString.Replace(vbCr, ""))
        End If

        Return sb.ToString
    End Function
End Class

Public Class WhileStatement
    Implements Statement
    Public Token As Token   ' while词法单元 
    Public LoopCondition As Expression
    Public LoopBlock As BlockStatement '循环的块

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append("while")
        sb.Append(" ")
        sb.Append(LoopCondition.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append(LoopBlock.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append("endwhile")

        Return sb.ToString
    End Function
End Class

Public Class AssignmentExpression
    Implements Expression
    Public Token As Token   ' ident词法单元 
    Public Identifier As Identifier
    Public Value As Expression '标识符的值

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append(Identifier.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append("=")
        sb.Append(" ")
        sb.Append(Value.ToString.Replace(vbCr, ""))

        Return sb.ToString
    End Function
End Class


Public Class ElseIfExpression
    Implements Expression
    Public Token As Token   'elseif'词法单元 
    Public Condition As Expression
    Public Consequence As BlockStatement '默认的块

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append("elseif")
        sb.Append(Condition.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append(Consequence.ToString.Replace(vbCr, ""))
        Return sb.ToString
    End Function
End Class
'对象函数调用表达式
Public Class ObjectCallExpression
    Implements Expression
    Public Token As Token
    Public Obj As Expression
    Public Func As Expression '// 标识符或函数字面量 
    Public Arguments As List(Of Expression) '参数
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '返回的大概内容:
        '[函数名](参数1, 参数2 ....)

        Dim sb As New StringBuilder

        Dim args As New List(Of String)
        For Each a As Expression In Arguments
            args.Add(a.ToString)
        Next


        sb.Append(Obj.ToString)
        sb.Append(".")
        sb.Append(Func.ToString())
        sb.Append("(")
        sb.Append(Strings.Join(args.ToArray, ", "))
        sb.Append(")")
        Return sb.ToString
    End Function
End Class

'函数调用表达式
Public Class CallExpression
    Implements Expression
    Public Token As Token
    Public Func As Expression '// 标识符或函数字面量 
    Public Arguments As List(Of Expression) '参数
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '返回的大概内容:
        '[函数名](参数1, 参数2 ....)

        Dim sb As New StringBuilder

        Dim args As New List(Of String)
        For Each a As Expression In Arguments
            args.Add(a.ToString)
        Next



        sb.Append(Func.ToString())
        sb.Append("(")
        sb.Append(Strings.Join(args.ToArray, ", "))
        sb.Append(")")
        Return sb.ToString
    End Function
End Class

'块语句
Public Class BlockStatement
    Implements Statement
    Public Token As Token
    Public Statements As List(Of Statement)

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '参考Program，类似的代码

        Dim sb As New StringBuilder
        For Each s As Statement In Statements
            sb.Append(s.ToString)
        Next

        Return sb.ToString
    End Function
End Class

Public Class FunctionLiteral
    Implements Expression
    Public Token As Token ' func tokentype
    Public Parameters As List(Of Identifier)
    Public Body As BlockStatement
    Public Name As Identifier

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString

        Dim sb As New StringBuilder

        Dim params As New List(Of String)
        For Each p In Parameters
            sb.Append(p.ToString)
        Next

        sb.Append(TokenLiteral())
        sb.Append("(")

        sb.Append(Strings.Join(params.ToArray, ","))
        sb.Remove(sb.Length, 1)

        sb.Append(") ")
        sb.Append(Body.ToString() & vbCrLf)
        sb.Append("endfunc")

        Return sb.ToString()
    End Function
End Class
