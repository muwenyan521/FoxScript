
Imports System.Text
Imports System.Numerics

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
    Public Statements As List(Of Statement) ' 存放着所有语句的列表
    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        '判断语句列表的语句数量
        If Statements.Count > 0 Then
            '返回第一个语句中的Token的TokenLiteral
            Return Statements(0).TokenLiteral()
        End If
        Return ""
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString '获取所有Statement的字符串
        Dim sb As New StringBuilder()

        '将所有语句转成字符串
        For Each s As Statement In Statements
            sb.Append(s.ToString)
        Next

        Return sb.ToString()
    End Function
End Class

'标识符
Public Class Identifier
    Implements Expression '为了继承而继承
    Public Token As Token ' IDENT 词法单元 
    Public Value As String '标识符的值
    Public IsReadOnly As Boolean = False

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
    Public Token As Token 'INTNUMBER  词法单元 
    Public Value As BigInteger '使用BigInteger储存数字

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
    Public Token As Token
    Public Value As Decimal

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
    Public Token As Token ' STRING_ 词法单元 
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
    Public Elements As List(Of Expression) '存放所有表达式
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub
    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        Dim sb As New StringBuilder

        '创建一个列表 存放所有表达式转成字符串后的结果
        Dim elements_ As New List(Of String)

        '将所有的表达式转成字符串后存放至 elements_列表
        For Each el As Expression In Elements
            elements_.Add(el.ToString)
        Next

        sb.Append("[")
        sb.Append(
            Strings.Join(
                elements_.ToArray, ", "
            )
        )
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
    Public Token As Token
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
    Public Token As Token ' DIM_ 词法单元 
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
        '或者 Dim [变量名]

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
    Public Operator_ As String '操作符
    Public Right As Expression '右侧表达式

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        '返回的大概字符串:
        '([操作符][表达式])

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
    Public Operator_ As String '操作符
    Public Right As Expression '右侧表达式
    Public Left As Expression '左侧表达式

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Expression.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Expression.ToString
        '返回的大概字符串:
        '([表达式] [操作符] [表达式])

        Dim sb As New StringBuilder

        sb.Append("(")
        sb.Append(Left.ToString())
        sb.Append(" " & Operator_ & " ")
        sb.Append(Right.ToString().Replace(vbCr, ""))
        sb.Append(")")

        Return sb.ToString()
    End Function
End Class

Public Class ObjectMemberExpression '对象成员表达式 比如 a.value
    Implements Expression
    Public Token As Token ' DOT词法单元
    Public Operator_ As String = "."
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
        '([表达式][.][表达式])

        Dim sb As New StringBuilder

        sb.Append("(")
        sb.Append(Left.ToString())
        sb.Append(Operator_)
        sb.Append(Right.ToString().Replace(vbCr, ""))
        sb.Append(")")

        Return sb.ToString()
    End Function
End Class


Public Class ForStatement 'For 语句
    Implements Statement
    Public Token As Token   'For 词法单元 
    Public ItemVar As Identifier '迭代变量
    Public Items As Expression '迭代变量
    Public LoopBlock As BlockStatement '欲循环的块 

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

Public Class NotExpression 'Not 表达式
    Implements Expression
    Public Token As Token   ' BOOL_NOT 词法单元 
    Public Right As Expression '右侧要取反的表达式

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append("not")
        sb.Append(" ")
        sb.Append(Right.ToString)
        Return sb.ToString
    End Function
End Class

Public Class AndExpression ' And 表达式
    Implements Expression
    Public Token As Token   ' BOOL_AND 词法单元 
    Public Left As Expression '左侧进行与运算的表达式
    Public Right As Expression '右侧进行与运算的表达式

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append(Left.ToString)
        sb.Append(" and ")
        sb.Append(Right.ToString)
        Return sb.ToString
    End Function
End Class

Public Class OrExpression 'Or 表达式
    Implements Expression
    Public Token As Token   ' BOOL_OR 词法单元 
    Public Left As Expression '左侧进行或运算的表达式
    Public Right As Expression '右侧进行或运算的表达式

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append(Left.ToString)
        sb.Append(" or ")
        sb.Append(Right.ToString)
        Return sb.ToString
    End Function
End Class


Public Class IfExpression 'If 表达式
    Implements Expression
    Public Token As Token   'IF_ 词法单元 
    Public Condition As Expression
    Public Consequence As BlockStatement 'If 表达式中默认要运行的块
    Public Alternative As BlockStatement 'If 表达式中默认条件不满足时运行的块
    Public ElseIf_List As List(Of ElseIfExpression) '存放所有ElseIf 表达式 的列表

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

Public Class WhileStatement 'While 表达式
    Implements Statement
    Public Token As Token   'WHILE_ 词法单元 
    Public LoopCondition As Expression '循环的条件
    Public LoopBlock As BlockStatement '欲循环执行的代码

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

Public Class ClassStatement '类语句
    Implements Statement
    Public Token As Token   'CLASS_ 词法单元 
    Public Body As BlockStatement
    Public Name As Identifier

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append("class")
        sb.Append(" ")
        sb.Append(Name.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append(Body.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append("endclass")

        Return sb.ToString
    End Function
End Class

Public Class AssignmentExpression '复制表达式
    Implements Expression
    Public Token As Token
    Public SetExp As Expression '欲设置的内容
    Public Value As Expression '欲设置的值

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        Dim sb As New StringBuilder
        sb.Append(SetExp.ToString.Replace(vbCr, ""))
        sb.Append(" ")
        sb.Append("=")
        sb.Append(" ")
        sb.Append(Value.ToString.Replace(vbCr, ""))

        Return sb.ToString
    End Function
End Class


Public Class ElseIfExpression
    Implements Expression
    Public Token As Token   'ELSEIF_ 词法单元 
    Public Condition As Expression '条件表达式
    Public Consequence As BlockStatement '欲执行的块

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

'对象函数创建表达式
Public Class ObjectCreateExpression
    Implements Expression
    Public Token As Token 'NEW_ 词法单元
    Public ObjType As Expression '要创建的类型 | 类
    Public Arguments As List(Of Expression) '对象创建的参数
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '返回的大概内容:
        'New [类/接口名](参数1, 参数2 ....)

        Dim sb As New StringBuilder

        Dim args As New List(Of String)
        For Each a As Expression In Arguments
            args.Add(a.ToString)
        Next


        sb.Append("New")
        sb.Append(" ")
        sb.Append(ObjType.ToString())
        sb.Append("(")
        sb.Append(Strings.Join(args.ToArray, ", "))
        sb.Append(")")
        Return sb.ToString
    End Function
End Class


'文件导入表达式
Public Class FileImportExpression
    Implements Expression
    Public Token As Token ' Import 词法单元
    Public FilePath As Expression ' 文件路径
    Public AliasName As Expression '别名
    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '返回的大概内容:
        'Import [路径] As [别名]

        Dim sb As New StringBuilder

        sb.Append("Import")
        sb.Append(" ")
        sb.Append($"""{FilePath.ToString}""")
        sb.Append(" ")
        sb.Append("As")
        sb.Append(" ")
        sb.Append(AliasName.ToString)

        Return sb.ToString
    End Function
End Class

'函数调用表达式
Public Class CallExpression
    Implements Expression
    Public Token As Token
    Public Func As Expression '标识符或函数字面量 
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
    Public Statements As List(Of Statement) '存放所有语句的列表

    Public Sub statementNode() Implements Statement.statementNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString
        '将块中所有的语句转换成字符串

        Dim sb As New StringBuilder
        For Each s As Statement In Statements
            sb.Append(s.ToString)
        Next

        Return sb.ToString
    End Function
End Class

Public Class FunctionLiteral
    Implements Expression
    Public Token As Token 'Func 词法单元
    Public Parameters As List(Of Identifier) '形参列表
    Public Body As BlockStatement '函数里的 块
    Public Name As Identifier

    Public Sub ExpressionNode() Implements Expression.ExpressionNode
        Throw New NotImplementedException()
    End Sub

    Public Function TokenLiteral() As String Implements Node.TokenLiteral
        Return Token.Value
    End Function

    Public Overrides Function ToString() As String Implements Node.ToString

        Dim sb As New StringBuilder

        '创建形参列表
        Dim params As New List(Of String)

        '将 所有形参 转成字符串后添加至 形参列表
        For Each p In Parameters
            sb.Append(p.ToString)
        Next


        sb.Append("func")
        sb.Append("(")

        sb.Append(Strings.Join(params.ToArray, ","))
        sb.Remove(sb.Length, 1)

        sb.Append(") ")
        sb.Append(Body.ToString() & vbCrLf)
        sb.Append("endfunc")

        Return sb.ToString()
    End Function
End Class
