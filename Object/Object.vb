Imports System.Numerics
Imports System.Text
Imports FoxScript.Evaluator
Imports FoxScript.Utils
Imports FoxScript.BooleanUtils
Imports FoxScript.FileSystemUtils
Imports FoxScript.StringUtils
Imports FoxScript.ErrorUtils
Imports FoxScript.StatementUtils

'各种类型
Public Enum ObjectType
    INTEGER_OBJ ' INTEGER
    DOUBLE_OBJ ' DOUBLE
    BOOL_OBJ ' BOOL
    RETURN_VALUE_OBJ 'RETURN_VALUE
    FUNCTION_OBJ ' "FUNCTION"
    STRING_OBJ 'STRING
    ERROR_OBJ 'ERROR
    CUSTOM_ERROR_OBJ 'ERROR
    BUILTIN_OBJ 'BUILTIN
    ARRAY_OBJ 'Array
    DICTIONARY_KEY_OBJ '字典Key
    DICTIONARY_VALUE_OBJ '字典Value
    DICTIONARY_OBJ '字典
    NOTHINGL_OBJ ' NOTHING
    CLASS_OBJ ' CLASS
    VB_FUNCTION_OBJ ' VBFunction
    VB_CLASS_OBJ ' VBClass
    VB_MEMBER_OBJ ' VBMember
End Enum

'True和False 没啥好介绍的，比较鸡肋
Public Enum BoolValue
    BOOL_FALSE
    BOOL_TRUE
End Enum

Public Class Fox_DictionaryPair
    Implements Fox_DictionaryItem

    Public Key As Fox_Object
    Public Value As Fox_Object

    Public Function GetKey() As Fox_DictionaryKey Implements Fox_DictionaryItem.GetKey
        Return Key
    End Function

    Public Function Type() As ObjectType Implements Fox_Object.Type
        Throw New NotImplementedException()
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return $"{Key.Inspect}:{Value.Inspect}"
    End Function
End Class
Public Class Fox_Dictionary
    Implements Fox_Object
    Public Pairs As Dictionary(Of Fox_DictionaryKey, Fox_DictionaryPair)
    Public Function Inspect() As String Implements Fox_Object.Inspect
        Dim sb As New StringBuilder
        Dim paris_ As New List(Of String)
        For Each pair As Fox_DictionaryPair In Pairs.Values
            paris_.Add($"{pair.Key.Inspect}:{pair.Value.Inspect}")
        Next

        sb.Append("{")
        sb.Append(Strings.Join(paris_.ToArray, ", "))
        sb.Append("}")

        Return sb.ToString()
    End Function

    Public Function Type() As ObjectType Implements Fox_Object.Type
        Return ObjectType.DICTIONARY_OBJ
    End Function
End Class

'FoxScript的Object类型 
Public Interface Fox_Object
    Function Type() As ObjectType
    Function Inspect() As String

End Interface

Public Interface VB_Object
    Inherits Fox_Object
End Interface

'FoxScript的DictionaryItem类型
Public Interface Fox_DictionaryItem
    Inherits Fox_Object
    Function GetKey() As Fox_DictionaryKey
End Interface


'FoxScript的DictionaryKey类型 
Public Class Fox_DictionaryKey
    Implements Fox_Object
    Public ValueType As ObjectType
    Public Value As Object

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Value.ToString
    End Function

    Public Function Type() As ObjectType Implements Fox_Object.Type
        Return ObjectType.DICTIONARY_KEY_OBJ
    End Function
End Class

'FoxScript的DictionaryKey类型 
Public Class Fox_DictionaryValue
    Implements Fox_Object
    Public ValueType As ObjectType
    Public Value As Object

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Value.ToString
    End Function

    Public Function Type() As ObjectType Implements Fox_Object.Type
        Return ObjectType.DICTIONARY_VALUE_OBJ
    End Function
End Class



'Fox_DictionaryHelper
Public Class Fox_DictionaryHelper
    Public Shared Function CreateKey(value As Fox_Bool) As Fox_DictionaryKey
        Return New Fox_DictionaryKey With {.ValueType = value.Type, .Value = value.Value}
    End Function
    Public Shared Function CreateKey(value As Fox_Integer) As Fox_DictionaryKey
        Return New Fox_DictionaryKey With {.ValueType = value.Type, .Value = value.Value}
    End Function
    Public Shared Function CreateKey(value As Fox_String) As Fox_DictionaryKey
        Return New Fox_DictionaryKey With {.ValueType = value.Type, .Value = value.Value}
    End Function
    Public Shared Function CreateKey(value As Fox_Object) As Fox_DictionaryKey
        Select Case value.Type
            Case ObjectType.INTEGER_OBJ
                Return CreateKey(TryCast(value, Fox_Integer))
            Case ObjectType.BOOL_OBJ
                Return CreateKey(TryCast(value, Fox_Bool))
            Case ObjectType.STRING_OBJ
                Return CreateKey(TryCast(value, Fox_String))
        End Select

        Return Nothing
    End Function


    Public Shared Function CreateValue(value As Fox_Bool) As Fox_DictionaryValue
        Return New Fox_DictionaryValue With {.ValueType = value.Type, .Value = value.Value}
    End Function
    Public Shared Function CreateValue(value As Fox_Integer) As Fox_DictionaryValue
        Return New Fox_DictionaryValue With {.ValueType = value.Type, .Value = value.Value}
    End Function
    Public Shared Function CreateValue(value As Fox_String) As Fox_DictionaryValue
        Return New Fox_DictionaryValue With {.ValueType = value.Type, .Value = value.Value}
    End Function
    Public Shared Function CreateValue(value As Fox_Object) As Fox_DictionaryValue
        Select Case value.Type
            Case ObjectType.INTEGER_OBJ
                Return CreateValue(TryCast(value, Fox_Integer))
            Case ObjectType.BOOL_OBJ
                Return CreateValue(TryCast(value, Fox_Bool))
            Case ObjectType.STRING_OBJ
                Return CreateValue(TryCast(value, Fox_String))
        End Select

        Return Nothing
    End Function

End Class


'FoxScript的Array类型 
Public Class Fox_Array
    '继承Object
    Implements Fox_Object
    Public Elements As List(Of Fox_Object)
    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Array类型
        Return ObjectType.ARRAY_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Dim sb As New StringBuilder
        Dim elements_ As New List(Of String)
        For Each e As Object In Elements
            elements_.Add(e.Inspect())
        Next

        sb.Append("[")
        sb.Append(Strings.Join(elements_.ToArray, ", "))
        sb.Append("]")

        Return sb.ToString()
    End Function

    Public Function ToList() As List(Of String)
        Return Elements.Select(Function(e) If(TypeOf e Is String, TryCast(e, Fox_String).Value, e.Inspect)).ToList
    End Function
End Class



'FoxScript的Builtin类型 
Public Class Fox_Builtin
    '继承Object
    Implements Fox_Object
    Public BuiltinFunction As Func(Of IEnumerable(Of Object), Object)
    Public BuiltinIdentifier As Fox_Object

    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Builtin类型
        Return ObjectType.BUILTIN_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Nothing
    End Function
End Class

'FoxScript的Double类型 
Public Class Fox_Double
    '继承Object
    Implements Fox_Object
    Public Value As Decimal

    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Integer类型
        Return ObjectType.DOUBLE_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Me.Value.ToString
    End Function
End Class

'FoxScript的Integer类型 
Public Class Fox_Integer
    '继承Object
    Implements Fox_Object
    Public Value As BigInteger
    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Integer类型
        Return ObjectType.INTEGER_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Value.ToString
    End Function

    Public Sub New()
    End Sub
    Public Sub New(number As BigInteger)
        Value = number
    End Sub
End Class

'FoxScript的ReturnValue类型 
Public Class Fox_ReturnValue
    '继承Object
    Implements Fox_Object
    Public Value As Fox_Object
    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回ReturnValue类型
        Return ObjectType.RETURN_VALUE_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Value.Inspect
    End Function
End Class

'布尔型
Public Class Fox_Bool
    '继承Object
    Implements Fox_Object
    Public Value As Boolean

    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Bool类型
        Return ObjectType.BOOL_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return CStr(Value).ToLower
    End Function

End Class

'字符串
Public Class Fox_String
    '继承Object
    Implements Fox_Object
    Public Value As String

    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Bool类型
        Return ObjectType.STRING_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return $"""{Value}"""
    End Function
End Class

'万 恶 之 源 Nothing. 空(类似C#的null)
Public Class Fox_Nothing
    Implements Fox_Object
    Public Function Type() As ObjectType Implements Fox_Object.Type
        Return ObjectType.NOTHINGL_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return "Nothing"
    End Function
End Class

'万 恶 之 源 * 2 . Error （参考C#的Expction）
Public Class Fox_Error
    Implements Fox_Object
    Public Message As String
    Public Function Type() As ObjectType Implements Fox_Object.Type
        Return ObjectType.ERROR_OBJ
    End Function

    '返回一个错误信息
    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return $"Error: {Message}"
    End Function

    Public Sub New(Message As String)
        Me.Message = Message
    End Sub

    Public Sub New()
    End Sub
End Class

'FoxScript的Function类型 
Public Class Fox_Function
    '继承Object
    Implements Fox_Object
    Public Parameters As List(Of Identifier)
    Public Body As BlockStatement
    Public Env As Environment
    Public Name As Identifier
    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Class类型
        Return ObjectType.FUNCTION_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Nothing
    End Function

    Public Overrides Function ToString() As String
        Dim sb As New StringBuilder
        Dim params As New List(Of String)

        If Parameters IsNot Nothing Then
            For Each p As Identifier In Parameters
                params.Add(p.ToString)
            Next
        End If

        sb.Append($"func {Name}")
        sb.Append("(")
        sb.Append(Strings.Join(params.ToArray, ", "))
        sb.Append(")")
        sb.Append(Body.ToString())
        sb.Append($"{vbCrLf}endfunc")

        Return sb.ToString
    End Function
End Class

'FoxScript的Class类型 
Public Class Fox_Class
    '继承Object
    Implements Fox_Object
    Public Body As BlockStatement
    Public Env As ClassEnvironment
    Public BaseClass As Fox_Class
    Public Name As Identifier
    Public CreateFunc As FunctionLiteral
    Public CreateArgs As List(Of Expression)

    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Class类型
        Return ObjectType.CLASS_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Nothing
    End Function

    Public Overrides Function ToString() As String
        Dim sb As New StringBuilder

        sb.Append($"class {Name}")
        sb.Append(vbCrLf)
        sb.Append(Body.ToString())
        sb.Append(vbCrLf)
        sb.Append("endclass")

        Return sb.ToString
    End Function
End Class


'VBClass类型 
Public Class VBClass
    '继承Object
    Implements VB_Object
    Public Name As Identifier
    Public CreateFunc As VBFunction
    Public CreateArgs As List(Of Expression)
    Public Members As List(Of VB_Object)
    Public OnPropertyChangeFunction As Func(Of String, Object, Object)
    Public Env As Environment
    Public Instance

    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Class类型
        Return ObjectType.VB_CLASS_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Nothing
    End Function

End Class

'VBFunction类型 
Public Class VBFunction
    '继承Object
    Implements VB_Object
    Public Func As Func(Of IEnumerable(Of Object), Object)
    Public Name As Identifier
    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Class类型
        Return ObjectType.VB_FUNCTION_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Nothing
    End Function

    Public Sub New(Name As String, Func As Func(Of IEnumerable(Of Object), Object))
        Me.Name = New Identifier With {.Value = Name}
        Me.Func = Func
    End Sub

    Public Sub New()

    End Sub
End Class


'VBMember类型 
Public Class VBMember
    '继承Object
    Implements VB_Object
    Public Value As Object
    Public Name As Identifier
    Public Function Type() As ObjectType Implements Fox_Object.Type
        '返回Class类型
        Return ObjectType.VB_MEMBER_OBJ
    End Function

    Public Function Inspect() As String Implements Fox_Object.Inspect
        Return Value.ToString
    End Function

    Public Sub New(Name As String, Value As Object)
        Me.Name = New Identifier With {.Value = Name}
        Me.Value = Value
    End Sub

    Public Sub New()

    End Sub
End Class

Public Class Fox_CustomError
    Inherits Fox_Error
    Implements Fox_Object
    Public Shadows Message As String
    Public ErrorName As String
    Public ClassObject As Fox_Class
    Public Overloads Function Type() As ObjectType Implements Fox_Object.Type
        Return ObjectType.CUSTOM_ERROR_OBJ
    End Function

    '返回一个错误信息
    Public Overloads Function Inspect() As String Implements Fox_Object.Inspect
        Return $"{ErrorName}: {Message}"
    End Function

    Public Sub New(ClassObj As Fox_Class)
        ClassObject = ClassObj
    End Sub

    Public Sub New()
    End Sub
End Class