
Imports FoxScript.Evaluator
Imports FoxScript.Utils
Imports FoxScript.BooleanUtils
Imports FoxScript.FileSystemUtils
Imports FoxScript.StringUtils
Imports FoxScript.ErrorUtils
Imports FoxScript.StatementUtils


Public Class AssignmentExpressionEvaluator
    Implements IEvaluatorHandler
    Private ReadOnly _Evaluator As Evaluator
    Private Delegate Function EvalHandler(SetExp As Expression, valueExp As Expression, ByRef env As Environment) As Fox_Object

    Private ReadOnly _EvalHandler As New Dictionary(Of Type, EvalHandler) From {
        {GetType(Identifier), AddressOf IdentiferEval},
        {GetType(IndexExpression), AddressOf IndexExpressionEval},
        {GetType(ObjectMemberExpression), AddressOf ObjectMemberExpressionEval}
    }


    Public Sub New(ByRef evaluator As Evaluator)
        _Evaluator = evaluator
    End Sub

    Private Function IdentiferEval(SetExp As Expression, valueExp As Expression, ByRef env As Environment) As Fox_Object
        '转换为表达式为Identifier
        Dim ident = TryCast(SetExp, Identifier)

        '尝试寻找标识符
        Dim get_Ident = env.GetValue(ident.ToString)

        '判断标识符是否存在
        If Not get_Ident.Item2 Then Return ThrowError($"标识符 {ident.Value} 不存在! ")

        '将环境中标识符的值修改
        Dim result = env.SetValue(ident.Value, _Evaluator.Eval(valueExp, env), False)
        If TypeOf result Is Fox_Error Then Return result

        Return Nothing
    End Function

    Private Function IndexExpressionEval(SetExp As Expression, valueExp As Expression, ByRef env As Environment) As Fox_Object
        Dim indexExp = TryCast(SetExp, IndexExpression)
        Dim left = _Evaluator.Eval(indexExp.Left, env)
        If IsError(left) Then Return left

        Dim index = _Evaluator.Eval(indexExp.Index, env)
        If IsError(index) Then Return index

        Dim origin_obj = left
        Select Case origin_obj.Type
            Case ObjectType.ARRAY_OBJ
                Dim arrayObj = TryCast(origin_obj, Fox_Array)
                Dim indexObj = TryCast(index, Fox_Integer)
                If indexObj Is Nothing Then Return ThrowError($"索引的值不是整数: {index.Type} 内容 {index.Inspect}")

                Dim max = CLng(arrayObj.Elements.Count - 1)
                If indexObj.Value < 0 Then Return ThrowError($"索引的值小于0")
                If indexObj.Value > max Then Return ThrowError($"索引的值大于数组索引最大值{max}")

                arrayObj.Elements(indexObj.Value) = _Evaluator.Eval(valueExp, env)

                env.SetValue(TryCast(indexExp.Left, Identifier).Value, arrayObj, False)
            Case ObjectType.STRING_OBJ
                Dim strObj = TryCast(origin_obj, Fox_String)
                Dim indexObj = TryCast(index, Fox_Integer)
                If indexObj Is Nothing Then Return ThrowError($"索引的值不是整数: {index.Type} 内容 {index.Inspect}")

                Dim max = CLng(strObj.Value.Length - 1)
                If indexObj.Value < 0 Then Return ThrowError($"索引的值小于0")
                If indexObj.Value > max Then Return ThrowError($"索引的值大于字符串索引最大值{max}")

                Dim changeCharObj = _Evaluator.Eval(valueExp, env)
                If IsError(changeCharObj) Then Return changeCharObj
                If changeCharObj.Type <> ObjectType.STRING_OBJ Then Return ThrowError($"不支持{changeCharObj.Type}类型")

                Dim newStr = strObj.Value.Remove(indexObj.Value, 1).Insert(indexObj.Value, DirectCast(changeCharObj, Fox_String).Value)
                strObj.Value = newStr

                env.SetValue(TryCast(indexExp.Left, Identifier).Value, strObj, False)
        End Select

        Return Nothing
    End Function

    Public Function ObjectMemberExpressionEval(SetExp As Expression, valueExp As Expression, ByRef env As Environment) As Fox_Object
        Dim ObjectMemberExp = TryCast(SetExp, ObjectMemberExpression)
        Dim ClassObject As Object = _Evaluator.Eval(ObjectMemberExp.Left, env)
        If IsError(ClassObject) Then Return ClassObject

        Dim name = ObjectMemberExp.Right.ToString.Replace(" ", "")
        Dim value = _Evaluator.Eval(valueExp, env)

        If ClassObject.Type = ObjectType.CLASS_OBJ Then
            ClassObject.Env.SetValue(name, value, False)
            ClassObject.Env.SetValue("Me", ClassObject, False)
        ElseIf ClassObject.Type = ObjectType.VB_CLASS_OBJ Then
            Dim vbClassObj As VBClass = ClassObject
            vbClassObj.Env.SetValue(name, value, False)

            Dim memberIndex = FindVBMemberIndex(vbClassObj.Members, name)
            Dim memberObj = vbClassObj.Members(memberIndex)

            If memberObj.Type <> ObjectType.VB_MEMBER_OBJ Then Return Nothing

            vbClassObj.Instance.GetType().GetField(name).SetValue(vbClassObj.Instance, value)
            vbClassObj.OnPropertyChangeFunction?.Invoke(name, value)
        End If

        Return Nothing
    End Function

    Public Function _Eval(SetExp As Expression, valueExp As Expression, ByRef env As Environment)
        If _EvalHandler.ContainsKey(SetExp.GetType) Then
            Return _EvalHandler(SetExp.GetType)(SetExp, valueExp, env)
        End If

        Return Nothing
    End Function

    Public Function Eval(ParamArray Args() As Object) As Fox_Object Implements IEvaluatorHandler.Eval
        Return _Eval(Args(0), Args(1), Args(2))
    End Function
End Class
