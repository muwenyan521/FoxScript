Imports System.Numerics
Imports System.Runtime.InteropServices

Public Class Builtins
    Public Shared AppPath = AppDomain.CurrentDomain.BaseDirectory
    Public Shared builtinFuncs As New Dictionary(Of String, Fox_Builtin) From
    {
        {"input", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                               Dim 所需实参数 = 1

                                                               If args.Count > 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"实参过多")
                                                               End If

                                                               If args.Count = 所需实参数 Then
                                                                   Console.Write(args(0).Value)
                                                                   Dim r = Console.ReadLine
                                                                   Return New Fox_String With {.Value = r}
                                                               End If

                                                               If args.Count = 0 Then
                                                                   Dim r = Console.ReadLine()
                                                                   Return New Fox_String With {.Value = r}
                                                               End If

                                                               If args.Count < 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               Return Nothing
                                                           End Function}},
        {"len", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                             Dim 所需实参数 = 1

                                                             If args.Count > 所需实参数 Then
                                                                 Return Evaluator.ThrowError($"实参过多")
                                                             End If

                                                             If args.Count < 所需实参数 Then
                                                                 Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                             End If

                                                             If args.Count = 所需实参数 Then
                                                                 Select Case args(0).Type()
                                                                     Case ObjectType.STRING_OBJ
                                                                         Return New Fox_Integer With {.Value = CLng(args(0).Value.ToString.Count)}
                                                                     Case ObjectType.ARRAY_OBJ
                                                                         Return New Fox_Integer With {.Value = CLng(TryCast(args(0), Fox_Array).Elements.Count)}
                                                                     Case Else
                                                                         Return Evaluator.ThrowError($"len函数不支持{args(0).Type()}类型")
                                                                 End Select
                                                             End If

                                                             Return Nothing
                                                         End Function}},
        {"print", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                               Dim 所需实参数 = 1

                                                               If args.Count > 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"实参过多")
                                                               End If

                                                               If args.Count < 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               If args.Count = 所需实参数 AndAlso args(0) IsNot Nothing Then
                                                                   If TypeOf args(0) Is Fox_String Then
                                                                       Console.WriteLine(args(0).Value)
                                                                   Else
                                                                       Console.WriteLine(args(0).Inspect)
                                                                   End If
                                                               End If

                                                               Return Nothing
                                                           End Function}},
        {"msgbox", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                                Dim 所需实参数 = 2

                                                                If args.Count > 所需实参数 Then
                                                                    Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                                End If

                                                                If args.Count < 所需实参数 Then
                                                                    Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                                End If

                                                                If args.Count = 所需实参数 Then
                                                                    Dim msg, title
                                                                    Select Case args(0).GetType()
                                                                        Case GetType(Fox_String)
                                                                            msg = args(0).Value
                                                                        Case GetType(Fox_Integer)
                                                                            msg = args(0).Value.ToString
                                                                        Case GetType(Fox_Bool)
                                                                            msg = args(0).Value.ToString
                                                                        Case Else
                                                                            Return Evaluator.ThrowError($"MsgBox函数不支持{args(0).Type()}类型")
                                                                    End Select

                                                                    Select Case args(1).GetType()
                                                                        Case GetType(Fox_String)
                                                                            title = args(1).Value
                                                                        Case GetType(Fox_Integer)
                                                                            title = args(1).Value.ToString
                                                                        Case GetType(Fox_Bool)
                                                                            title = args(1).Value.ToString
                                                                        Case Else
                                                                            Return Evaluator.ThrowError($"MsgBox函数不支持{args(1).Type()}类型")
                                                                    End Select

                                                                    MsgBox(msg,, title)
                                                                End If

                                                                Return Evaluator.Fox_Nothing
                                                            End Function}},
        {"clear", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                               Dim 所需实参数 = 0
                                                               If args.Count > 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               If args.Count = 所需实参数 Then
                                                                   Console.Clear()
                                                               End If

                                                               Return Nothing
                                                           End Function}},
        {"exit", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                              End
                                                          End Function}},
        {"CDbl", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                              Dim 所需实参数 = 1

                                                              If args.Count > 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count < 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                              End If
                                                              If args.Count = 所需实参数 Then
                                                                  Select Case args(0).Type()
                                                                      Case ObjectType.INTEGER_OBJ
                                                                          Return New Fox_Double With {.Value = CDbl(CLng(args(0).Value.ToString()))}
                                                                      Case ObjectType.DOUBLE_OBJ
                                                                          Return args(0)
                                                                      Case ObjectType.BOOL_OBJ
                                                                          Return New Fox_Double With {.Value = CDbl(Evaluator.BoolToLong(args(0).Value))}
                                                                      Case ObjectType.STRING_OBJ
                                                                          Dim strObj = TryCast(args(0), Fox_String)
                                                                          Dim r = Nothing
                                                                          If Decimal.TryParse(strObj.Value, r) Then
                                                                              Return New Fox_Double With {.Value = r}
                                                                          Else
                                                                              Return Evaluator.ThrowError($"无法转换 {strObj.Value} 为整数")
                                                                          End If
                                                                      Case ObjectType.NOTHINGL_OBJ
                                                                          Return New Fox_Double With {.Value = 0.0}
                                                                      Case Else
                                                                          Return Evaluator.ThrowError($"CDbl函数不支持{args(0).Type()}类型")
                                                                  End Select
                                                              End If

                                                              Return Evaluator.Fox_Nothing
                                                          End Function}},
        {"CInt", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                              Dim 所需实参数 = 1

                                                              If args.Count > 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count < 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count = 所需实参数 Then
                                                                  Select Case args(0).Type()
                                                                      Case ObjectType.INTEGER_OBJ
                                                                          Return args(0)
                                                                      Case ObjectType.DOUBLE_OBJ
                                                                          Return New Fox_Integer With {.Value = New BigInteger(CLng(args(0).Value))}
                                                                      Case ObjectType.BOOL_OBJ
                                                                          Return New Fox_Integer With {.Value = Evaluator.BoolToLong(args(0).Value)}

                                                                      Case ObjectType.STRING_OBJ
                                                                          Dim strObj = TryCast(args(0), Fox_String)
                                                                          Dim r = Nothing
                                                                          If Long.TryParse(strObj.Value, r) Then
                                                                              Return New Fox_Integer With {.Value = r}
                                                                          Else
                                                                              Return Evaluator.ThrowError($"无法转换 {strObj.Value} 为整数")
                                                                          End If
                                                                      Case ObjectType.NOTHINGL_OBJ
                                                                          Return Evaluator.Fox_Zero
                                                                      Case Else
                                                                          Return Evaluator.ThrowError($"CInt函数不支持{args(0).Type()}类型")
                                                                  End Select
                                                              End If

                                                              Return Evaluator.Fox_Nothing
                                                          End Function}},
        {"CBool", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                               Dim 所需实参数 = 1

                                                               If args.Count > 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               If args.Count < 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               If args.Count = 所需实参数 Then
                                                                   If args(0) Is Nothing Then Return Evaluator.ThrowError($"CInt函数不支{Evaluator.Fox_Nothing.Type.ToString}类型")

                                                                   Select Case TryCast(args(0), Fox_Object).Type
                                                                       Case ObjectType.INTEGER_OBJ, ObjectType.DOUBLE_OBJ
                                                                           Return New Fox_Bool With {.Value = args(0).Value <> 0}
                                                                       Case ObjectType.BOOL_OBJ
                                                                           Return args(0)
                                                                       Case ObjectType.STRING_OBJ
                                                                           Return If(TryCast(args(0), Fox_String).Value <> "", Evaluator.Fox_True, Evaluator.Fox_False)
                                                                       Case ObjectType.ARRAY_OBJ
                                                                           Return New Fox_Bool With {.Value = TryCast(args(0), Fox_Array).Elements.Any}
                                                                       Case ObjectType.DICTIONARY_OBJ
                                                                           Return New Fox_Bool With {.Value = TryCast(args(0), Fox_Dictionary).Pairs.Any}
                                                                       Case ObjectType.NOTHINGL_OBJ
                                                                           Return Evaluator.Fox_False
                                                                       Case Else
                                                                           Return Evaluator.ThrowError($"CInt函数不支持{args(0).Type()}类型")
                                                                   End Select
                                                               End If

                                                               Return Evaluator.Fox_Nothing
                                                           End Function}},
        {"CStr", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                              Dim 所需实参数 = 1

                                                              If args.Count > 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count < 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count = 所需实参数 Then
                                                                  Select Case args(0).Type()
                                                                      Case ObjectType.INTEGER_OBJ, ObjectType.DOUBLE_OBJ
                                                                          Return New Fox_String With {.Value = args(0).Value.ToString}
                                                                      Case ObjectType.BOOL_OBJ
                                                                          Return New Fox_String With {.Value = CStr(args(0).Value)}
                                                                      Case ObjectType.STRING_OBJ
                                                                          Return args(0)
                                                                      Case ObjectType.NOTHINGL_OBJ
                                                                          Return New Fox_String With {.Value = ""}
                                                                      Case Else
                                                                          Return Evaluator.ThrowError($"CStr函数不支持{args(0).Type()}类型")
                                                                  End Select
                                                              End If

                                                              Return Evaluator.Fox_Nothing
                                                          End Function}},
        {"CArray", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                                Dim 所需实参数 = 1

                                                                If args.Count > 所需实参数 Then
                                                                    Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                                End If

                                                                If args.Count < 所需实参数 Then
                                                                    Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                                End If

                                                                If args.Count = 所需实参数 Then
                                                                    Select Case args(0).Type()
                                                                        Case ObjectType.STRING_OBJ
                                                                            Dim stringObject = TryCast(args(0), Fox_String)
                                                                            Dim stringList = stringObject.Value.ToList

                                                                            Dim arrayObject As New Fox_Array With {.Elements = New List(Of Fox_Object)}
                                                                            For Each str As String In stringList
                                                                                arrayObject.Elements.Add(New Fox_String With {.Value = str})
                                                                            Next

                                                                            Return arrayObject
                                                                        Case ObjectType.ARRAY_OBJ
                                                                            Return args(0)
                                                                        Case ObjectType.NOTHINGL_OBJ
                                                                            Return New Fox_Array With {.Elements = New List(Of Fox_Object)}
                                                                        Case Else
                                                                            Return Evaluator.ThrowError($"CStr函数不支持{args(0).Type()}类型")
                                                                    End Select
                                                                End If

                                                                Return Evaluator.Fox_Nothing
                                                            End Function}},
        {"range", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                               Dim 所需实参数 = 1

                                                               If args.Count > 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               If args.Count < 所需实参数 Then
                                                                   Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                               End If

                                                               If args.Count = 所需实参数 Then
                                                                   Select Case args(0).Type()
                                                                       Case ObjectType.INTEGER_OBJ
                                                                           Dim arr = New Fox_Array With {.Elements = New List(Of Fox_Object)}
                                                                           Try
                                                                               For i = 0 To args(0).Value
                                                                                   arr.Elements.Add(New Fox_Integer With {.Value = i})
                                                                               Next
                                                                           Catch ex As OutOfMemoryException
                                                                               Return Evaluator.ThrowError($"内存溢出! {ex.Message}")
                                                                           End Try
                                                                           Return arr
                                                                       Case Else
                                                                           Return Evaluator.ThrowError($"range函数不支持{args(0).Type()}类型")
                                                                   End Select
                                                               End If

                                                               Return Evaluator.Fox_Nothing
                                                           End Function}},
        {"BubbleSort", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                                    Dim 所需实参数 = 1

                                                                    If args.Count > 所需实参数 Then
                                                                        Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                                    End If

                                                                    If args.Count < 所需实参数 Then
                                                                        Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                                    End If

                                                                    If args.Count = 所需实参数 Then
                                                                        Select Case args(0).Type()
                                                                            Case ObjectType.ARRAY_OBJ
                                                                                Dim arr = TryCast(args(0), Fox_Array)
                                                                                Dim numbers As New List(Of Long)

                                                                                For Each item As Fox_Object In arr.Elements
                                                                                    numbers.Add(CLng(builtinFuncs("CInt").BuiltinFunction({item}).Value.ToString))
                                                                                Next

                                                                                Dim sorted_numbers = BubbleSort(numbers.ToArray)
                                                                                Dim objects As New List(Of Fox_Object)

                                                                                For Each sorted_number As Long In sorted_numbers
                                                                                    objects.Add(New Fox_Integer With {.Value = New BigInteger(sorted_number)})
                                                                                Next

                                                                                Return New Fox_Array With {.Elements = objects}
                                                                            Case Else
                                                                                Return Evaluator.ThrowError($"BubbleSort函数不支持{args(0).Type()}类型")
                                                                        End Select
                                                                    End If

                                                                    Return Evaluator.Fox_Nothing
                                                                End Function}},
        {"CNothing", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                                  Dim 所需实参数 = 1

                                                                  If args.Count > 所需实参数 Then
                                                                      Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                                  End If

                                                                  If args.Count < 所需实参数 Then
                                                                      Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                                  End If

                                                                  Return Evaluator.Fox_Nothing
                                                              End Function}},
        {"Chr", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                             Dim 所需实参数 = 1

                                                             If args.Count > 所需实参数 Then
                                                                 Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                             End If

                                                             If args.Count < 所需实参数 Then
                                                                 Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                             End If

                                                             If args.Count = 所需实参数 Then
                                                                 Select Case args(0).Type()
                                                                     Case ObjectType.STRING_OBJ
                                                                         Return New Fox_String With {.Value = Chr(CLng(args(0).Value.ToString))}
                                                                     Case Else
                                                                         Return Evaluator.ThrowError($"BubbleSort函数不支持{args(0).Type()}类型")
                                                                 End Select
                                                             End If

                                                             Return Evaluator.Fox_Nothing
                                                         End Function}},
        {"ChrW", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                              Dim 所需实参数 = 1

                                                              If args.Count > 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"实参过多 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count < 所需实参数 Then
                                                                  Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                              End If

                                                              If args.Count = 所需实参数 Then
                                                                  Select Case args(0).Type()
                                                                      Case ObjectType.STRING_OBJ
                                                                          Return New Fox_String With {.Value = ChrW(CLng(args(0).Value.ToString))}
                                                                      Case Else
                                                                          Return Evaluator.ThrowError($"BubbleSort函数不支持{args(0).Type()}类型")
                                                                  End Select
                                                              End If

                                                              Return Evaluator.Fox_Nothing
                                                          End Function}}
    }
    Public Shared builtinVars As New Dictionary(Of String, Fox_Builtin) From
    {
        {"Crlf", New Fox_Builtin With {.BuiltinIdentifier = New Fox_String With {.Value = vbCrLf}}},
        {"NextLine", New Fox_Builtin With {.BuiltinIdentifier = New Fox_String With {.Value = vbCrLf}}},
        {"InterpreterPath", New Fox_Builtin With {.BuiltinIdentifier = New Fox_String With {.Value = AppPath}}},
        {"IncludesPath", New Fox_Builtin With {.BuiltinIdentifier = New Fox_String With {.Value = AppPath & "includes\"}}},
        {"Math_Module_Path", New Fox_Builtin With {.BuiltinIdentifier = New Fox_String With {.Value = AppPath & "includes\Math.Fox"}}},
        {"IO_Module_Path", New Fox_Builtin With {.BuiltinIdentifier = New Fox_String With {.Value = AppPath & "includes\IO.Fox"}}},
        {"Nothing", New Fox_Builtin With {.BuiltinIdentifier = New Fox_Nothing}}
    }
    Shared Function BubbleSort(arr() As Long) As Long()
        ' 创建原数组的副本
        Dim sortedArray As Long() = DirectCast(arr.Clone(), Long())
        Dim n As Long = sortedArray.Length
        Dim i As Long
        Dim j As Long
        Dim temp As Long

        For i = 0 To n - 1
            For j = 0 To n - i - 2
                If sortedArray(j) > sortedArray(j + 1) Then
                    ' 交换元素
                    temp = sortedArray(j)
                    sortedArray(j) = sortedArray(j + 1)
                    sortedArray(j + 1) = temp
                End If
            Next j
        Next i

        ' 返回排序后的新数组
        Return sortedArray
    End Function
End Class



