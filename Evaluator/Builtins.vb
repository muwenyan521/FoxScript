Public Class Builtins
    '{"input", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
    '                                                       Dim 所需实参数 = 1

    '                                                       If args.Count > 所需实参数 Then
    '                                                           Return Evaluator.ThrowError($"实参过多")
    '                                                       End If

    '                                                       If args.Count < 所需实参数 Then
    '                                                           Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
    '                                                       End If

    '                                                       If args.Count = 所需实参数 Then
    '                                                           Console.Write(args(0).Value)
    '                                                           Dim r = Console.ReadLine
    '                                                           Return New Fox_String With {.Value = r}
    '                                                       End If

    '                                                       Return Evaluator.Fox_Nothing
    '                                                   End Function}},

    Public Shared builtinFuncs As New Dictionary(Of String, Fox_Builtin) From
    {
        {"len", New Fox_Builtin With {.BuiltinFunction = Function(args As IEnumerable(Of Object))
                                                             Dim 所需实参数 = 1

                                                             If args.Count > 所需实参数 Then
                                                                 Return Evaluator.ThrowError($"实参过多")
                                                             End If

                                                             If args.Count < 所需实参数 Then
                                                                 Return Evaluator.ThrowError($"提供的实参过少 实参{args.Count}个 形参{所需实参数}个")
                                                             End If

                                                             If args.Count = 所需实参数 Then
                                                                 Select Case args(0).GetType()
                                                                     Case GetType(Fox_String)
                                                                         Return New Fox_Integer With {.Value = CLng(args(0).Value.ToString.Count)}
                                                                     Case GetType(Fox_Array)
                                                                         Return New Fox_Integer With {.Value = CLng(TryCast(args(0), Fox_Array).Elements.Count)}
                                                                     Case Else
                                                                         Return Evaluator.ThrowError($"len函数不支持{args(0).Type()}类型")
                                                                 End Select
                                                             End If
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
                                                                   Try
                                                                       Console.WriteLine(args(0).Inspect)
                                                                   Catch ex As StackOverflowException
                                                                       Return Evaluator.ThrowError($"栈溢出! 请检查您的代码")
                                                                   End Try
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
                                                                  Select Case args(0).GetType()
                                                                      Case GetType(Fox_Integer), GetType(Fox_Double)
                                                                          Return New Fox_Double With {.Value = CDbl(args(0).Value)}
                                                                      Case GetType(Fox_Bool)
                                                                          Return New Fox_Double With {.Value = CDbl(Evaluator.BoolToLong(args(0).Value))}
                                                                      Case GetType(Fox_String)
                                                                          Dim strObj = TryCast(args(0), Fox_String)
                                                                          Dim r = Nothing
                                                                          If Double.TryParse(strObj.Value, r) Then
                                                                              Return New Fox_Double With {.Value = r}
                                                                          Else
                                                                              Return Evaluator.ThrowError($"无法转换 {strObj.Value} 为整数")
                                                                          End If
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
                                                                  Select Case args(0).GetType()
                                                                      Case GetType(Fox_Integer), GetType(Fox_Double)
                                                                          Return New Fox_Integer With {.Value = CLng(args(0).Value)}
                                                                      Case GetType(Fox_Bool)
                                                                          Return New Fox_Integer With {.Value = Evaluator.BoolToLong(args(0).Value)}
                                                                      Case GetType(Fox_String)
                                                                          Dim strObj = TryCast(args(0), Fox_String)
                                                                          Dim r = Nothing
                                                                          If Long.TryParse(strObj.Value, r) Then
                                                                              Return New Fox_Integer With {.Value = r}
                                                                          Else
                                                                              Return Evaluator.ThrowError($"无法转换 {strObj.Value} 为整数")
                                                                          End If
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
                                                                  Select Case args(0).GetType()
                                                                      Case GetType(Fox_Integer), GetType(Fox_Double)
                                                                          Return New Fox_String With {.Value = CStr(args(0).Value)}
                                                                      Case GetType(Fox_Bool)
                                                                          Return New Fox_String With {.Value = CStr(args(0).Value)}
                                                                      Case GetType(Fox_String)
                                                                          Return args(0)
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
                                                                   Select Case args(0).GetType()
                                                                       Case GetType(Fox_Integer)
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
                                                           End Function}}
    }

End Class
