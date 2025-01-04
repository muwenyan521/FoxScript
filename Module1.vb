Imports System.IO
Imports System.Text
Imports System.Windows.Forms

Public Class Runner
    Private AppPath = $"{Application.StartupPath}\"
    Public Mode = "REPL"
    Public FilePath As String = ""

    Public Shared Function Eval(code As String, env As Environment)
        Dim lexer = New Lexer(code)
        Dim parser = New Parser(lexer)
        Dim analyzer = New SemanticAnalyzer()
        Dim evaluator As New Evaluator

        Dim program As Program = parser.ParseProgram
        If parser.CheckParserErrors Then Return Nothing

        analyzer.Analysis(program)
        If analyzer.CheckErrors Then Return Nothing

        Dim result As Fox_Object = evaluator.Eval(program, env)
        Return result
    End Function

    Public Sub Run()
        Dim env As New Environment
        env.SetValue(
            "ModulePath",
            Eval($"[{$"""{AppPath}\includes\"}""]", env),
            True
        )

        Dim evaluator As New Evaluator
        Dim lexer As Lexer
        Dim parser As Parser
        Dim analyzer As SemanticAnalyzer

        If Mode = "REPL" Then
            Dim art As String =
                "  _____                    ____                  _           _   " & vbCrLf &
                " |  ___|   ___   __  __   / ___|    ___   _ __  (_)  _ __   | |_ " & vbCrLf &
                " | |_     / _ \  \ \/ /   \___ \   / __| | '__| | | | '_ \  | __|" & vbCrLf &
                " |  _|   | (_) |  >  <     ___) | | (__  | |    | | | |_) | | |_ " & vbCrLf &
                " |_|      \___/  /_/\_\   |____/   \___| |_|    |_| | .__/   \__|" & vbCrLf &
                "                                                    |_|           " & vbCrLf
            Console.WriteLine(art)

            While True
                Console.Write(">>> ")
                Dim input = Console.ReadLine() & vbCrLf
                Dim multiline_result = ReadMultiLineInput(input)
                If multiline_result <> "" Then input = multiline_result

                Dim result = Eval(input, env)
                If result IsNot Nothing Then
                    Console.WriteLine(result.Inspect)
                End If
            End While
        Else
            If Not File.Exists(FilePath.Replace("""", "")) Then
                Console.WriteLine($"文件 {FilePath} 不存在!")

                Return '文件不存在就退出
            End If

            Dim text = ""
            Using sr As New StreamReader(FilePath.Replace("""", ""), Encoding.UTF8)
                text = sr.ReadToEnd '读取文本
            End Using

            lexer = New Lexer(text)
            parser = New Parser(lexer)
            analyzer = New SemanticAnalyzer()

            '解析程序
            Dim program As Program = parser.ParseProgram()
            If parser.CheckParserErrors Then Return

            analyzer.Analysis(program)
            If analyzer.CheckErrors Then Return

            Dim result As Fox_Object = evaluator.Eval(program, env)
            If TypeOf result Is Fox_Error Then
                Console.WriteLine(result.Inspect)
            End If
        End If
    End Sub


    Private Function GetTokenTypes(Tokens As List(Of Token))
        Dim TokenTypes As New List(Of TokenType)

        If Tokens Is Nothing Then
            Return TokenTypes
        End If

        For Each tkn In Tokens
            TokenTypes.Add(tkn.TokenType)
        Next

        Return TokenTypes
    End Function
    Private Function ReadMultiLineInput(Line As String) As String
        Dim Code = ""

        Dim Tokens As List(Of Token) = New Lexer(Line).Lexer
        Dim TokenTypes As List(Of TokenType) = GetTokenTypes(Tokens)
        Dim TokenTypePos = 0
        Dim CurTokenType As TokenType = TokenTypes(TokenTypePos)

        Dim MultiLine_TokenType_Dictionary As New Dictionary(Of TokenType, TokenType) From
        {
            {TokenType.IF_, TokenType.ENDIF_},
            {TokenType.WHILE_, TokenType.ENDWHILE},
            {TokenType.FOR_, TokenType.NEXT_},
            {TokenType.FUNC, TokenType.ENDFUNC},
            {TokenType.CLASS_, TokenType.ENDCLASS}
        }

        For Each Start_TokenType As TokenType In MultiLine_TokenType_Dictionary.Keys
            If Not TokenTypes.Contains(Start_TokenType) Then Continue For
            If TokenTypes.Contains(Start_TokenType) AndAlso TokenTypes.Contains(MultiLine_TokenType_Dictionary(Start_TokenType)) Then Continue For

            Dim Lines As New List(Of String)
            Dim input = ""

            Lines.Add(Line)
            While CurTokenType <> MultiLine_TokenType_Dictionary(Start_TokenType)
                Console.Write("... ")
                input = Console.ReadLine()
                Lines.Add(input)

                Tokens = New Lexer(input).Lexer
                TokenTypes = GetTokenTypes(Tokens)

                CurTokenType = TokenTypes(0)
            End While


            Code = Strings.Join(Lines.ToArray, vbCrLf)
        Next


        Return Code
    End Function


    Public Function RunModule(ByRef env As Environment)
        Dim evaluator As New Evaluator
        Dim lexer As Lexer
        Dim parser As Parser

        If Not File.Exists(FilePath.Replace("""", "")) Then Return Nothing '文件不存在就退出

        Dim text = ""
        Using sr As New StreamReader(FilePath.Replace("""", ""), Encoding.UTF8)
            text = sr.ReadToEnd '读取文本
        End Using

        lexer = New Lexer(text)
        parser = New Parser(lexer)

        '解析程序
        Dim program As Program = parser.ParseProgram()
        If parser.CheckParserErrors Then
            Return Nothing
        End If

        Dim result As Fox_Object = evaluator.Eval(program, env)
        If result IsNot Nothing Then
            Console.WriteLine(result.Inspect)
        End If

        Return program.Statements
    End Function
End Class

Module Module1

    Sub Main()
        Console.OutputEncoding = Encoding.UTF8
        Console.InputEncoding = Encoding.UTF8

        '获取启动参数
        Dim cmd = My.Application.CommandLineArgs

        '没有提供文件路径 进入REPL
        If cmd.Count <= 0 Then
            Dim runner As New Runner With {.Mode = "REPL"}
            runner.Run()
        Else '提供了文件路径 解析文件中的代码
            Dim runner As New Runner With {.Mode = "FILE", .FilePath = cmd(0)}
            runner.Run()
        End If
    End Sub

End Module
