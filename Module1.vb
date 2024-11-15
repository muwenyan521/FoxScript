Imports System.IO
Imports System.Text
Imports System.Windows.Forms

Module Module1

    Sub Main()
        '获取启动参数
        Dim cmd = My.Application.CommandLineArgs

        '没有提供文件路径 进入REPL
        If cmd.Count <= 0 Then
            Dim env As New Environment
            While True
                Console.Write(">>> ")
                Dim input = Console.ReadLine() & vbCrLf
                Dim lexer As New Lexer(input)
                Dim parser As New Parser(lexer)
                Dim evaluator As New Evaluator


                Dim program As Program = parser.ParseProgram
                Dim result As Fox_Object = evaluator.eval(program, env)
                If result IsNot Nothing Then
                    Console.WriteLine(result.Inspect)
                End If

            End While
        Else '提供了文件路径 解析文件中的代码
            If Not File.Exists(cmd(0).Replace("""", "")) Then Return '文件不存在就退出

            Dim text = ""
            Using sr As New StreamReader(cmd(0).Replace("""", ""), Encoding.UTF8)
                text = sr.ReadToEnd '读取文本
            End Using

            Dim lexer As New Lexer(text)
            Dim parser As New Parser(lexer)
            Dim evaluator As New Evaluator
            Dim env As New Environment


            '解析程序
            Dim program As Program = parser.ParseProgram()

            '遍历所有Statement
            For Each stmt As Statement In program.Statements
                '求值
                Dim r As Fox_Object = evaluator.eval(stmt, env)
                If r IsNot Nothing AndAlso r.Inspect IsNot Nothing Then
                    Console.WriteLine(r.Inspect)
                End If
            Next

        End If
    End Sub

End Module
