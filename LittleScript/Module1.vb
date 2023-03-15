Module Module1

    Sub Main(args As String())

        'Variables
        Dim keywords As New List(Of String)
        Dim flags As New List(Of String)

        'Selection
        For Each arg As String In args
            If arg.StartsWith("-") Then
                flags.Add(arg)
            Else
                keywords.Add(arg)
            End If
        Next

        'Error
        If keywords.Count = 0 Then
            showHelp()
            endApp()
        End If

        'Interpreter
        Dim interpreter As New interpreter()
        interpreter.runCode(keywords(0), "", flags)

        'End
        endApp()

    End Sub

    Public Sub showHelp()
        Console.ForegroundColor = ConsoleColor.DarkGreen
        Console.WriteLine("ls <filename> [flags]")
        Console.ResetColor()
        Console.WriteLine("<filename>" & vbTab & ": Path to the script file")
        Console.WriteLine("[flags]" & vbTab & vbTab & ": List of flags")
        Console.WriteLine(vbTab & "-d" & vbTab & ": Debug, show log")
    End Sub

    Public Sub endApp()
        Console.ReadKey()
        End
    End Sub

End Module
