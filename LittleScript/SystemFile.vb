Imports System.IO

'==============================
'========== LIM FILE ==========
'==============================
Public Class LSFile
    Inherits Node

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public interpreter As interpreter
    Public name As String
    Public path As String
    Public content As String = ""

    Public FilesImports As New List(Of LSFile)

    Public variables As New List(Of Variable)
    Public functions As New List(Of FunctionNode)

    '=================================
    '========== CONSTRUCTOR ==========
    '=================================
    Public Sub New(ByVal path As String, ByRef interpreter As interpreter)

        'Mybase
        MyBase.New(0, 0)

        'Set path
        Me.path = path.Replace("\", "/")

        'Set name
        If Me.path.Contains("/") Then
            Me.name = Me.path.Substring(Me.path.LastIndexOf("/") + 1)
        Else
            Me.name = path
        End If

        'LimLib
        If Not path.EndsWith(".ls") Then
            addBasicError("Unsupported file type", "The """ & Me.name & """ file is not a ""lim"" source file")
        End If

        'Set compiler
        Me.interpreter = interpreter

        'No file
        If Not File.Exists(Me.path) Then
            addBasicError("file not found", "The file """ & Me.path & """ does not exist.")
        End If
        Try
            content = File.ReadAllText(Me.path)
        Catch ex As Exception
            addBasicError("Unable to read file", ex.Message)
        End Try

        'Compile tokens (Keywords)
        Dim tokens As List(Of token) = (New lexer()).parse(Me)

        'Nothing
        If tokens.Count = 0 Then
            Exit Sub
        End If

        'Handle import statement
        While True

            'Tokens count
            If Not tokens.Count > 2 Then
                Exit While
            End If

            'New line
            If Not tokens(0).type = tokenType.CT_LINESTART Then
                Exit While
            End If

            'Check if line is import
            If Not tokens(1).type = tokenType.KW_IMPORT Then
                Exit While
            End If

            'Remove tokens
            tokens.RemoveAt(0)
            tokens.RemoveAt(0)

            'Handle filename
            If tokens(0).type = tokenType.CT_STRING Then

                'Import file
                importFile(tokens(0).value)

            Else

                addSyntaxError("SFN01", "The keyword ""import"" is followed by the link to the file that you want to import.", Me, tokens(0).positionStart, tokens(0).positionEnd, "import ""my_file.ls""")
            End If

            'Remove tokens
            tokens.RemoveAt(0)

        End While

        'Generate AST (Abstract syntax tree)
        Dim parser As New AST()
        parser.parse(tokens, Me)

    End Sub

    '=================================
    '========== IMPORT FILE ==========
    '=================================
    Public Sub importFile(ByVal path As String)

        'Fix path
        If path.Contains("\") Then
            path = path.Replace("\", "/")
        End If

        'Search if file already imported
        For Each existingFile As LSFile In Me.interpreter.files
            If existingFile.path = path Then
                Me.FilesImports.Add(existingFile)
                Exit Sub
            End If
        Next

        'Get file
        Dim file As New LSFile(path, Me.interpreter)

        'Import file
        Me.interpreter.files.Add(file)

        'Add to file
        Me.FilesImports.Add(file)

    End Sub

End Class