Public Class AST

    '=============================
    '========= VARIABLES =========
    '=============================
    Dim tokens As New List(Of token)
    Dim tok_index As Integer
    Dim current_tok As token
    Dim file As LSFile

    '===========================
    '========= ADVANCE =========
    '===========================
    Private Sub advance(Optional ByVal try_get As Boolean = False)
        tok_index += 1
        If tok_index < tokens.Count Then
            current_tok = tokens(tok_index)
        Else
            If Not try_get Then
                If current_tok Is Nothing Then
                    addBasicError("NPA01", "Something was expected in <" & file.name & ">")
                Else
                    addSyntaxError("NPA01", "Something was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
            End If
        End If
    End Sub

    '==========================
    '========= RECEDE =========
    '==========================
    Private Sub recede(ByVal index As Integer)
        tok_index = index
        If tok_index >= 0 And tok_index < tokens.Count Then
            current_tok = tokens(tok_index)
        Else
            addBasicError("Compileur problem", "Token undefined")
        End If
    End Sub

    '=========================
    '========= PARSE =========
    '=========================
    Public Sub parse(ByVal tokens As List(Of token), ByVal file As LSFile)

        'Reload informations
        Me.file = file
        Me.tokens.Clear()
        For Each tok As token In tokens
            Me.tokens.Add(tok)
        Next
        tok_index = -1
        advance()

        Dim main_func As New FunctionNode(-1, -1, "__main__", New List(Of FunctionArgument))
        main_func.parentNode = file

        While tok_index < tokens.Count - 1

            Dim obj As Node = func()
            If TypeOf obj Is FunctionNode Then
                obj.parentNode = file
                file.functions.Add(obj)
            Else
                obj.parentNode = main_func
                main_func.addNodeToCode(obj)
            End If

        End While

        file.functions.Add(main_func)

    End Sub

    '==========================
    '========= FACTOR =========
    '==========================
    Private Function factor() As Node

        Dim tok As token = current_tok

        If tok.type = tokenType.OP_PLUS Or tok.type = tokenType.OP_MINUS Then
            advance()
            Dim fac = factor()
            Return New UnaryOpNode(tok.positionStart, fac.positionEnd, tok, fac)

        ElseIf tok.type = tokenType.CT_INTEGER Or tok.type = tokenType.CT_FLOAT Then
            advance()
            Return New valueNode(tok.positionStart, tok.positionEnd, tok)

        ElseIf tok.type = tokenType.CT_STRING Then
            advance()
            Return New StringNode(tok.positionStart, tok.positionEnd, tok.value)

        ElseIf tok.type = tokenType.CT_TEXT Then
            advance()
            Return New VariableNode(tok.positionStart, tok.positionEnd, tok.value)

        ElseIf tok.type = tokenType.CT_TRUE Then
            advance()
            Return New BooleanNode(tok.positionStart, tok.positionEnd, True)

        ElseIf tok.type = tokenType.CT_FALSE Then
            advance()
            Return New BooleanNode(tok.positionStart, tok.positionEnd, False)

        ElseIf tok.type = tokenType.OP_LPAR Then
            advance()
            Dim com = boolOp()
            If current_tok.type = tokenType.OP_RPAR Then
                advance()
                Return com
            Else
                addSyntaxError("NPF02", "A parenthesis was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If

        ElseIf tok.type = tokenType.OP_LBRACKET Then
            advance()
            Dim list As New ListNode(tok.positionStart, 0)

            'Empty
            If current_tok.type = tokenType.OP_RBRACKET Then
                list.positionEnd = current_tok.positionEnd
                advance()
                Return list
            End If

            While True

                'Get value
                list.addElement(boolOp())

                'End list ?Q
                If current_tok.type = tokenType.OP_RBRACKET Then
                    list.positionEnd = current_tok.positionEnd
                    Exit While
                End If

                'Comma ?
                If current_tok.type = tokenType.OP_COMMA Then
                    advance()
                    Continue While
                End If

                'Error
                addSyntaxError("NPF03", "A comma or square bracket was expected here.", file, current_tok.positionStart, current_tok.positionEnd)

            End While
            advance()
            Return list

        End If

        addSyntaxError("NPF01", "Something else was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        Return Nothing

    End Function

    '================================
    '========= FunctionCall =========
    '================================
    Private Function functionCall() As Node

        'Start of function call
        If Not current_tok.type = tokenType.CT_TEXT Then

            'Something else
            Return factor()

        End If

        'Get start position
        Dim startPosition As Integer = current_tok.positionStart
        Dim recedeIndex As Integer = tok_index

        'Get content
        Dim functionPath As String = current_tok.value
        advance()

        'Check if it's a function call node
        If Not current_tok.type = tokenType.OP_LPAR Then
            recede(recedeIndex)
            Return factor()
        End If
        advance()

        'Get arguments
        Dim arguments As New List(Of Node)
        While True

            'End of function call
            If current_tok.type = tokenType.OP_RPAR Then
                Exit While
            End If

            'Get argument
            arguments.Add(boolOp())

            'Comma
            If current_tok.type = tokenType.OP_COMMA Then
                advance()
            ElseIf Not current_tok.type = tokenType.OP_RPAR Then
                addSyntaxError("ASTFC01", "A comma or a closing parenthesis must have been omitted here.", file, current_tok.positionStart, current_tok.positionEnd)
            End If

        End While

        'Get positionEnd
        Dim endPosition As Integer = current_tok.positionEnd
        advance()

        'Add node
        Return New FunctionCallNode(startPosition, endPosition, functionPath, arguments)

    End Function

    '====================================
    '========= BracketsSelector =========
    '====================================
    Private Function BracketsSelector() As Node

        Dim Target As Node = functionCall()
        While current_tok.type = tokenType.OP_LBRACKET
            advance()
            Dim Index As Node = functionCall()
            Target = New BracketsSelectorNode(Target.positionStart, Index.positionEnd, Target, Index)
            If Not current_tok.type = tokenType.OP_RBRACKET Then
                addSyntaxError("NPBS01", "A ""]"" was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()
        End While

        Return Target

    End Function

    '==============================
    '========= Child Node =========
    '==============================
    Private Function child() As Node

        Dim left = BracketsSelector()
        While current_tok.type = tokenType.OP_POINT
            advance()
            Dim right = BracketsSelector()
            left = New childNode(left.positionStart, right.positionEnd, left, right)
        End While

        Return left

    End Function

    '========================
    '========= TERM =========
    '========================
    Private Function term() As Node

        Dim left = child()
        While current_tok.type = tokenType.OP_MULTIPLICATION Or current_tok.type = tokenType.OP_DIVISION Or current_tok.type = tokenType.OP_MODULO
            Dim op = current_tok
            advance()
            Dim right = child()
            left = New binOpNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '========================
    '========= EXPR =========
    '========================
    Private Function expr() As Node

        Dim left As Node = term()
        While {tokenType.OP_PLUS, tokenType.OP_MINUS}.Contains(current_tok.type)
            Dim op As token = current_tok
            advance()
            Dim right As Node = term()
            left = New binOpNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '=================================
    '========= STRING CONCAT =========
    '=================================
    Private Function concat() As Node

        Dim left As Node = expr()
        While current_tok.type = tokenType.OP_CONCAT
            Dim op As token = current_tok
            advance()
            Dim right As Node = expr()
            left = New binOpNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '==============================
    '========= COMPARISON =========
    '==============================
    Private Function comparison() As Node

        Dim left As Node = concat()
        While {tokenType.OP_EQUAL, tokenType.OP_LESSTHAN, tokenType.OP_LESSTHANEQUAL, tokenType.OP_MORETHAN, tokenType.OP_MORETHANEQUAL, tokenType.OP_IN}.Contains(current_tok.type)
            Dim op As token = current_tok
            advance()
            Dim right As Node = concat()
            left = New ComparisonNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '===========================
    '========= BOOL OP =========
    '===========================
    Private Function boolOp() As Node

        Dim left As Node = comparison()
        While {tokenType.OP_AND, tokenType.OP_OR}.Contains(current_tok.type)
            Dim op As token = current_tok
            advance()
            Dim right As Node = comparison()
            left = New boolOpNode(left.positionStart, right.positionEnd, left, op, right)
        End While

        Return left

    End Function

    '========================
    '========= LINE =========
    '========================
    Private Function line(ByVal currentLineIndentation As Integer) As Node

        'It's another thing that a line
        If Not current_tok.type = tokenType.CT_LINESTART Then
            Return boolOp()
        End If

        'Get startPosition
        advance()
        Dim positionStart As Integer = current_tok.positionStart

        'Return statement
        If current_tok.type = tokenType.KW_RETURN Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart

            'Get value
            advance()
            Dim value As Node = boolOp()
            Return New ReturnNode(startPosition, value.positionEnd, value)

        End If

        'While loop
        If current_tok.type = tokenType.KW_WHILE Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart
            advance()

            'Get condition
            Dim condition As Node = boolOp()

            'Do
            If Not current_tok.type = tokenType.KW_DO Then
                addSyntaxError("NPL05", "A ""do"" keyword was exepted here.", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()

            'Create while object
            Dim while_statement As New whileStatementNode(positionStart, current_tok.positionEnd, condition)

            'Get lines
            While True

                If Not current_tok.type = tokenType.CT_LINESTART Then
                    addSyntaxError("NPL05", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                If addLineIndentation <= currentLineIndentation Then
                    Exit While
                End If

                while_statement.addNodeToCode(line(addLineIndentation))

            End While

            'Set position end
            while_statement.positionEnd = tokens(tok_index - 1).positionEnd

            'Return
            Return while_statement

        End If

        'For loop
        If current_tok.type = tokenType.KW_FOR Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart
            advance()

            'Get variable name
            If Not current_tok.type = tokenType.CT_TEXT Then
                addSyntaxError("NPF06", "A variable name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            Dim variableName As String = current_tok.value
            advance()

            'Get in
            If Not current_tok.type = tokenType.OP_IN Then
                addSyntaxError("NPF07", "A ""in"" token was expected here", file, current_tok.positionStart, current_tok.positionEnd, "for variable_name in a_list")
            End If
            advance()

            'Get target
            Dim target As Node = boolOp()

            'Do
            If Not current_tok.type = tokenType.KW_DO Then
                addSyntaxError("NPL05", "A ""do"" keyword was exepted here.", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()

            'Create for object
            Dim for_statement As New forStatementNode(positionStart, current_tok.positionEnd, target, variableName)

            'Get lines
            While True

                If Not current_tok.type = tokenType.CT_LINESTART Then
                    addSyntaxError("NPF08", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                If addLineIndentation <= currentLineIndentation Then
                    Exit While
                End If

                for_statement.addNodeToCode(line(addLineIndentation))

            End While

            'Set position end
            for_statement.positionEnd = tokens(tok_index - 1).positionEnd

            'Return
            Return for_statement

        End If

        'If statement
        If current_tok.type = tokenType.KW_IF Then

            'Variables
            Dim startPosition As Integer = current_tok.positionStart
            advance()

            'Get condition
            Dim condition As Node = boolOp()

            'Do
            If Not current_tok.type = tokenType.KW_DO Then
                addSyntaxError("NPL05", "A ""do"" keyword was exepted here.", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()

            'Get if lines
            Dim if_statements As New List(Of Node)
            While True

                If Not current_tok.type = tokenType.CT_LINESTART Then
                    addSyntaxError("NPF09", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                If addLineIndentation <= currentLineIndentation Then
                    Exit While
                End If

                if_statements.Add(line(addLineIndentation))

            End While

            'Else if
            Dim elseif_statement As New List(Of Tuple(Of Node, List(Of Node)))
            While current_tok.type = tokenType.CT_LINESTART

                'Advance
                advance(True)

                'Keyword
                If Not current_tok.type = tokenType.KW_ELSEIF Then
                    recede(tok_index - 1)
                    Exit While
                End If
                advance()

                'Get condition
                Dim elseif_condition As Node = boolOp()

                'Do
                If Not current_tok.type = tokenType.KW_DO Then
                    addSyntaxError("NPL05", "A ""do"" keyword was exepted here.", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                advance()

                'Get if lines
                Dim elseif_lines As New List(Of Node)
                While True

                    If Not current_tok.type = tokenType.CT_LINESTART Then
                        addSyntaxError("NPF10", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                    If addLineIndentation <= currentLineIndentation Then
                        Exit While
                    End If

                    elseif_lines.Add(line(addLineIndentation))

                End While

                'Add
                elseif_statement.Add(New Tuple(Of Node, List(Of Node))(elseif_condition, elseif_lines))

            End While

            'Else
            Dim recedeIndex As Integer = tok_index
            If Not current_tok.type = tokenType.CT_LINESTART Then
                addSyntaxError("NPL12", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance(True)

            'Else
            Dim else_statement As New List(Of Node)
            If current_tok.type = tokenType.KW_ELSE Then

                'Advance
                advance()

                'Do
                If Not current_tok.type = tokenType.KW_DO Then
                    addSyntaxError("NPL05", "A ""do"" keyword was exepted here.", file, current_tok.positionStart, current_tok.positionEnd)
                End If
                advance()

                'Get if lines
                While True

                    If Not current_tok.type = tokenType.CT_LINESTART Then
                        addSyntaxError("NPF11", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    Dim addLineIndentation As Integer = Convert.ToInt32(current_tok.value)

                    If addLineIndentation <= currentLineIndentation Then
                        Exit While
                    End If

                    else_statement.Add(line(addLineIndentation))

                End While

            Else

                recede(recedeIndex)

            End If

            'Return
            Return New ifStatementNode(positionStart, tokens(tok_index - 1).positionEnd, condition, if_statements, elseif_statement, else_statement)

        End If

        'Set variable
        If current_tok.type = tokenType.KW_SET Then

            'Get target
            advance()
            Dim target As Node = boolOp()

            'To
            If Not current_tok.type = tokenType.KW_TO Then
                addSyntaxError("ASTL01", "Missing ""to"" keyword after variable name", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()

            'New value
            Dim newValue As Node = boolOp()

            'Add node
            Return New SetVariableNode(positionStart, newValue.positionEnd, target, newValue)

        End If

        'Append
        If current_tok.type = tokenType.KW_APPEND Then

            'Get target
            advance()
            Dim value As Node = boolOp()

            'To
            If Not current_tok.type = tokenType.KW_TO Then
                addSyntaxError("ASTL02", "Missing ""to"" keyword after value", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            advance()

            'New value
            Dim target As Node = boolOp()

            'Add node
            Return New appendNode(positionStart, target.positionEnd, target, value)

        End If

        'Sub
        If current_tok.type = tokenType.CT_TEXT Then

            'Get name
            Dim name As String = current_tok.value
            Dim lastPos As Integer = current_tok.positionEnd
            advance()

            'Arguments
            Dim arguments As New List(Of Node)
            Dim firstArg As Boolean = True
            While True

                'Stop loop ?
                If current_tok.type = tokenType.CT_LINESTART Or current_tok.type = tokenType.KW_TO Then
                    Exit While
                End If

                'Comma
                If Not firstArg Then
                    If Not current_tok.type = tokenType.OP_COMMA Then
                        addSyntaxError("ASTL03", "Missing "","" to seperate arguments", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    advance()
                End If

                'FirstArg
                firstArg = False

                'Argument
                arguments.Add(boolOp())

            End While

            'To
            Dim target As Node = Nothing
            If current_tok.type = tokenType.KW_TO Then
                advance()
                target = boolOp()
            End If

            'Return
            Return New SubCall(positionStart, lastPos, name, arguments, target)

        End If

        'Unknow line type
        addSyntaxError("NPL01", "Unable to find line type", file, positionStart, current_tok.positionEnd)
        Return Nothing

    End Function

    '========================
    '========= FUNC =========
    '========================
    Private Function func() As Node

        'Check indentation
        Dim needToRecede As Boolean = False
        Dim funcIndentation As Integer = 0
        If current_tok.type = tokenType.CT_LINESTART Then
            funcIndentation = Convert.ToInt32(current_tok.value)
            advance()
            needToRecede = True
        End If

        'Check if node is func
        If Not current_tok.type = tokenType.KW_FUNC Then

            'It's another thing that a function
            If needToRecede = True Then
                recede(tok_index - 1)
            End If
            Return line(funcIndentation)

        End If

        'Save start pos
        Dim startPosition As Integer = current_tok.positionStart
        advance()

        'Get error
        If Not current_tok.type = tokenType.CT_TEXT Then
            addSyntaxError("NPFU01", "A name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Get name
        Dim name As String = current_tok.value
        advance()

        'Get arguments
        Dim arguments As New List(Of FunctionArgument)
        If current_tok.type = tokenType.OP_LPAR Then 'func Name(username:str, var id:int[])

            'First arg
            advance()

            'Direct ending ?
            If current_tok.type = tokenType.OP_RPAR Then

                'Direct ending
                advance()

            Else

                'Arguments in there
                While True

                    'Variables
                    Dim LastArgumentName As String = ""
                    Dim LastArgumentValue As Node = Nothing

                    'Search for a name
                    If Not current_tok.type = tokenType.CT_TEXT Then
                        addSyntaxError("NPF02", "A argument name was expected here", file, current_tok.positionStart, current_tok.positionEnd)
                    End If
                    LastArgumentName = current_tok.value
                    advance()

                    'Value
                    If current_tok.type = tokenType.OP_EQUAL Then
                        advance()
                        LastArgumentValue = boolOp()
                    End If

                    'Add argument
                    arguments.Add(New FunctionArgument(LastArgumentName, LastArgumentValue))

                    'Search for end
                    If current_tok.type = tokenType.OP_COMMA Then

                        advance()

                    ElseIf current_tok.type = tokenType.OP_RPAR Then

                        advance()
                        Exit While

                    Else

                        addSyntaxError("NPF04", "An end of parenthesis or a comma was expected here", file, current_tok.positionStart, current_tok.positionEnd)

                    End If

                End While

            End If

        End If

        'Create node
        Dim currentFunction As New FunctionNode(startPosition, startPosition + 1, name, arguments)

        'Get error
        If Not current_tok.type = tokenType.CT_LINESTART Then
            addSyntaxError("NPF02", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
        End If

        'Get codes
        While True

            If Not current_tok.type = tokenType.CT_LINESTART Then
                addSyntaxError("NPF05", "A newline was expected here", file, current_tok.positionStart, current_tok.positionEnd)
            End If
            Dim currentLineIndentation As Integer = Convert.ToInt32(current_tok.value)

            If currentLineIndentation <= funcIndentation Then
                Exit While
            End If

            currentFunction.addNodeToCode(line(currentLineIndentation))

        End While

        'Add node
        Return currentFunction

    End Function

End Class