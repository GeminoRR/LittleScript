Imports System.IO

'====================================
'========== LS INTERPRETER ==========
'====================================
Public Class interpreter

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public files As New List(Of LSFile)
    Private entryFile As LSFile

    '==============================
    '========== RUN CODE ==========
    '==============================
    Public Sub runCode(ByVal inputFile As String, ByVal workingDirectory As String, ByVal flags As List(Of String))

        'Get file
        entryFile = New LSFile(inputFile, Me)
        files.Add(entryFile)

        'Get entry point
        Dim mainCall As New FunctionCallNode(-1, -1, "__main__", New List(Of Node))
        mainCall.parentNode = entryFile
        executeFunction(mainCall)

    End Sub

    '==================================
    '========== GET FUNCTION ==========
    '==================================
    Private Function getFunction(ByVal fun As FunctionCallNode) As FunctionNode

        Dim result As FunctionNode = Nothing
        For Each fn As FunctionNode In entryFile.functions
            If fn.Name = fun.FunctionName Then
                result = fn
                Exit For
            End If
        Next

        If result IsNot Nothing Then
            For Each f As LSFile In files
                For Each fn As FunctionNode In f.functions
                    If fn.Name = fun.FunctionName Then
                        result = fn
                        Exit For
                    End If
                Next
            Next
        End If

        'Error
        If result Is Nothing Then
            addNodeNamingError("IGF01", "Function """ & fun.FunctionName & """ cannot be found", fun)
        End If

        'Arguments
        If fun.Arguments.Count < result.minArguments Then
            addNodeSyntaxError("IGF02", "Missing " & (result.minArguments - fun.Arguments.Count).ToString() & " arguments", fun)
        End If
        If fun.Arguments.Count > result.maxArguments Then
            addNodeSyntaxError("IGF02", "Two many arguments", fun)
        End If

        'Return
        Return result

    End Function

    '==================================
    '========== GET VARIABLE ==========
    '==================================
    Public Function getVariable(ByVal name As String, ByVal node As Node, Optional notThrowMessage As Boolean = False) As Variable

        'Search parent node
        Dim parentNode As Node = node
        While parentNode.parentNode IsNot Nothing

            parentNode = parentNode.parentNode
            If TypeOf parentNode Is containerNode Then

                Dim castedContainer As containerNode = DirectCast(parentNode, containerNode)
                For Each var As Variable In castedContainer.variables
                    If var.name = name Then
                        Return var
                    End If
                Next

            End If

        End While

        'Nothing
        If Not notThrowMessage Then
            addNodeNamingError("IGV01", "Variable cannot be found", node)
        End If
        Return Nothing

    End Function

    '======================================
    '========== EXECUTE FUNCTION ==========
    '======================================
    Private Function executeFunction(ByVal fun As FunctionCallNode)

        'FUNCTIONS
        Select Case fun.FunctionName

            Case "msg"
                If fun.Arguments.Count < 1 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 1 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Console.WriteLine(getValueOfType(Of String)(fun.Arguments(0)))

            Case "ask"
                If fun.Arguments.Count < 1 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 1 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Console.Write(getValueOfType(Of String)(fun.Arguments(0)))
                Return Console.ReadLine()

            Case "range"
                If fun.Arguments.Count < 1 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 2 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Dim result As New List(Of Object)
                If fun.Arguments.Count = 1 Then
                    Dim count As Object = getValueOfType(Of Integer)(fun.Arguments(0))
                    For i As Integer = 0 To count - 1
                        result.Add(i)
                    Next
                ElseIf fun.Arguments.Count = 2 Then
                    Dim start As Object = getValueOfType(Of Integer)(fun.Arguments(0))
                    Dim count As Object = getValueOfType(Of Integer)(fun.Arguments(1))
                    For i As Integer = start To count - 1
                        result.Add(i)
                    Next
                End If
                Return result

            Case "read"
                If fun.Arguments.Count < 1 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 1 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Try
                    Return File.ReadAllText(getValueOfType(Of String)(fun.Arguments(0)))
                Catch ex As Exception
                    addBasicError("Cannot read file", ex.Message)
                    Return ""
                End Try

            Case "write"
                If fun.Arguments.Count < 2 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 2 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Try
                    File.WriteAllText(getValueOfType(Of String)(fun.Arguments(0)), getValueOfType(Of String)(fun.Arguments(1)))
                Catch ex As Exception
                    addBasicError("Cannot write file", ex.Message)
                    Return ""
                End Try

            Case "cmd"
                If fun.Arguments.Count < 1 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 1 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Process.Start("cmd", "/c " & getValueOfType(Of String)(fun.Arguments(0)))

            Case "start"
                If fun.Arguments.Count < 1 Then
                    addNodeSyntaxError("IEF01", "Missing " & (1 - fun.Arguments.Count).ToString() & " arguments", fun)
                End If
                If fun.Arguments.Count > 2 Then
                    addNodeSyntaxError("IEF02", "Two many arguments", fun)
                End If
                Dim argument As String = ""
                If fun.Arguments.Count = 2 Then
                    argument = getValueOfType(Of String)(fun.Arguments(1))
                End If
                Process.Start(getValueOfType(Of String)(fun.Arguments(0)), argument)

            Case Else

                'Search fun
                Dim fn As FunctionNode = getFunction(fun)

                'Execute lines
                For Each line As Node In fn.codes
                    Dim result As Object = executeLine(line)
                    If result IsNot Nothing Then
                        Return result
                    End If
                Next
                Return Nothing

        End Select

        'Nothing
        Return Nothing

    End Function

    '==================================
    '========== SET VARIABLE ==========
    '==================================
    Private Sub setVariable(ByVal var As Node, ByVal newValue As Object)

        'Variables
        Dim target As Variable = Nothing

        'VariableNode
        If TypeOf var Is VariableNode Then

            'Cast
            Dim castedNode As VariableNode = DirectCast(var, VariableNode)

            'Get target
            target = getVariable(castedNode.VariableName, var, True)

            'No variable
            If target Is Nothing Then
                target = New Variable(castedNode.VariableName)
                getNodeParentContainer(var).variables.Add(target)
            End If

            'Change value
            target.value = newValue

            'Return
            Exit Sub

        End If

        'BracketSelectorNode
        If TypeOf var Is BracketsSelectorNode Then

            'Cast
            Dim castedNode As BracketsSelectorNode = DirectCast(var, BracketsSelectorNode)

            'Get index
            Dim index As Object = getValue(castedNode.index)

            'Get value
            Dim value As Object = getValue(castedNode.Target)

            'List
            If TypeOf value Is List(Of Object) Then

                'Error
                If Not TypeOf index Is Integer Then
                    addNodeTypeError("ISV02", "The index of a list has to be a integer", castedNode.index)
                End If

                'Get element
                DirectCast(value, List(Of Object))(DirectCast(index, Integer)) = newValue

            End If

            'Return
            Exit Sub

        End If

    End Sub

    '===========================================
    '========== CONVERT VALUE TO TYPE ==========
    '===========================================
    Private Function convertValueToType(Of wanted_type)(ByVal value As Object, ByVal node As Node) As wanted_type
        Return _convertValueToType(Of wanted_type)(value, node)
    End Function
    Private Function _convertValueToType(Of wanted_type)(ByVal value As Object, ByVal node As Node) As Object

        'Transform type
        Try

            'String
            If TypeOf value Is String Then

                If GetType(wanted_type) = GetType(String) Then
                    Return value
                ElseIf GetType(wanted_type) = GetType(Integer) Then
                    Return Convert.ToInt32(value)
                ElseIf GetType(wanted_type) = GetType(Double) Then
                    Return Convert.ToDouble(value)
                End If
                Throw New Exception()

            End If

            'Integer
            If TypeOf value Is Integer Then

                If GetType(wanted_type) = GetType(String) Then
                    Return value.ToString()
                ElseIf GetType(wanted_type) = GetType(Integer) Then
                    Return value
                ElseIf GetType(wanted_type) = GetType(Double) Then
                    Return Convert.ToDouble(value)
                ElseIf GetType(wanted_type) = GetType(Boolean) Then
                    If DirectCast(value, Integer) = 0 Then
                        Return True
                    Else
                        Return False
                    End If
                End If
                Throw New Exception()

            End If

            'Double
            If TypeOf value Is Double Then

                If GetType(wanted_type) = GetType(String) Then
                    Return value.ToString()
                ElseIf GetType(wanted_type) = GetType(Integer) Then
                    Return Convert.ToInt32(Math.Round(value))
                ElseIf GetType(wanted_type) = GetType(Double) Then
                    Return value
                ElseIf GetType(wanted_type) = GetType(Boolean) Then
                    If DirectCast(value, Double) = 0 Then
                        Return True
                    Else
                        Return False
                    End If
                End If
                Throw New Exception()

            End If

            'Boolean
            If TypeOf value Is Boolean Then

                If GetType(wanted_type) = GetType(String) Then
                    Return value.ToString()
                ElseIf GetType(wanted_type) = GetType(Integer) Then
                    If DirectCast(value, Boolean) Then
                        Return 0
                    Else
                        Return 1
                    End If
                ElseIf GetType(wanted_type) = GetType(Double) Then
                    If DirectCast(value, Boolean) Then
                        Return 0.0
                    Else
                        Return 0.0
                    End If
                ElseIf GetType(wanted_type) = GetType(Boolean) Then
                    Return value
                End If
                Throw New Exception()

            End If

            'List
            If TypeOf value Is List(Of Object) Then

                If GetType(wanted_type) = GetType(List(Of Object)) Then
                    Return value
                ElseIf GetType(wanted_type) = GetType(String) Then
                    Dim result As String = ""
                    For Each elm As Object In DirectCast(value, List(Of Object))
                        result &= ", " & convertValueToType(Of String)(elm, node)
                    Next
                    If result.StartsWith(", ") Then
                        result = result.Substring(2)
                    End If
                    Return "[" & result & "]"
                End If
                Throw New Exception()

            End If

        Catch ex As Exception
        End Try


        'Return
        addNodeTypeError("ICVTT01", "Unable to convert a value of type <" & value.GetType().Name & "> to <" & GetType(wanted_type).Name & ">", node)
        Return Nothing

    End Function

    '=======================================
    '========== GET VALUE OF TYPE ==========
    '=======================================
    Private Function getValueOfType(Of wanted_type)(ByVal node As Node) As wanted_type

        'Get value
        Dim value As Object = getValue(node)

        'Convert value
        Return convertValueToType(Of wanted_type)(value, node)

    End Function

    '===============================
    '========== GET VALUE ==========
    '===============================
    Private Function getValue(ByVal node As Node)

        'String
        If TypeOf node Is StringNode Then
            Return DirectCast(node, StringNode).value
        End If

        'Variable
        If TypeOf node Is VariableNode Then
            Return getVariable(DirectCast(node, VariableNode).VariableName, node).value
        End If

        'Function call
        If TypeOf node Is FunctionCallNode Then
            Return executeFunction(node)
        End If

        'Value
        If TypeOf node Is valueNode Then
            Dim castedNode As valueNode = DirectCast(node, valueNode)
            If castedNode.tok.type = tokenType.CT_FLOAT Then
                Return Convert.ToDouble(castedNode.tok.value)
            Else
                Return Convert.ToInt32(castedNode.tok.value)
            End If
        End If

        'Boolean
        If TypeOf node Is BooleanNode Then
            Dim castedNode As BooleanNode = DirectCast(node, BooleanNode)
            Return castedNode.value
        End If

        'List
        If TypeOf node Is ListNode Then
            Dim castedNode As ListNode = DirectCast(node, ListNode)
            Dim result As New List(Of Object)
            For Each elm As Node In castedNode.elements
                result.Add(getValue(elm))
            Next
            Return result
        End If

        'BracketSelector
        If TypeOf node Is BracketsSelectorNode Then

            'Cast
            Dim castedNode As BracketsSelectorNode = DirectCast(node, BracketsSelectorNode)

            'Get index
            Dim index As Object = getValue(castedNode.index)

            'Get value
            Dim value As Object = getValue(castedNode.Target)

            'List
            If TypeOf value Is List(Of Object) Then

                'Error
                If Not TypeOf index Is Integer Then
                    addNodeTypeError("IGV02", "The index of a list has to be a integer", castedNode.index)
                End If

                'Get element
                Return DirectCast(value, List(Of Object))(DirectCast(index, Integer))

            End If

            'Else
            addNodeTypeError("IGV03", "Aucun index ne peut être utiliser sur un éléments de type <" & value.GetType().Name & ">", castedNode.Target)
            Return Nothing

        End If

        'BinOpNode
        If TypeOf node Is binOpNode Then

            'Variables
            Dim castedNode As binOpNode = DirectCast(node, binOpNode)

            'Operator
            Select Case castedNode.op.type

                Case tokenType.OP_PLUS '() + ()
                    Return getValueOfType(Of Double)(castedNode.leftNode) + getValueOfType(Of Double)(castedNode.rightNode)

                Case tokenType.OP_MINUS '() - ()
                    Return getValueOfType(Of Double)(castedNode.leftNode) - getValueOfType(Of Double)(castedNode.rightNode)

                Case tokenType.OP_MULTIPLICATION '() * ()
                    Return getValueOfType(Of Double)(castedNode.leftNode) * getValueOfType(Of Double)(castedNode.rightNode)

                Case tokenType.OP_DIVISION '() / ()
                    Return getValueOfType(Of Double)(castedNode.leftNode) / getValueOfType(Of Double)(castedNode.rightNode)

                Case tokenType.OP_CONCAT '() & ()
                    Return getValueOfType(Of String)(castedNode.leftNode) & getValueOfType(Of String)(castedNode.rightNode)

            End Select

        End If

        'Nothing
        Return Nothing

    End Function

    '==================================
    '========== EXECUTE LINE ==========
    '==================================
    Private Function executeLine(ByVal line As Node)

        'Sub
        If TypeOf line Is SubCall Then
            executeSubcall(DirectCast(line, SubCall))
        End If

        'Set
        If TypeOf line Is SetVariableNode Then
            executeSet(DirectCast(line, SetVariableNode))
        End If

        'For
        If TypeOf line Is forStatementNode Then
            Return executeFor(DirectCast(line, forStatementNode))
        End If

        'Append
        If TypeOf line Is appendNode Then
            executeAppend(DirectCast(line, appendNode))
        End If

        'Return
        If TypeOf line Is ReturnNode Then
            Return executeReturn(DirectCast(line, ReturnNode))
        End If

        'Return
        Return Nothing

    End Function

    '====================================
    '========== EXECUTE RETURN ==========
    '====================================
    Private Function executeReturn(ByVal node As ReturnNode)
        Return getValue(node.value)
    End Function

    '====================================
    '========== EXECUTE APPEND ==========
    '====================================
    Private Sub executeAppend(ByVal node As appendNode)

        'Get target
        getValueOfType(Of List(Of Object))(node.Target).Add(getValue(node.value))

    End Sub

    '=================================
    '========== EXECUTE FOR ==========
    '=================================
    Private Function executeFor(ByVal node As forStatementNode)

        'Get enumerator
        Dim preLooper As Object = getValue(node.looperTarget)

        'Error
        If Not (TypeOf preLooper Is List(Of Object)) Then
            addNodeTypeError("IEF01", "Cannot iterate in a <" & preLooper.GetType().Name & "> object", node.looperTarget)
        End If
        Dim looper As List(Of Object) = DirectCast(preLooper, List(Of Object))

        'Add variable
        Dim var As New Variable(node.variableName)
        node.variables.Add(var)

        'For each
        For Each elm As Object In looper
            var.value = elm
            For Each line As Node In node.codes
                Dim result As Object = executeLine(line)
                If result IsNot Nothing Then
                    Return result
                End If
            Next
        Next

        'Return
        Return Nothing

    End Function

    '=================================
    '========== EXECUTE SET ==========
    '=================================
    Public Function executeSet(ByVal node As SetVariableNode)

        'Variable
        setVariable(node.Target, getValue(node.NewValue))

        'Return
        Return Nothing

    End Function

    '=====================================
    '========== EXECUTE SUBCALL ==========
    '========= ============================
    Private Function executeSubcall(ByVal node As SubCall)
        Dim tempFunCall As New FunctionCallNode(node.positionStart, node.positionEnd, node.SubName, node.Arguments)
        tempFunCall.parentNode = node
        Dim result As Object = executeFunction(tempFunCall)
        If node.target IsNot Nothing Then
            If result Is Nothing Then
                addNodeTypeError("IEL01", "The sub hasn't returned any values", node)
            End If
            setVariable(node.target, result)
        End If
        Return Nothing
    End Function


End Class
