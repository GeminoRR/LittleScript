'================================
'========= SET VARIABLE =========
'================================
Public Class SetVariableNode
    Inherits Node

    'Variable
    Public Target As Node
    Public NewValue As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Target As Node, ByVal NewValue As Node)
        MyBase.New(positionStart, positionEnd)
        Me.Target = Target
        Me.Target.parentNode = Me
        Me.NewValue = NewValue
        Me.NewValue.parentNode = Me
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "set " & Target.ToString() & " to " & NewValue.ToString()
    End Function

End Class

'==========================
'========= APPEND =========
'==========================
Public Class appendNode
    Inherits Node

    'Variable
    Public Target As Node
    Public value As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Target As Node, ByVal value As Node)
        MyBase.New(positionStart, positionEnd)
        Me.Target = Target
        Me.Target.parentNode = Me
        Me.value = value
        Me.value.parentNode = Me
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "append " & value.ToString() & " to " & Target.ToString()
    End Function

End Class

'===================================
'========= WHILE STATEMENT =========
'===================================
Public Class whileStatementNode
    Inherits containerNode

    'Variable
    Public condition As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal condition As Node)
        MyBase.New(positionStart, positionEnd)
        Me.condition = condition
        Me.condition.parentNode = Me
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "(While " & condition.ToString() & " Do " & Me.codes.Count.ToString() & " things)"
    End Function

End Class

'=================================
'========= FOR STATEMENT =========
'=================================
Public Class forStatementNode
    Inherits containerNode

    'Variable
    Public looperTarget As Node
    Public variableName As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal looperTarget As Node, ByVal variableName As String)
        MyBase.New(positionStart, positionEnd)
        Me.looperTarget = looperTarget
        Me.looperTarget.parentNode = Me
        Me.variableName = variableName
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "(For " & variableName & " in (" & looperTarget.ToString() & ") do " & Me.codes.Count.ToString() & " things)"
    End Function

End Class

'================================
'========= IF STATEMENT =========
'================================
Public Class ifStatementNode
    Inherits containerNode

    'Variable
    Public condition As Node
    Public if_statements As List(Of Node)
    Public elseif_statements As List(Of Tuple(Of Node, List(Of Node)))
    Public else_statement As List(Of Node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, condition As Node, ByVal if_statement As List(Of Node), ByVal elseif_statements As List(Of Tuple(Of Node, List(Of Node))), ByVal else_statement As List(Of Node))
        MyBase.New(positionStart, positionEnd)

        Me.condition = condition
        Me.condition.parentNode = Me

        Me.if_statements = if_statement
        For Each statement As Node In Me.if_statements
            statement.parentNode = Me
        Next

        Me.elseif_statements = elseif_statements
        For Each statement As Tuple(Of Node, List(Of Node)) In Me.elseif_statements
            statement.Item1.parentNode = Me
            For Each statement2 As Node In statement.Item2
                statement2.parentNode = Me
            Next
        Next

        Me.else_statement = else_statement
        For Each statement As Node In Me.else_statement
            statement.parentNode = Me
        Next

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "(If " & Me.condition.ToString() & " Do ...)"
    End Function

End Class

'==============================
'========== FUNCTION ==========
'==============================
Public Class FunctionNode
    Inherits containerNode

    'Variable
    Public Name As String
    Public Arguments As List(Of FunctionArgument)
    Public maxArguments As Integer
    Public minArguments As Integer

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Name As String, ByVal Arguments As List(Of FunctionArgument))
        MyBase.New(positionStart, positionEnd)
        Me.Name = Name
        Me.Arguments = Arguments
        Me.maxArguments = Me.Arguments.Count
        Me.minArguments = 0
        Dim lastArgWasOptional As Boolean = False
        For Each arg As FunctionArgument In Me.Arguments
            If arg.value IsNot Nothing Then
                arg.value.parentNode = Me
                lastArgWasOptional = True
            ElseIf lastArgWasOptional Then
                addNodeSyntaxError("NFN01", "A non-optional argument cannot follow an optional argument", Me, "Put optional arguments at the end of the argument list.")
            Else
                Me.minArguments += 1
            End If
        Next
    End Sub

    'ToString
    Public Overrides Function ToString() As String

        'Argument
        Dim ATS As String = ""
        If Arguments.Count > 0 Then
            For Each arg As FunctionArgument In Arguments
                ATS &= ", " & arg.ToString()
            Next
            ATS = ATS.Substring(2)
        End If

        'Actions
        Dim LTS As String = " = *" & Me.codes.Count.ToString() & " elements*"

        'Return
        Return "(" & Name & "(" & ATS & ")" & LTS & ")"

    End Function

End Class
Public Class FunctionArgument

    Public name As String
    Public value As Node
    Public Sub New(ByVal name As String, Optional value As Node = Nothing)
        Me.name = name
        Me.value = value
    End Sub

    Public Overrides Function ToString() As String
        Dim strValue As String = ""
        If value IsNot Nothing Then
            strValue &= " = " & value.ToString()
        End If
        Return name & strValue
    End Function

End Class

'==============================
'========= ReturnNode =========
'==============================
Public Class ReturnNode
    Inherits Node

    'Variables
    Public value As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal node As Node)

        MyBase.New(positionStart, positionEnd)
        Me.value = node
        Me.value.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("return {0}", value.ToString())
    End Function

End Class

'========================================
'========= BracketsSelectorNode =========
'========================================
Public Class BracketsSelectorNode
    Inherits Node

    'Variable
    Public Target As Node
    Public index As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal Target As Node, ByVal index As Node)

        MyBase.New(positionStart, positionEnd)
        Me.Target = Target
        Me.Target.parentNode = Me
        Me.index = index
        Me.index.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0}[{1}])", Target.ToString(), index.ToString())
    End Function

End Class

'============================
'========= ListNode =========
'============================
Public Class ListNode
    Inherits Node

    'Variable
    Public elements As New List(Of Node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer)

        MyBase.New(positionStart, positionEnd)

    End Sub

    'Add element
    Public Sub addElement(ByVal elm As Node)
        elm.parentNode = Me
        elements.Add(elm)
    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim elementToString As String = ""
        For Each elm As Node In elements
            elementToString &= ", " & elm.ToString()
        Next
        If elementToString.StartsWith(", ") Then
            elementToString = elementToString.Substring(2)
        End If
        Return "[" & elementToString & "]"
    End Function

End Class

'====================================
'========= FunctionCallNode =========
'====================================
Public Class FunctionCallNode
    Inherits Node

    'Variable
    Public FunctionName As String
    Public Arguments As New List(Of Node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal FunctionName As String, ByVal Arguments As List(Of Node))

        MyBase.New(positionStart, positionEnd)
        Me.FunctionName = FunctionName
        For Each Arg As Node In Arguments
            Arg.parentNode = Me
            Me.Arguments.Add(Arg)
        Next

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim ATS As String = ""
        For Each arg As Node In Arguments
            ATS &= ", " & arg.ToString()
        Next
        If ATS.StartsWith(", ") Then
            ATS = ATS.Substring(2)
        End If
        Return FunctionName.ToString & "(" & ATS & ")"
    End Function

End Class

'============================
'========= SUB CALL =========
'============================
Public Class SubCall
    Inherits Node

    'Variable
    Public SubName As String
    Public Arguments As New List(Of Node)
    Public target As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal SubName As String, ByVal Arguments As List(Of Node), ByVal target As Node)

        MyBase.New(positionStart, positionEnd)
        Me.SubName = SubName
        For Each Arg As Node In Arguments
            Arg.parentNode = Me
            Me.Arguments.Add(Arg)
        Next
        Me.target = target
        If target IsNot Nothing Then
            Me.target.parentNode = Me
        End If

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim ATS As String = ""
        For Each arg As Node In Arguments
            ATS &= ", " & arg.ToString()
        Next
        If ATS.StartsWith(", ") Then
            ATS = ATS.Substring(2)
        End If
        Return SubName.ToString & "(" & ATS & ")"
    End Function

End Class

'================================
'========= VariableNode =========
'================================
Public Class VariableNode
    Inherits Node

    'Variable
    Public VariableName As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal VariableName As String)

        MyBase.New(positionStart, positionEnd)
        Me.VariableName = VariableName

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return VariableName.ToString()
    End Function

End Class

'==================================
'========= ComparisonNode =========
'==================================
Public Class ComparisonNode
    Inherits Node

    'Variable
    Public leftNode As Node
    Public op As token
    Public rightNode As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal leftNode As Node, ByVal op As token, ByVal rightNode As Node)

        MyBase.New(positionStart, positionEnd)
        Me.leftNode = leftNode
        Me.leftNode.parentNode = Me
        Me.op = op
        Me.rightNode = rightNode
        Me.rightNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1} {2})", leftNode, op, rightNode)
    End Function

End Class

'===============================
'========= BooleanNode =========
'===============================
Public Class BooleanNode
    Inherits Node

    'Variable
    Public value As Boolean

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As Boolean)

        MyBase.New(positionStart, positionEnd)
        Me.value = value

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return value.ToString()
    End Function

End Class

'==============================
'========= StringNode =========
'==============================
Public Class StringNode
    Inherits Node

    'Variables
    Public value As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As String)

        MyBase.New(positionStart, positionEnd)
        Me.value = value

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return """" & value & """"
    End Function

End Class

'=================================
'========= AddSourceNode =========
'=================================
Public Class AddSourceNode
    Inherits Node

    'Variables
    Public value As String

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal value As String)

        MyBase.New(positionStart, positionEnd)
        Me.value = value

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return "<""" & value & """>"
    End Function

End Class

'=============================
'========= ValueNode =========
'=============================
Public Class valueNode
    Inherits Node

    'Variables
    Public tok As token

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal tok As token)

        MyBase.New(positionStart, positionEnd)
        Me.tok = tok

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Select Case tok.type
            Case tokenType.CT_FLOAT
                If tok.value.Contains(".") Then
                    Return tok.value
                Else
                    Return tok.value & ".0"
                End If
            Case tokenType.CT_INTEGER
                Return tok.value
            Case Else
                Return "" & tok.ToString() & ""
        End Select
    End Function

End Class

'===============================
'========= UnaryOpNode =========
'===============================
Public Class UnaryOpNode
    Inherits Node

    'Variables
    Public op As token
    Public node As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal op As token, ByVal node As Node)

        MyBase.New(positionStart, positionEnd)
        Me.node = node
        Me.node.parentNode = Me
        Me.op = op

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1})", op, node)
    End Function

End Class


'=============================
'========= BinOpNode =========
'=============================
Public Class binOpNode
    Inherits Node

    'Variables
    Public leftNode As Node
    Public op As token
    Public rightNode As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal leftNode As Node, ByVal op As token, ByVal rightNode As Node)

        MyBase.New(positionStart, positionEnd)
        Me.leftNode = leftNode
        Me.leftNode.parentNode = Me
        Me.op = op
        Me.rightNode = rightNode
        Me.rightNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1} {2})", leftNode, op, rightNode)
    End Function

End Class

'==============================
'========= BoolOpNode =========
'==============================
Public Class boolOpNode
    Inherits Node

    'Variables
    Public leftNode As Node
    Public op As token
    Public rightNode As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal leftNode As Node, ByVal op As token, ByVal rightNode As Node)

        MyBase.New(positionStart, positionEnd)
        Me.leftNode = leftNode
        Me.leftNode.parentNode = Me
        Me.op = op
        Me.rightNode = rightNode
        Me.rightNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0} {1} {2})", leftNode, op, rightNode)
    End Function

End Class

'==============================
'========= Child Node =========
'==============================
Public Class childNode
    Inherits Node

    'Variables
    Public parentStruct As Node
    Public childNode As Node

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal left As Node, ByVal right As Node)

        MyBase.New(positionStart, positionEnd)

        Me.parentStruct = left
        Me.parentStruct.parentNode = Me

        Me.childNode = right
        Me.childNode.parentNode = Me

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Return String.Format("({0}.{1})", parentStruct.ToString(), childNode.ToString())
    End Function

End Class

'============================
'========= New Node =========
'============================
Public Class newNode
    Inherits Node

    'Variables
    Public className As String
    Public arguments As List(Of Node)

    'New
    Public Sub New(ByVal positionStart As Integer, ByVal positionEnd As Integer, ByVal functionCall As FunctionCallNode)

        MyBase.New(positionStart, positionEnd)
        className = functionCall.FunctionName
        arguments = functionCall.Arguments
        For Each arg As Node In arguments
            arg.parentNode = Me
        Next

    End Sub

    'ToString
    Public Overrides Function ToString() As String
        Dim argSTR As String = ""
        For Each arg As Node In arguments
            argSTR &= ", " & arg.ToString()
        Next
        If argSTR.StartsWith(", ") Then
            argSTR = argSTR.Substring(2)
        End If
        Return String.Format("(New {0}({1}))", className, argSTR)
    End Function

End Class