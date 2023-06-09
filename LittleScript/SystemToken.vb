﻿'===========================
'========== TOKEN ==========
'===========================
Public Class token

    Public type As tokenType
    Public value As Object

    Public positionStart As Integer
    Public positionEnd As Integer

    Public Sub New(ByVal type As tokenType, ByVal positionStart As Integer, ByVal positionEnd As Integer, Optional ByVal value As String = Nothing)

        Me.type = type
        Me.value = value

        Me.positionStart = positionStart
        Me.positionEnd = positionEnd

    End Sub

    Public Overrides Function ToString() As String
        If value Is Nothing Then
            Return String.Format("[{0}]", Me.type.ToString())
        Else
            Return String.Format("[{0}, ""{1}""]", Me.type.ToString(), Me.value.ToString())
        End If
    End Function

End Class

'================================
'========== TOKEN TYPE ==========
'================================
Public Enum tokenType

    'Content (CT)
    CT_STRING
    CT_INTEGER
    CT_FLOAT
    CT_TEXT
    CT_TRUE
    CT_FALSE

    CT_LINESTART

    'Keywords (KW)
    KW_WHILE
    KW_FOR
    KW_DO
    KW_SET
    KW_TO
    KW_IF
    KW_ELSE
    KW_ELSEIF
    KW_APPEND

    KW_RETURN
    KW_FUNC

    KW_IMPORT

    'Operators (OP)
    OP_POINT
    OP_COMMA
    OP_LPAR
    OP_RPAR
    OP_LBRACKET
    OP_RBRACKET
    OP_LBRACE
    OP_RBRACE

    OP_PLUS
    OP_MINUS
    OP_MULTIPLICATION
    OP_DIVISION
    OP_MODULO
    OP_CONCAT

    OP_NOT
    OP_AND
    OP_OR
    OP_EQUAL
    OP_NOTEQUAL
    OP_LESSTHAN
    OP_LESSTHANEQUAL
    OP_MORETHAN
    OP_MORETHANEQUAL
    OP_IN

End Enum