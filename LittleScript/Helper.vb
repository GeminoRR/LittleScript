﻿Imports System.IO
Module Helper

    '===============================
    '========== VARIABLES ==========
    '===============================
    Public ReadOnly AppData As String = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData).Replace("\", "/") & "/LittleScript"

    '==================================
    '========== GET TEMPLATE ==========
    '==================================
    Public Function getTemplate(ByVal templateFolder As String, ByVal filename As String) As String
        If Not File.Exists(templateFolder & "/" & filename) Then
            addBasicError("Cannot read file", """" & templateFolder & "/" & filename & """ is missing")
        End If
        Dim result As String = ""
        Try
            result = File.ReadAllText(templateFolder & "/" & filename)
        Catch ex As Exception
            addBasicError("Cannot read file", ex.Message)
        End Try
        Return result
    End Function

    '=============================
    '========== ADD LOG ==========
    '=============================
    Public Sub addLog(ByVal message As String)

        Console.ResetColor()
        Console.WriteLine("[LOGS] " & message)

    End Sub

    '==========================================
    '========== GET NODE PARENT FILE ==========
    '==========================================
    Public Function getNodeParentFile(ByVal node As Node) As LSFile

        'Get most upper parent
        Dim parentNode As Node = node
        While Not parentNode.parentNode Is Nothing
            parentNode = parentNode.parentNode
        End While

        'Check if is file
        If TypeOf parentNode Is LSFile Then
            Return DirectCast(parentNode, LSFile)
        End If

        'Error
        addBasicError("unreachable element", "Unable to reach file link to the <" & node.GetType().FullName.ToString() & "> node. Please report the problem to the developers. For now, try modifying your code.")
        Return Nothing

    End Function

    '==============================================
    '========== GET NODE PARENT FUNCTION ==========
    '==============================================
    Public Function getNodeParentFunction(ByVal node As Node, Optional ByVal pushError As Boolean = True) As FunctionNode

        'Get most upper parent
        Dim parentNode As Node = node
        While Not parentNode.parentNode Is Nothing
            If TypeOf parentNode Is FunctionNode Then
                Exit While
            End If
            parentNode = parentNode.parentNode
        End While

        'Check if is file
        If TypeOf parentNode Is FunctionNode Then
            Return DirectCast(parentNode, FunctionNode)
        End If

        'Error
        If pushError Then
            addNodeNamingError("unreachable element", "No function is linked to the following element.", node)
        End If
        Return Nothing

    End Function

    '===============================================
    '========== GET NODE PARENT CONTAINER ==========
    '===============================================
    Public Function getNodeParentContainer(ByVal node As Node) As containerNode

        'Get most upper parent
        Dim parentNode As Node = node
        While Not parentNode.parentNode Is Nothing
            If TypeOf parentNode Is containerNode Then
                Exit While
            End If
            parentNode = parentNode.parentNode
        End While

        'Check if is file
        If TypeOf parentNode Is containerNode Then
            Return DirectCast(parentNode, containerNode)
        End If

        'Error
        addNodeNamingError("unreachable element", "No function is linked to the following element", node)
        Return Nothing

    End Function

End Module