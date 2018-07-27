'================================================================
'       BIGINT CONVERSION FUNCTIONS
'================================================================
' convert a Bigint to binary (0110000111 etc.)
Function Bin(ByRef s As Bigint) As String
	Dim As Long i
	Dim As String h     ' lsb is string[0] = little endian
	For i = Len(s.s)-1 To 0 Step -1
		h = h & Bin(s.s[i], 8)
	Next i
	Return h
End Function

Function Bin (ByRef a As Bigint,ByRef n As ULong) As String
	Dim result As String = Right(Bin(a),n)
	Return result
End Function

'----------------------------------------------------------------
' convert a Bigint to hexadecimal
Function Hex (ByRef s As Bigint) As String
	Dim As Long i
	Dim As String h     ' lsb is string[0] = little endian
	For i = Len(s.s)-1 To 0 Step -1
		h = h & Hex(s.s[i], 2)
	Next i
	h = ltrim(h,any "0")
	If Len(h) <> 0 Then
		Return h
	Else
		Return "0"
	End If
End Function

Function Hex (ByRef a As Bigint,ByRef n As ULong) As String
	Dim result As String = Right(Hex(a),n)
	If a < 0 And Len(result) < n Then
		result = String(n-Len(result),"F") & result
	ElseIf a >= 0 And Len(result) < n Then
		result = String(n-Len(result),"0") & result
	End If
	Return result
End Function

'----------------------------------------------------------------
' convert a Bigint to unsigned hexadecimal (trimmed)
Function UHex(ByRef s As Bigint) As String
	Dim As Long i
	Dim As Bigint a = s
	If 128 And a.s[Len(a.s)-1] Then     ' Bigint is negative
		Print "cannot convert negative to uniform"
		Sleep : End
	End If
	Dim As String h     ' hex is big-endian
	For i = Len(s.s)-1 To 0 Step -1
		h = h & Hex(s.s[i], 2)
	Next i
	If Len(h) <> 8 Then h=LTrim(h,"00000000")
	Return h
End Function

Function UHexT(ByRef s As Bigint,ByRef n As ULong) As String
	Dim As String h = Uhex(s)
	If Len(h) < n Then
		h = String(n - Len(h),"0") & h
	End If
	Return h
End Function

'----------------------------------------------------------------
' convert a Bigint to octal
Function Oct(ByRef a As Bigint) As String
	Dim As String b, c
	Dim As Bigint s = a
	If 128 And a.s[Len(a.s)-1] Then ' extend the sign
		s.s = s.s & Chr(255,255,255,255)
	Else
		s.s = s.s & Bigint_s0
	End If
	Dim As Long i
	Dim As ULongInt u
	For i = 1 To Len(a.s) Step 3
		b = Mid(s.s, i, 3)    ' take three bytes = 24 bits
		u = b[0] + 256 * (b[1] + 256 * b[2])
		c = Oct(u, 8) + c ' eight symbols per three bytes
	Next i
	Return c
End Function

Function Oct (ByRef a As Bigint,ByRef n As ULong) As String
	Dim result As String = Right(Hex(a),n)
	Return result
End Function

'----------------------------------------------------------------
' Bigint to twos compliment binary
Function MkBigint(ByRef a As Bigint) As String
	Dim As String s = a.s
	Return s
End Function

'----------------------------------------------------------------
' Bigint to unsigned binary
Function MkUBigint(ByRef a As Bigint) As String
	Dim As String s
	If 128 And a.s[Len(a.s)-1] Then     ' Bigint is negative
		Print "cannot convert negative to unsigned"
		Sleep : End
	Else
		s = a.s
	End If
	s = RTrim(s,Bigint_s0)
	Return s
End Function

'----------------------------------------------------------------
' Val (as Bigint)
Function ValBigint(ByRef aa As String) As Bigint
	Dim c As Bigint = aa        ' VAL is integrated in the Constructor(As String)
	Return c
End Function

'----------------------------------------------------------------
' Val (as UnSignedBigint)
Function ValUBigint(ByRef aa As String) As Bigint
	Dim c As Bigint = aa & "u"  ' use the constructor with unsigned suffix
	Return c
End Function

'----------------------------------------------------------------
' twos compliment binary to Bigint
Function CVBigint (ByRef a As String) As Bigint
	Dim As Bigint b
	If Len(a) = 0 Then
		b = 0
		Return b
	End If
	If (128 And a[Len(a)-1]) Then   ' negative, pad to blocklen with FF
		b.s = a & String((4-Len(a) Mod 4),Chr(255))
	Else
		b.s = a & String((4-Len(a) Mod 4),Chr(0)) ' positive, pad to blocklen with 00
	End If
	Return b
End Function

'----------------------------------------------------------------
' unsigned binary to Bigint
Function CVUBigint(ByRef a As String) As Bigint
	Dim As Bigint b
	Dim As Long pad
	pad = 4 - (Len(a) Mod 4)
	If (pad = 4) And (Len(a) <> 0) Then pad = 0
	b.s = a & String(pad,0)    ' Pad to blocklen
	If (128 And b.s[Len(b.s)-1]) Then b.s &= Bigint_s0 ' make it positive
	Return b
End Function
