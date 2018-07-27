'================================================================
'       BIGINT COMPARATION FUNCTIONS
'================================================================
Operator = (ByRef a As Bigint, ByRef b As Bigint) As Integer
	Dim As Integer c = bigint.compare(a, b)
	If c=0 Then Return -1 Else Return 0
End Operator

Operator <> (ByRef a As Bigint, ByRef b As Bigint) As Integer
	Dim As Integer c = bigint.compare(a, b)
	If c=0 Then Return 0 Else Return -1
End Operator

Operator < (ByRef a As Bigint, ByRef b As Bigint) As Integer
	Dim As Integer c = bigint.compare(a,b)
	If c = 1 Then Return -1 Else Return 0
End Operator

Operator > (ByRef a As Bigint, ByRef b As Bigint) As Integer
	Dim As Integer c = bigint.compare(a,b)
	If c = -1 Then Return -1 Else Return 0
End Operator

Operator <= (ByRef a As Bigint, ByRef b As Bigint) As Integer
	Dim As Integer c = bigint.compare(a,b)
	If c = 1 Or c = 0 Then Return -1 Else Return 0
End Operator

Operator >= (ByRef a As Bigint, ByRef b As Bigint) As Integer
	Dim As Integer c = bigint.compare(a, b)
	If c = -1 Or c = 0 Then Return -1 Else Return 0
End Operator

'=====================================================================
'Main Comparation function - faster than substract ...
'=====================================================================
Function Bigint.compare(ByRef a As Bigint, ByRef b As Bigint) As Long
	' return -1 for a>b; 1 for a<b and 0 for equal
	Dim As Byte signa, signb
	signa = 128 And a.s[Len(a.s)-1]   ' -1 (true) negative, 0 (false) positive
	signb = 128 And b.s[Len(b.s)-1]
	'-------------------------------------------
	' sign is different - easy:
	If signa = 0 And signb = -128 Then
		Return -1
	ElseIf signa = -128 And signb = 0 Then
		Return 1
	End If
	'-------------------------------------------
	' len is different - easy:
	If Len(a.s) > Len(b.s) Then
		If signa = 0 Then
			Return -1
		Else
			Return 1
		End If
	ElseIf Len(a.s) < Len(b.s) Then
		If signa = 0 Then
			Return 1
		Else
			Return -1
		End If
	End If
	'-------------------------------------------
	' compare block for block:
	Dim As Long i
	Dim as Ulong Ptr pa = CPtr(Ulong Ptr, Strptr(a.s))
	Dim as Ulong Ptr pb = CPtr(Ulong Ptr, Strptr(b.s))

	For i = (Len(a.s) \ 4 ) - 1 To 0 Step -1
		If pa[i] > pb[i] Then
			Return -1
		ElseIf pa[i] < pb[i] Then
			Return 1
		End If
	Next i

	Return 0
End Function
