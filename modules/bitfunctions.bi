'=======================================================================
'       BIGINT BIT FUNCTIONS
'=======================================================================
' find the bit position of the first bit that differs from the sign bit
Function Bigint.msbit(ByRef a As Bigint) As Long
	Dim As Long i, j, k = 0
	i = Len(a.s)
	If 128 And a.s[Len(a.s)-1] Then ' negative
		Do  ' find the highest non-255 byte in the string
			i = i - 1
			j = a.s[i]
		Loop Until (j < 255) Or (i = 0)
		j = 255 - j
	Else                ' positive
		Do  ' find the highest non-zero byte in the string
			i = i - 1
			j = a.s[i]
		Loop Until (j > 0) Or (i = 0)
	End If
	' find the highest non-sign bit in the byte
	If j And   1 Then k = 1 ' straight code is faster than a loop
	If j And   2 Then k = 2
	If j And   4 Then k = 3
	If j And   8 Then k = 4
	If j And  16 Then k = 5
	If j And  32 Then k = 6
	If j And  64 Then k = 7
	If j And 128 Then k = 8
	k = k + (i * 8) - 1 ' if no bits differ (-1 or 0) then return -1
	Return k
End Function

'=======================================================================
' get the value of a specified bit in a big integer
Function Bigint.Bit_Value(ByRef v As Bigint, ByVal b As ULongInt) As Long
	Dim As Long bitval, by = b \ 8
	If by < Len(v.s) Then
		bitval = Bit(v.s[by], b Mod 8)
	Else
		If v.s[Len(v.s)-1] And 128 Then bitval = -1 ' the extended sign bit
	End If
	Return bitval
End Function

'================================================================
' set a specified bit in a big integer
Function Bigint.Bit_Set(ByRef vv As Bigint, ByVal b As ULongInt) As Bigint
	Dim As Bigint v = vv
	Dim As Long by, bi, delta, sign
	by = b \ 8      ' byte number
	bi = b Mod 8    ' bit number
	delta = by - Len(v.s) + 1
	If bi = 7 Then delta = delta + 1    ' protect the sign bit
	If delta > 0 Then    ' lengthen the number
		delta = ((delta + 3)\ 4) * 4
		If v.s[Len(v.s)-1] And 128 Then sign = -1 ' the extended sign bit
		v.s = v.s + String(delta, sign)
	End If
	v.s[by] = BitSet(v.s[by], bi)
	Return v
End Function

'================================================================
' clear a specified bit in a big integer
Function Bigint.Bit_Reset(ByRef vv As Bigint, ByVal b As ULongInt) As Bigint
	Dim As Bigint v = vv
	Dim As Long by, bi, delta, sign
	by = b \ 8      ' byte number
	bi = b Mod 8    ' bit number
	delta = by - Len(v.s) + 1
	If bi = 7 Then delta = delta + 1    ' protect the sign bit
	If delta > 0 Then    ' lengthen the number
		delta = ((delta + 3)\ 4) * 4
		If v.s[Len(v.s)-1] And 128 Then sign = -1 ' the extended sign bit
		v.s =  v.s + String(delta, sign)
	End If
	v.s[by] = BitReset(v.s[by], bi)
	Return v
End Function
