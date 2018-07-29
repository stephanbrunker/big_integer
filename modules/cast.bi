'================================================================
'       BIGINT CAST FUNCTIONS
'================================================================
Operator Bigint.cast() As Byte          ' CByte
	If This > 127 Or This < -128 Then
		Print " Overflow in BigInt to Byte conversion. "
		Print This
		Sleep : End
	End If
	Return *CPtr(Byte Ptr, StrPtr(this.s))
End Operator
'----------------------------------------------------------------
Operator Bigint.cast() As UByte         ' CUByte
	If This > 255 Or This < 0 Then
		Print " Overflow in BigInt to Ubyte conversion. "
		Print This
		Sleep : End
	End If
	Return *CPtr(UByte Ptr, StrPtr(this.s))
End Operator
'----------------------------------------------------------------
Operator Bigint.cast() As Short         ' CShort
	If This > 32767 Or This < -32768 Then
		Print " Overflow in BigInt to Short conversion. "
		Print This
		Sleep : End
	End If
	Return *CPtr(Short Ptr, StrPtr(this.s))
End Operator
'----------------------------------------------------------------
Operator Bigint.cast() As UShort        ' CUShort
	If This > 65535 Or This < 0 Then
		Print " Overflow in BigInt to UShort conversion. "
		Print This
		Sleep : End
	End If
	Return *CPtr(UShort Ptr, StrPtr(this.s))
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As Long          ' CLng
	If Len(this.s) <> 4 Then
		Print " Overflow in BigInt to Long conversion. "
		Print This
		Sleep : End
	End If
	Return *CPtr(Long Ptr, StrPtr(this.s))
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As ULong          ' CULng
	If This > 4294967295 Or This < 0 Then
		Print " Overflow in BigInt to ULong conversion. "
		Print This
		Sleep : End
	End If
	Return *CPtr(ULong Ptr, StrPtr(this.s))
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As LongInt        ' CLongInt
	Dim As String s = this.s
	If Len(s) > 8 Then
		Print " Overflow in BigInt to LongInteger conversion. "
		Print This
		Sleep : End
	End If
	If Len(s) = 4 Then  ' sign extend 4 byte integer to 8 byte LongInt
		If Bit(s[3], 7) Then
			s &= Bigint_s_1
		Else
			s &= Bigint_s0
		End If
	End If
	Return *CPtr(LongInt Ptr, StrPtr(s))
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As ULongInt       ' CULongInt
	Dim As String s = this.s
	If This > 18446744073709551615 Or This < 0 Then
		Print " Overflow in BigInt to LongInteger conversion. "
		Print This
		Sleep : End
	End If
	If Len(s) = 4 Then s &= Bigint_s0 'extend to len 8
	Return *CPtr(ULongInt Ptr, StrPtr(s))
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As Integer        ' CInt
	Dim a As Integer = 2147483647
	If (a + 1) > a Then     '64bit
		Return CLngInt(This)
	Else
		Return CLng(This)
	End If
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As UInteger       ' CUint
	Dim a As UInteger = 4294967295
	If (a + 1) > a Then     '64bit
		Return CULngInt(This)
	Else
		Return CULng(This)
	End If
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As Single         ' CSng
	Dim As Bigint b = This
	Dim As ULong ul, Sign_Bit
	If Bit( b.s[Len(b.s) - 1], 7) Then  ' extract the sign bit
		Sign_Bit = CULng(1) Shl 31   ' remember the sign
		b = -b  ' rectify
	End If  ' b is now a positive BigInt
	' overflows single?
	If Len(b.s) > 32 Then ' 32 bytes * 8 bits per byte = 256 bits
		' special case of sign bit = lead block &FFFF
		If Len(b.s) = 36 And ( b.s[35] And b.s[34] And b.s[33] And b.s[32] ) = &hFF Then
			ul = Sign_Bit Or &hFF000000 ' all ones exponent
			Return *Cast(Single Ptr, @ul)  ' bit pattern is a double
		End If
		Print " Overflow in BigInt to Single conversion. "
		Print This
		Sleep : End
	End If
	If b = 0 Then Return 0   ' test for simple zero
	Dim As LongInt expo = 8 * Len(b.s) + 126 ' = bits + expo_bias - 1
	' if needed for the conversion, extend tail with two LS blocks of zero
	If Len(b.s) = 4 Then b.s = Bigint_s0 + b.s
	' the ms block still contains the data, so no change to expo
	Dim As UByte Ptr ubp = StrPtr(b.s) + Len(b.s) - 1 ' point to the MSbyte
	Dim As Long i
	For i = 0 To 4  ' find the leading non-zero byte, MS block may be zero
		If *ubp > 0 Then Exit For
		ubp = ubp - 1
		expo = expo - 8 ' expo reduction of 8 bits per zero byte skipped
	Next i  ' ubp now points to the MS non-zero byte
	ul = *Cast(ULong Ptr, ubp - 3)  ' normalize bytes, left justify
	For i = 31 To 24 Step -1    ' find the MS set bit
		If Bit(ul, i) Then Exit For
		expo = expo - 1
	Next i  ' i now points to MSbit
	ul = ul Shr (i - 23)  ' shift right to put MSbit in bit 23
	ul = BitReset(ul, 23) ' kill only the implicit bit now in bit 52
	ul = Sign_Bit Or (expo Shl 23) Or ul  ' build the single
	Return *Cast(Single Ptr, @ul)  ' return the bit pattern as a double
End Operator
'----------------------------------------------------------------
Operator Bigint.Cast() As Double    ' CDbl
	Dim As Bigint b = This
	Dim As ULongInt uli, Sign_Bit
	If Bit( b.s[Len(b.s) - 1], 7) Then  ' extract the sign bit
		Sign_Bit = CULngInt(1) Shl 63   ' remember the sign
		b = -b  ' rectify
	End If  ' b is now a positive BigInt
	' overflows double? if mag > 1.797693134862310e308 = signed infinity
	If Len(b.s) > 128 Then ' 128 bytes * 8 bits per byte = 1024 bits
		' special case of sign bit = entire block
		If Len(b.s) = 132 And ( b.s[131] And b.s[130] And b.s[129] And b.s[128] ) = &hFF Then
			uli = Sign_Bit Or &h7FF0000000000000 ' all ones exponent
			Return *Cast(Double Ptr, @uli)  ' bit pattern is a double
		End If
		Print " Overflow in BigInt to Double conversion. "
		Print This
		Sleep : End
	End If
	If Len(b.s) = 4 Then    ' test for simple zero
		If ( b.s[3] Or b.s[2] Or b.s[1] Or b.s[0] ) = 0 Then Return 0
	End If
	Dim As LongInt expo = 8 * Len(b.s) + 1022 ' = bits + expo_bias - 1
	' if needed for the conversion, extend tail with two LS blocks of zero
	If Len(b.s) < 12 Then b.s = Chr(0,0,0,0, 0,0,0,0) + b.s
	' the ms block still contains the data, so no change to expo
	Dim As UByte Ptr ubp = StrPtr(b.s) + Len(b.s) - 1 ' point to the MSbyte
	Dim As Long i
	For i = 0 To 4  ' find the leading non-zero byte, MS block may be zero
		If *ubp > 0 Then Exit For
		ubp = ubp - 1
		expo = expo - 8 ' expo reduction of 8 bits per zero byte skipped
	Next i  ' ubp now points to the MS non-zero byte
	uli = *Cast(ULongInt Ptr, ubp - 7)  ' normalize bytes, left justify
	For i = 63 To 56 Step -1    ' find the MS set bit
		If Bit(uli, i) Then Exit For
		expo = expo - 1
	Next i  ' i now points to MSbit
	uli = uli Shr (i - 52)  ' shift right to put MSbit in bit 52
	uli = BitReset(uli, 52) ' kill only the implicit bit now in bit 52
	uli = Sign_Bit Or (expo Shl 52) Or uli  ' build the double
	Return *Cast(Double Ptr, @uli)  ' return the bit pattern as a double
End Operator

'----------------------------------------------------------------
' unpack a straight binary string to a decimal ascii string
Operator Bigint.cast() As String
	Dim As bigint b
	Dim As String d
	b = This
	d = Chr(0) ' initial decimal output string
	Dim As Integer i, j, sign
	Dim As UInteger carry
	' if negative then negate, append the sign later
	If b.s[Len(b.s) - 1] And 128 Then ' negative
		sign = - 1
		b = -b
	End If
	' change from base 2 to base 100
	For j = Len(b.s) - 1 To 0 Step - 1 ' each byte in string is base 256
		carry = b.s[j] ' the next byte to add after multiply
		Dim as UByte Ptr k = StrPtr(d) + Len(d) - 1 ' added pointer
		For i = Len(d) - 1 To 0 Step - 1
			carry += *k * 256
			*k = carry Mod 100
			carry \= 100
			k -= 1
		Next i
		Do While carry > 0 ' output string overflows
			d = Chr(carry Mod 100) & d ' extend output string as needed
			carry = carry \ 100
		Loop
	Next j
	' need to split the value of the byte into 2 digits and to convert into ASCII
	Dim As String d2 = Chr(d[0] \ 10 + Asc("0")) + Chr(d[0] Mod 10 + Asc("0"))
	' remove a possible 0 in front of the number
	d2 = LTrim(d2, "0")
	If Len(d2) = 0 Then Return "+0"
	For i = 1 To Len(d) - 1
		d2 &= Chr(d[i] \ 10 + Asc("0")) & Chr(d[i] Mod 10 + Asc("0"))
	Next i
	If sign Then d2 = "-" & d2 Else d2 = "+" & d2
	Return d2
End Operator

'----------------------------------------------------------------
' Casting to Bigint
Function CBig Overload(a as Byte) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as UByte) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as Short) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as UShort) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as Integer) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as UInteger) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as Long) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as ULong) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as LongInt) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as ULongInt) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as Single) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as Double) as Bigint
	Dim As Bigint b=a
	Return b
End Function

Function CBig Overload(a as String) as Bigint
	Dim As Bigint b=a
	Return b
End Function
