'================================================================
'        BIGINT CONSTRUCTORS
'================================================================

Constructor Bigint ()    ' default constructor
	this.s = Bigint_s0   ' zero
End Constructor

Constructor Bigint (ByRef a As Bigint) ' copy constructor
	this.s = a.s
End Constructor

Constructor Bigint (ByRef a As Byte)
	If (128 And a) Then
		this.s = Chr(a,-1,-1,-1)
	Else
		this.s = Chr(a,0,0,0)
	End If
End Constructor

Constructor Bigint (ByRef a As UByte)
	this.s = Chr(a,0,0,0)
End Constructor

Constructor Bigint (ByRef a As Short)
	If (32768 And a) Then
		this.s = Chr(LoByte(a), HiByte(a), -1, -1 )
	Else
		this.s = Chr(LoByte(a), HiByte(a), 0, 0 )
	End If
End Constructor

Constructor Bigint (ByRef a As UShort)
	this.s = Chr(LoByte(a), HiByte(a), 0, 0 )
End Constructor

Constructor Bigint (ByRef a As Long)
	this.s = Bigint_s0
	Dim As Long Ptr bip = CPtr(Long Ptr, StrPtr(this.s))
	Dim As Long Ptr  ip = CPtr(Long Ptr, @a)
	*bip = *ip
End Constructor

Constructor Bigint (ByRef a As ULong)
	this.s = Bigint_s0
	Dim As ULong Ptr bip = CPtr(ULong Ptr, StrPtr(this.s))
	Dim As ULong Ptr uip = CPtr(ULong Ptr, @a)
	*bip = *uip
	If (128 And this.s[3]) Then this.s &= Bigint_s0 ' make it positive
End Constructor

Constructor Bigint (ByRef a As Integer)
	If a < -2147483648 Or a > 2147483647 Then   ' integer<64>
		this.s = Bigint_s00
		Dim As LongInt Ptr bip = CPtr(LongInt Ptr, StrPtr(this.s))
		Dim As LongInt Ptr lip = CPtr(LongInt Ptr, @a)
		*bip = *lip
	Else                    ' integer<32>
		this.s = Bigint_s0
		Dim As Long Ptr bip = CPtr(Long Ptr, StrPtr(this.s))
		Dim As Long Ptr  ip = CPtr(Long Ptr, @a)
		*bip = *ip
	End If
End Constructor

Constructor Bigint (ByRef a As UInteger)
	If a > 4294967295 Then  ' uinteger<64>
		this.s = Bigint_s00
		Dim As ULongInt Ptr  bip = CPtr(ULongInt Ptr, StrPtr(this.s))
		Dim As ULongInt Ptr ulip = CPtr(ULongInt Ptr, @a)
		*bip = *ulip
		If (128 And this.s[7]) Then this.s &= Bigint_s0 ' make it positive
	Else                   ' integer<32>
		this.s = Bigint_s0
		Dim As ULong Ptr bip = CPtr(ULong Ptr, StrPtr(this.s))
		Dim As ULong Ptr uip = CPtr(ULong Ptr, @a)
		*bip = *uip
		If (128 And this.s[3]) Then this.s &= Bigint_s0 ' make it positive
	End If
End Constructor

Constructor Bigint (ByRef a As LongInt)
	If a < -2147483648 Or a > 2147483647 Then   '8Byte-Bigint
		this.s = Bigint_s00
		Dim As LongInt Ptr bip = CPtr(LongInt Ptr, StrPtr(this.s))
		Dim As LongInt Ptr lip = CPtr(LongInt Ptr, @a)
		*bip = *lip
	Else                    '4Byte-Bigint
		this.s = Bigint_s0
		Dim As Long Ptr bip = CPtr(Long Ptr, StrPtr(this.s))
		Dim As Long Ptr  ip = CPtr(Long Ptr, @a)
		*bip = *ip
	End If
End Constructor

Constructor Bigint (ByRef a As ULongInt)
	If a > 4294967295 Then  ' 8Byte-Bigint
		this.s = Bigint_s00
		Dim As ULongInt Ptr  bip = CPtr(ULongInt Ptr, StrPtr(this.s))
		Dim As ULongInt Ptr ulip = CPtr(ULongInt Ptr, @a)
		*bip = *ulip
		If (128 And this.s[7]) Then this.s &= Bigint_s0 ' make it positive
	Else                   ' 4Byte-Bigint
		this.s = Bigint_s0
		Dim As ULong Ptr bip = CPtr(ULong Ptr, StrPtr(this.s))
		Dim As ULong Ptr uip = CPtr(ULong Ptr, @a)
		*bip = *uip
		If (128 And this.s[3]) Then this.s &= Bigint_s0 ' make it positive
	End If
End Constructor

Constructor Bigint (ByRef a As Single)
	Const As ULong implicit_bit = 2^23
	Const As ULong mant_mask = implicit_bit - 1
	Dim As ULong u, mant
	Dim As Long negative, expo
	Dim As Bigint x
	'----------------------------------------------------
	If a < 0 Then negative = -1 ' remember sign
	a = Int(Abs(a) + 0.5)       ' rectify and round to closest integer
	'----------------------------------------------------
	u = *(CPtr(ULong Ptr, @a))  ' copy Single into a Ulong frame
	expo = (u Shr 23) And 255   ' the 8 bit exponent
	mant = (u And mant_mask )   ' 23 mantissa bits
	'----------------------------------------------------
	If expo = 0 Then         ' the double has zero value or is de-normalized
		this.s = Bigint_s0   ' de-normalized is very very small
	Else
		mant = mant + implicit_bit ' insert the missing implicit bit
		expo = expo - 127          ' remove the exponent bias
		If expo < 24 Then
			mant = mant Shr (23 - expo) ' reduce it to integer
			x.s = Bigint_s0
			*(CPtr(ULong Ptr, StrPtr(x.s))) = mant   ' make Bigint
			If negative Then x = - x
		Else
			Print "WARNING: Single argument was unreliable."
			Sleep : End
		End If
		This = x
	End If
End Constructor

Constructor Bigint (ByRef a As Double)
	Const As ULongInt implicit_bit = 2^52          ' 4503599627370496
	Const As ULongInt mant_mask = implicit_bit - 1 ' 4503599627370495
	Dim As ULongInt u, mant
	Dim As Long negative, expo
	Dim As Bigint x
	'----------------------------------------------------
	If a < 0 Then negative = -1 ' remember sign
	a = Int(Abs(a) + 0.5) ' rectify and round to closest integer
	'----------------------------------------------------
	u = *(CPtr(ULongInt Ptr, @a))   ' copy Double into a Ulongint frame
	expo = (u Shr 52) And 2047  ' the 11 bit exponent
	mant = (u And mant_mask )   ' 52 mantissa bits
	'----------------------------------------------------
	If expo = 0 Then    ' the double has zero value or is de-normalized
		this.s = Bigint_s0   ' de-normalized is very very small
	Else
		mant = mant + implicit_bit ' insert the missing implicit bit
		expo = expo - 1023  ' remove the exponent bias
		If expo < 53 Then
			mant = mant Shr (52 - expo) ' reduce it to integer
			x.s = Bigint_s00
			*(CPtr(ULongInt Ptr, StrPtr(x.s))) = mant   ' make Bigint
			If negative Then x = - x
		Else
			Print "WARNING: Double argument was unreliable."
			Sleep : End
		End If
		This = x
	End If
End Constructor

' pack ascii numeric, octal, dual or hexadecimal string to a Bigint
' the output Bigint will have a length that is a multiple of 4 bytes
' works equivalent to VAL: ignore leading space, accept + and - at
' the beginning and stop if a non-number appears in the string
' 'u' enforces Unsigned conversion
Constructor Bigint (ByRef aa As String)
	Dim as String a = LTrim(aa)  ' remove space
	If Len(a) = 0 Then This.s = Bigint_s0 : Exit Constructor

	#Define decimal 1
	#Define hexadecimal 2
	#Define dual 3
	#Define octal 4
	Dim as Byte mode, sign, usgn
	'------------------------------------------------------------
	' check first character and trim to numbers only
	Select case a[0]
		Case Asc("&")
			If Len(a) > 1 Then
				Select case a[1]
					Case Asc("h"), Asc("H")
						mode = hexadecimal
						a = Right(a,Len(a)-2)
					Case Asc("b"), Asc("B")
						mode = dual
						a = Right(a,Len(a)-2)
					Case Asc("o"), Asc("O")
						mode = octal
						a = Right(a,Len(a)-2)
					Case Else
						If a[1] > 47 And a[1] < 58 Then ' octal is &3333 and &o3333
							mode = octal
							a = LTrim(a,"&")
						Else
							This.s = Bigint_s0 : Exit Constructor
						End If
				End Select
			Else
				This.s = Bigint_s0 : Exit Constructor
			End If
		Case Asc("-")
			a = LTrim(a,"-")
			sign = -1
			mode = decimal
		Case Asc("+")
			a = LTrim(a,"+")
			mode = decimal
		Case Else
			If a[0] > 47 And a[0] < 58 Then
				mode = decimal
			Else
				This.s = Bigint_s0 : Exit Constructor
			End If
	End Select
	'------------------------------------------------------------
	' check for suffix
	If a[Len(a)-1] = Asc("u") Then
		usgn = -1
		a = RTrim(a,"u")
	End If

	'------------------------------------------------------------
	' stop conversion if a nonconform type occurs
	Dim i as Long
	For i  = 0 to Len(a)-1
		Select Case mode
			Case hexadecimal
				If (a[i] < 48 And a[i] > 57 ) And (a[i] < 65 And a[i] > 70) And (a[i] < 97 And a[i] > 102) Then
					a = Left(a,i)
					Exit For
				End If
			Case octal
				If a[i] < 48 And a[i] > 56 Then
					 a = Left(a,i)
					Exit For
				End If
			Case dual
				If a[i] < 48 And a[i] > 49 Then
					a = Left(a,i)
					Exit For
				End If
			Case decimal
				If a[i] < 48 And a[i] > 57 Then
					a = Left(a,i)
					Exit For
				End If
		End Select
	Next i

	Select Case Mode
	Case hexadecimal
		' pad leading zeroes for positive hex
		If (Len(a) Mod 8) <> 0 Then a = String(8-(Len(a) Mod 8),"0") & a
		Dim as Ulong b
		Dim as Long d
		Dim as Bigint c
		' get positive or negative value for the first block
		If usgn = -1 Then   ' unsigned suffix
			b = ValUInt("&h" & Mid(a,1,8))
			c = b
		Else
			d = ValInt("&h" & Mid(a,1,8))     ' can be positive or negative
			c = d
		End If
		' add all following blocks (freakish, but correct also for negative values)
		For i = 9 To Len(a) Step 8
			c = c Shl 32
			b = ValUInt("&h" & Mid(a,i,8))
			c += b
		Next i
		This = c
	Case octal
		' pad leading zeroes
		If (Len(a) Mod 10) <> 0 Then a = String(10-(Len(a) Mod 10),"0") & a
		Dim as Ulong b
		Dim as Bigint c
		For i = 1 To Len(a) Step 10
			b = ValUInt("&o" & Mid(a,i,10))
			c = c Shl 30
			c += b
		Next i
		This = c
	Case dual
		' pad leading zeroes if blocksize <> 32bit - positive
		If (Len(a) Mod 32) <> 0 Then a = String(32-(Len(a) Mod 32),"0") & a
		Dim as Ulong b
		Dim as Long d
		Dim as Bigint c
		If usgn = -1 Then
			b = ValUInt("&b" & Mid(a,1,32))
			c = b
		Else
			d = ValInt("&b" & Mid(a,1,32))
			c = d
		End If
		For i = 33 To Len(a) Step 32
			c = c Shl 32
			b = ValUInt("&b" & Mid(a,i,32))
			c += b
		Next i
		This = c
	Case decimal
		Dim As Long p, j, blocks
		' extend to next multiple of 9 digits
		i = Len(a)
		blocks = i \ 9      ' number of 9 digit blocks needed
		If i Mod 9 <> 0 Then blocks += 1
		p = 9 * blocks
		a = String(p - i, "0") & a  ' pad to next multiple of 9 digits
		'------------------------------------------------------------
		' decimal to binary conversion
		i = ( 8 + Len(a) * 3.32192809488) \ 8   ' bytes needed for binary
		blocks = 1 + (i \ 4)                    ' adjust to multiple of 4
		this.s = String(blocks * 4, 0 ) ' binary destination string
		'------------------------------------------------------------
		Dim As ULong Ptr bp, bpz, bpcarry, bpdata
		bpz = Cast(ULong Ptr, StrPtr(this.s)) ' binary output string[0]
		Dim As ULongInt product, carry, multiplier = 1e9
		bpdata = Cast(ULong Ptr, @product) ' bottom half of product
		bpcarry = bpdata + 1                ' top half of product
		'------------------------------------------------------------
		blocks = 1  ' blocks will be advanced as required by carry
		For i = 1 To Len(a)-8 Step 9   ' msd to lsd in blocks of 9
			bp = bpz    ' point back to the start
			carry = ValULng(Mid(a, i, 9))  ' take the next 9 digit block
			For j = 1 To blocks
				product = multiplier * *bp + carry
				*bp = CULng(*bpdata)
				carry = CULngInt(*bpcarry)
				bp += 1
			Next j
			' advancing blocks only as needed doubles the speed of conversion
			If Carry Then
				*bp = carry
				blocks += 1 ' an exact count of the blocks used
			End If
		Next i
		this.s = Left(this.s, blocks * 4) ' keep only used blocks
		'-------------------------------------------------------------
		If this.s[Len(this.s)-1] And 128 Then this.s &= Bigint_s0 ' MSB must be 0
		If sign Then
			This = - This
			' That one doesn't make sense: negative Input to unsigned:
			If usgn Then this.s &= Bigint_s0
		End If
	End Select
End Constructor

Operator Bigint.let (ByRef a As Bigint)
	this.s=a.s
End Operator
