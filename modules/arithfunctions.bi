'================================================================
'      BIGINT ARITHMETRIC FUNCTIONS
'================================================================
' remove unnecessary leading blocks, prune time easily earns it's keep
Sub Bigint.prune(ByRef a As Bigint)
	a.s = Left(a.s, (((bigint.msbit(a) + 1) \ 32 ) + 1 ) * 4)
	' Times and space are improved through the sensible use of prune.
	' Addition of opposite signs or subtraction can cancel many blocks.
	' A redundant block can appear during multiplication. Square or div2.
	' Mul2, Complement, Negate and Absolute do not generate unnecessary blocks.
	' Power is pruned internally by the prune in multiply and square.
End Sub

'=======================================================================
' square a number, approaches twice the speed of multiply for big numbers
Function Bigint.square(ByRef aa As Bigint) As Bigint
	If aa.s = Bigint_s0 Then
		Return aa
	ElseIf aa.s = Bigint_s1 Then
		Return aa
	End If
	Dim As Bigint a = aa, c
	If (128 And a.s[Len(a.s)-1]) Then a = -a
	'------------------------------------------------------------
	' find the dimension of the problem
	Dim As Long i, j, asize = Len(a.s)  ' number of bytes in a
	c.s = String(2 * asize, Chr(0) )  ' initialise accumulator
	asize = (asize \ 4) - 1   ' highest block number in a
	'------------------------------------------------------------
	' pointers into all the Bigints
	Dim As ULong Ptr pa, pc
	pa = Cast(ULong Ptr, StrPtr(a.s) )  ' the base addresses of Bigints
	pc = Cast(ULong Ptr, StrPtr(c.s) )
	Dim As ULongInt product  ' bottom half is data, top half will be carry
	Dim As ULong carry, sum
	'------------------------------------------------------------
	' multiply one triangular corner only
	For i = 1 To asize
		' pa starts at 1 not zero because 0,0 is on the diagonal
		' the second element in a starts at zero
		' pc is the accumulator ic = icz + i + j
		carry = 0     ' clear carry
		For j = 0 To i - 1
			product = CULngInt(pa[i]) * pa[j] + pc[j+i] + carry
			pc[j+i] = product
			carry = product Shr 32
		Next j
		pc[j+i] = carry     ' line of blocks gets one longer each loop
	Next i
	'------------------------------------------------------------
	' double the triangle, cannot do it at same time as squaring diagonal
	' because it can cause the product to overflow
	carry = 0 ' clear carry
	For i = 0 To (2 * asize) + 1
		sum = pc[i]
		product = CULngInt(sum) + sum + carry
		pc[i] = product
		carry = product Shr 32
	Next i
	'------------------------------------------------------------
	' square and accumulate the diagonal elements
	carry = 0     ' clear carry
	For i = 0 To asize
		' square the diagonal entry, while propagating carry
		sum = pa[i]
		product = CULngInt(sum) * sum + pc[i+i] + carry
		pc[i+i] = product
		carry = product Shr 32
		' propagate carry through the following odd block of C
		product = CULngInt(pc[i+i+1]) + carry
		pc[i+i+1] = product
		carry = product Shr 32
	Next i
	'------------------------------------------------------------
	If 128 And c.s[Len(c.s)-1] Then c.s = c.s & Bigint_s0 ' sign is positive
	Bigint.prune(c)
	Return c
End Function

'=======================================================================
' shift up one bit, low towards high
Function Bigint.mul2(ByRef a As Bigint) As Bigint
	If a.s = Bigint_s0 Then Return a
	Dim As Long i, sign, sum, carry
	Dim As Bigint b = a
	sign = b.s[Len(b.s) - 1] And 128    ' snag the msb of highest byte
	For i = 0 To Len(b.s) - 1
		sum = b.s[i] + b.s[i] + carry
		carry = HiByte(sum)
		b.s[i] = LoByte(sum)
	Next i
	If ( b.s[Len(b.s) - 1] And 128 ) <> sign Then
		carry = carry * 255
		b.s = b.s + Chr(carry,carry,carry,carry)    ' sign extend four bytes
	End If
	Return b
End Function

'=======================================================================
' shift down one bit, high towards low
Function Bigint.div2(ByRef a As Bigint) As Bigint
	If a.s = Bigint_s0 Then Return a
	Dim As Long i, carry
	Dim As Bigint b = a
	For i = 0 To Len(a.s)-2   ' all except the top byte of four
		b.s[i] = ( b.s[i] \ 2 ) + (128 * (b.s[i+1] And 1))
	Next i
	i = Len(b.s) - 1
	b.s[i] = b.s[i] \ 2
	b.s[i] = b.s[i] Or (2 * ( b.s[i] And 64)) ' sign extend the msb
	Bigint.prune(b)
	Return b
End Function

'=======================================================================
' integer divide, a \ b, return div and mod
Sub Bigint.div(_
	ByRef a As Bigint, ByRef bb As Bigint,_
	ByRef q As Bigint, ByRef r As Bigint)
	If bb.s = Bigint_s0 Then
		Print " Division by zero. "
		Sleep : End
	End If
	Dim As Long lena = Len(a.s), lenbb = Len(bb.s)
	'------------------------------------------------------------
	' Test if Longint Division possible
	If (lena <= 8) And (lenbb <= 8) Then ' arguments are one or two blocks
		Dim As Longint va = a, vb = bb
		q = va \ vb
		r = va Mod vb
		Exit Sub
	End If
	'------------------------------------------------------------
	' Test if divisor is bigger than dividend
	If Abs(bb) > Abs(a) Then
		r.s = a.s
		q.s = Bigint_s0
		Exit Sub
	End If
	'------------------------------------------------------------
	' Read Signs
	Dim As Long sa, sb, sq
	sa = 128 And a.s[lena-1]
	sb = 128 And bb.s[lenbb-1]
	sq = sa Xor sb  ' sign of the result
	'---------------------------------------------------------------------
	' Setup variables and pointers
	' r=dividend and remainder
	' b=divisor
	' q=quotient
	' sum=interim result for subtraction

	Dim As Bigint b = bb
	If sb Then b = -b
	r.s = a.s
	If sa Then r = -r
	Dim As UShort Ptr pr, pb, pq
	Dim As ULong sum, offset=&hFFFF0000
	Dim As ULong rblocks,bblocks,blockshift,rounds,blocks
	Dim As ULong lenr = Len(r.s), lenb = Len(b.s)
	Dim As UShort qi, carry
	Dim As Long substract, i
	Dim As ULongInt high48b
	Dim As ULongInt high48r = 0
	pr = Cast(UShort Ptr, StrPtr(r.s))
	pb = Cast(UShort Ptr, StrPtr(b.s))
	rblocks = (lenr \ 2) - 1
	bblocks = (lenb \ 2) - 1
	'------------------------------------------------------------
	' convert to 16bit blocklength for effective testdivison
	' append exactly two zero blocks and ignore the second leading zeroblock
	If pr[rblocks] <> 0 Then
		r.s &= Bigint_s0
		rblocks += 1
	ElseIf pr[rblocks] = 0 And pr[rblocks-1] = 0 Then
		rblocks -= 1
	End If
	If pb[bblocks] <> 0 Then
		b.s &= Bigint_s0
		bblocks += 1
	ElseIf pb[bblocks] = 0 And pb[bblocks-1] = 0 Then
		bblocks -= 1
	End If
	' new pointer after changing string
	pr = Cast(UShort Ptr, StrPtr(r.s))
	pb = Cast(UShort Ptr, StrPtr(b.s))
	blockshift = rblocks - bblocks
	rounds = blockshift + 1
	'------------------------------------------------------------
	' setup quotient to usual 32bit blocklength
	If Bit(rounds,0) = 0 Then
		q.s = String(rounds * 2, 0) ' quotient
	Else
		q.s = String((rounds + 1) * 2, 0)
	End If
	pq = Cast(UShort Ptr, StrPtr(q.s))
	pq += blockshift ' start at msb
	'------------------------------------------------------------
	' start calculation
	' most significant bits of divisor are constant
	If bblocks = 1 Then
		' shift left if divisor is less than 16 bits
		high48b = CULng(pb[bblocks-1] Shl 16)
	Else
		' most significant 32bits of divisor, rounded up
		high48b = CULngInt(pb[bblocks-1]) Shl 16 + pb[bblocks-2] + 1
	End If

	For blocks = 1 To rounds
		' msbits:  remainder of previous step and following 32bits (= ms48bits)
		high48r = CULngInt(pr[rblocks]) Shl 32 + CULng(pr[rblocks-1] Shl 16) + pr[rblocks-2]

		' testdivision: because of rounding up result may be 1 too low
		qi = (high48r \ high48b )

		If qi > 0 Then
			' r= r - q*b for every block, begin at lsblock, with carry
			carry = 0
			For i = blockshift To rblocks
				If carry = 0 Then     ' don't apply the offset!
					sum = pr[i] - (pb[i-blockshift] * qi)
				Else          ' offset because the overflow 16->32 bit
					sum = pr[i] - (pb[i-blockshift] * qi) + carry + offset
				End If
				pr[i] = sum
				carry = sum Shr 16
			Next i
		End If
		'------------------------------------------------------------
		' if testdivision was too low, additional substraction needed
		substract = 0
		' test if remainder > shifted quotient
		For i = rblocks To blockshift Step -1
			substract = pr[i] - pb[i-blockshift]
			If substract <> 0 Then Exit For ' not equal
		Next i  ' equal, check next block

		' if higher, then substract quotient once and thus fix rounding error
		If substract >= 0 Then
			carry = 1
			qi += 1
			For i = blockshift To rblocks
				sum = pr[i] + CUShort(Not(pb[i-blockshift])) + carry
				pr[i] = sum
				carry = sum Shr 16
			Next i
		End If
		'------------------------------------------------------------
		rblocks -= 1
		blockshift -= 1
		*pq = qi      ' write quotient with pointer
		pq -= 1     ' next block of quotient
	Next blocks
	'------------------------------------------------------------
	' finalisation
	If Bit(q.s[Len(q.s) - 1], 7) Then q.s &= Bigint_s0
	Bigint.prune(r)    ' trim result
	Bigint.prune(q)
	If sq Then q = -q  ' Xor of input signs
	If sa Then r = -r    ' sign of original input A
	'------------------------------------------------------------
End Sub

'=======================================================================
Function Bigint.factorial(ByRef a As Bigint) As Bigint
	Dim As Bigint f = 1, n = a
	Do Until n < 2
		f = f * n
		n = n - 1
	Loop
	Return f
End Function

'=======================================================================
'exponentiation modulus
Function Bigint.modpower(ByRef bb As Bigint, ByRef ee As Bigint, ByRef m As Bigint) As Bigint
	If m.s = Bigint_s0 Then
		Print " Division by zero. "
		Sleep : End
	End If
	If (ee.s[Len(ee.s)-1] And 128) Then
		Print "Cannot raise a Bigint to a negative power"
		Sleep : End
	ElseIf ee.s = Bigint_s0 Then
		If Abs(m) = 1 Then Return 0 Else Return 1
	End If
	Dim As Bigint r = 1, b, y = bb, x ' x is dump for the quotient
	Dim As Long bitlen,i,z
	Dim As ULong Ptr pee
	pee = Cast(ULong Ptr, StrPtr(ee.s))
	Dim As ULong spee
	spee = *pee         ' load first block of exponent in variable

	bitlen = bigint.msbit(ee)    ' the highest set bit
	bigint.div(y,m,x,b)     ' initial reduction

	' i counts from the lsb to msb, z counts the 32 bits in a block
	For i=0 To bitlen-1
		If Bit(spee,z) Then     ' if bit is set then multiply
			y = r * b
			bigint.div(y,m,x,r)
		End If
		y = bigint.square(b)
		bigint.div(y,m,x,b)
		If z = 31 Then            ' reset z, next block
			z = 0
			pee += 1
			spee = *pee
		Else
			z += 1
		End If
	Next i
	y = r * b  ' bitvalue for highest bit=1
	bigint.div(y,m,x,r)
	Return r
End Function
