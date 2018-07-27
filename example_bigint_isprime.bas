'Miller-Rabin prime test for Big Integers
'========================================

'for optimization purposes, the original test is preceeded by
'test divisions through the primes 2,3,5 and the following primes in the primes() array
'because these are the most frequent prime factors

'this example shows the nearly seamless integration of big integer calculation into Freebasic Code
'and uses different operations on these big integers
'most of them look like operations with normal datatypes because of the overloading
'but for optimization purposes, you can access the member functions directly

#Include Once "big_integer.bi"

Declare Function randomstring(nbytes as Integer) as String
Declare Function mod_pow32(ByRef bb As ULong, ByRef ee As ULong, ByRef m As ULong) As ULong
Declare Function mod_square32(ByRef b As ULong, ByRef m As ULong) As ULong
Declare Function primearray(primes() As ULong) As ULong
Declare Function isprime(ByRef p As Bigint, iters As UShort, primes() As ULong) As Boolean

'get the first 1000 primes into an array
Dim primes(0 To 1000) As ULong
primearray( primes() )

Dim p As Bigint
Dim i as Integer
Dim r As String

'generate random 1024bit numbers until a prime is found
For i = 1 To 1000
	r = randomstring(128)
	p = CVUBigint(r)	'conversion of a binary string to a bigint (unsigned)
	If isprime(p, 100, primes() ) Then
		Print "1024 bit Prime number found in try #" & i
		Print "decimal:"
		Print p
		Print ""
		Print "hexadecimal:"
		Print UHex(p)
		Print Hex(r)
		Sleep
		End
	EndIf
Next i

Print "No prime found in " & i-1 & " tries"
Sleep
End


'test if a number is (pseudo)prime
'p: input number to test
'iters: iterations of the test, after 100 the possibility of a false positive result is neglible
'			even for 2048bit numbers
'primes(): array of the lowest primes (arraysize 1000 to 10000), starting with 7
'			arraysize has to be minimum 8 for the first 8 iterations
'Return value: True (is prime) or False (is not prime or error)
Function isprime(ByRef p As Bigint, iters As UShort, primes() As ULong) As Boolean
	Dim As ULong i, j, k

	'the test accepts only positive values for p
	If p < 2 Then
		Print "Error: p has to be bigger than 2"
		Return FALSE
	EndIf

	If LBound(primes) <> 0 Then
		Print "Error: primes() array is out of bounds"
		Return FALSE
	EndIf
	If UBound(primes) < 8 Then
		Print "Error: primes() array is out of bounds"
		Return FALSE
	EndIf

	'get the size of p in bytes
	Dim pstring As String = UHex(p)		'conversion of bigint to String in Hexadecimal notation (unsigned)
	Dim bytelen As UShort = Len(pstring) \ 2

	'check LSB, if 0 => p is multiple of 2
	If bigint.bit_value(p,0) = 0 Then
		Print "p is multiple of 2"
		Return FALSE
	EndIf

	'cast of bigint to decimal String
	pstring=p
	pstring=LTrim(pstring,"+")		'strip the sign

	'build horizontal checksum: test if multiple of 3
	Dim hcheck As ULong
	For i = 0 To (Len(pstring)-1)
		hcheck += Val(Chr(pstring[i]))
	Next i
	If hcheck Mod 3 = 0 Then
		Print "p is multiple of 3"
		Return FALSE
	EndIf

	'check lowest decimal: if 5 or 0, it is multiple of 3
	Dim lsd As UByte = pstring[Len(pstring)-1]
	If lsd = Asc("0") Or lsd = Asc("5") Then
		Print "p is multiple of 5"
		Return FALSE
	EndIf

	'testdivison modulus through the content of the primes array
	For i = LBound(primes) To UBound(primes)
		If p Mod primes(i) = 0 Then
			Print "p is multiple of " & primes(i)
			Return FALSE
		EndIf
	Next i

	'start of the Miller-Rabin test
	'factorize p-1 into d * 2^j
	Dim As Bigint d = p - 1
	Dim As Bigint a, x, pminus1
	Do
		If Bigint.Bit_Value(d,0) = 0 Then
			j += 1
			d = Bigint.div2(d)	'direct access to member function, Shr= 1 would also work
		Else
			Exit Do
		End If
	Loop

	pminus1 = p-1

	'iterations: values for a:
	'a valid strategy is to try a number of low primes for a first, then random numbers
	For k = 0 To iters - 1
		If k <= ubound(primes) Then
			a=primes(k)
		Else
			'generate random value for a
			a = CVUBigint(randomstring(bytelen))	'conversion of a binary string to a bigint (unsigned)
			a Mod= p
			If a <= primes(ubound(primes)) Then	'try again
				k -= 1
				Continue For
			EndIf
		End If

		'first step:
		'if x = a ^ d mod p = (1 or -1) then p is probably prime
		x = Bigint.modpower(a, d, p)
		If x = 1 Or x = pminus1 Then 
            Print "p is a Fermat pseudoprime to base " & a
            Continue For    'probably prime, try next value of a
        End If
        
		'second step:
		'calculate x = x^2 mod p and repeat j times
		For i=1 To (j-1)
			x = x ^ 2
			x Mod= p
			If x = 1 Then Return FALSE  'if x=1 then p is not prime
			If x = pminus1 Then
                Print "p is a strong pseudoprime to base " & a
                Continue For, For 'if x=-1 then probably prime, try next value of a
            End If
		Next i

		'if no -1 appears, it is not prime
		Return FALSE

	Next k

	Return TRUE    'p is prime with a high probability

End Function

'generates a random number as binary String with length nbytes
Function randomstring(nbytes as Integer) as String
	Randomize
	Dim heaps as Integer = nbytes \ 4
	If nbytes Mod 4 <> 0 Then heaps += 1
	Dim s as String
	For i as Integer = 1 to heaps
		Dim u as ULong = CUlng(Rnd * 4294967295)
		s &= Mkl(u)
	Next i
	Return Left(s,nbytes)
End Function

'generates an array of primes, starting with 2
'using the Miller-Rabin Prime Test for 32bit-UIntegers (same as above for Bigint)
Function primearray(primes() As ULong) As ULong

    Dim p As Ulong = 1
	Dim As Ulong i, j, a, pminus1, div, modu, x, d
	
    primes(LBound(primes)) = 2
    If UBound(primes) - Lbound(primes) > 0 Then primes(LBound(primes)+1) = 3

	Dim z As UInteger

	For z = LBound(primes) + 2 To UBound(primes)

		Do  'try increasing numbers until prime is found
			p += 2

			'factorize p-1 into d * 2^j
			d = p - 1
			j = 0
			Do
				If Bit(d, 0) = 0 Then
					j += 1
					d Shr= 1
				Else
					Exit Do
				End If
			Loop

			pminus1 = p - 1

			For a = 2 To 3
				'first step:
				'if x = a ^ d mod p = (1 or -1) then p is probably prime
				'for numbers < 1.373.653 the test is deterministic with these two values for a
				x = mod_pow32(a, d, p)
				If x = 1 Or x = pminus1 Then Continue For	'probably prime, try next value of a

				'second step:
				'calculate x = x^2 mod p and repeat j times
				For i = 1 To j - 1
					x = mod_square32(x, p)
					If x=1 Then Continue Do    'if x=1 then p is not prime
					If x=pminus1 Then Continue For, For 'if x=-1 then probably prime, try next value of a
				Next i

				'if no -1 appears, it is not prime
				Continue Do

			Next a

			primes(z)=p
			Exit Do

		Loop    'pro forma

	Next z

	Return 1

End Function

'modular exponentiation @32bit
Function mod_pow32(ByRef bb As ULong, ByRef ee As ULong, ByRef m As ULong) As ULong
	If ee < 0 Then Print "Cannot raise a integer to a negative power"
	If ee = 0 Then Return 1
	Dim As LongInt r=1, b=bb
	Dim As Byte i,bitlen
	For i = 31 To 0 Step -1
		If Bit(ee,i) Then
			bitlen = i
			Exit For
		End If
	Next i
	b Mod= m
	For i = 0 To bitlen-1
		If Bit(ee,i) Then
			r *= b
			r Mod= m
		End If
		b = b * b
		b Mod= m
	Next i
	r *= b
	r Mod= m
	Return CULng(r)
End Function

'modular square @32bit
Function mod_square32(ByRef b As ULong, ByRef m As ULong) As ULong
	Dim As LongInt r
	r = b*b
	r Mod= m
	Return CULng(r)
End Function

