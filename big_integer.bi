' version 2.8.1 29 July 2018
'================================================================
' by Stephan Brunker (stephanbrunker at web punkt de)
' based on version 1.2 by Richard @ freebasic.net/forum
' with credits to: Hartmut Ruske and frisian
'================================================================
' The easy way to use - analog to the "hello world" the 1+1:
'------------------------------
' #include "big_integer.bi"
' Dim as Bigint a,b,c,d
' a = 1234
' b = "1357239875892745982784975230987590287"
' c = VALBigint("&h3a84458e9bc47")
' d = a + b * c
' print d
'------------------------------
' hex and string input can be so long as you want - up to 4.29E09 bytes.
' input can be any data type (ulong, ulongint)
' also strings coded in decimal, hex or binary(endian reversed)
' all the operators are overloaded
' also all the comparators: = < > <> >= <=
' for additional functions look in the code, it should be
' well commented to see for what the functions are for
' you can also input and output in various formats, see section
' "Conversion Functions"
'===============================================================
' This package only handles integers encoded in a two's complement format.
' The first bit in the two's complement format is always the sign which
' takes the value of 1 = negative, 0 = positive or zero. Byte examples are;
' +127 = 0111 1111  most positive
'   +8 = 0000 1000
'   +7 = 0000 0111
'   +4 = 0000 0100
'   +2 = 0000 0010
'   +1 = 0000 0001
'    0 = 0000 0000  zero
'   -1 = 1111 1111
'   -2 = 1111 1110
'   -4 = 1111 1100
'   -7 = 1111 1001
'   -8 = 1111 1000
' -127 = 1000 0001
' -128 = 1000 0000  most negative
'----------------------------------------------------------------
' Each successive 32 bits are packed into a 4 byte block.
' Each block is stored in 4 successive bytes of a string.
'----------------------------------------------------------------
' Strings in FreeBASIC are indexed and printed from left to right. Big
' integers appear to be stored in strings backwards which can be confusing.
' The Left side of the string is the Right side of the number and vice versa.
' Beware: Shift_Left moves a string to the right, Shift_Right moves it left.
'----------------------------------------------------------------
' The first byte in the string is the least significant byte of the number.
' The last block in the string is the most significant block of the number.
' String s indexing has s[0] as the LS byte and s[len(s)-1] as the MS byte.
' These big integer strings are always multiples of 4 bytes long.
' The msb of a number is sign extended to the MS bit of the MS block.
'----------------------------------------------------------------
' A number is always stored in a way that correctly represents that number.
' Where an operation would overflow into the MSB, a sign block is
' appended to the number so as to prevent overflow or sign change.
' Unnecessary leading zero or all ones blocks are removed by prune.
'----------------------------------------------------------------
' String pointers may change if a string is created or any length is changed.

'================================================================
Type Bigint     ' a little endian, two's complement binary number
	s As String ' packed into a string, in blocks four bytes long

	' Constructors
	'-------------
	Declare Constructor ()                    ' default constructor
	Declare Constructor (ByRef a As Bigint)   ' copy constructor
	Declare Constructor (ByRef a As Byte)
	Declare Constructor (ByRef a As UByte)
	Declare Constructor (ByRef a As Short)
	Declare Constructor (ByRef a As UShort)
	Declare Constructor (ByRef a As Long)
	Declare Constructor (ByRef a As ULong)
	Declare Constructor (ByRef a As Integer)
	Declare Constructor (ByRef a As UInteger)
	Declare Constructor (ByRef a As LongInt)
	Declare Constructor (ByRef a As ULongInt)
	Declare Constructor (ByRef a As Single)
	Declare Constructor (ByRef a As Double)
	Declare Constructor (ByRef a As String)

	' let
	'----
	Declare Operator Let (ByRef a As Bigint)

	' cast to other datatypes
	'------------------------
	Declare Operator Cast() As Byte     ' CByte
	Declare Operator Cast() As UByte    ' CUByte
	Declare Operator Cast() As Short    ' CShort
	Declare Operator Cast() As UShort   ' CUShort
	Declare Operator Cast() As Long     ' CLng
	Declare Operator Cast() As ULong    ' CULng
	Declare Operator Cast() As LongInt  ' CLngint
	Declare Operator Cast() As ULongInt ' CULngint
	Declare Operator Cast() As Integer  ' Cint
	Declare Operator Cast() As UInteger ' CUInt
	Declare Operator Cast() As Single   ' CSng
	Declare Operator Cast() As Double   ' CDbl
	Declare Operator Cast() As String   ' Str

	' functions
	'----------
	' private: (cannot make them really protected because the operators access them)
	'using them directly on your own risk, because everything is passed ByRef and changed internally
	Declare Static Function compare (ByRef a As Bigint, ByRef b As Bigint) As Long
	Declare Static Function square(ByRef aa As Bigint) As Bigint
	Declare Static Function mul2(ByRef a As Bigint) As Bigint
	Declare Static Function div2(ByRef a As Bigint) As Bigint
	Declare Static Sub div(ByRef aa As Bigint, ByRef bb As Bigint,ByRef q As Bigint, ByRef r As Bigint)
	Declare Static Sub prune(ByRef a As Bigint)

	' public:
	Declare Static Function modpower(ByRef bb As Bigint, ByRef ee As Bigint, ByRef m As Bigint) As Bigint
	Declare Static Function factorial (ByRef a As Bigint) As Bigint
	Declare Static Function msbit(ByRef a As Bigint) As Long
	' Overloading Bit / Bitset / BitReset is not possible, because these are macros
	' containing a "cast(bigint,1)" operator, and you can only overload a cast from bigint to other
	' and not to the bigint itself (this had to be done in the type integer for example)
	' Couriously, #undef bit and declare a new Function bit(a as bigint, b as Ulongint)
	' would work in this file, but not if the file is included in another project
	Declare Static Function Bit_Value(ByRef v As Bigint, ByVal b As ULongInt) As Long
	Declare Static Function Bit_Set(ByRef vv As Bigint, ByVal b As ULongInt) As Bigint
	Declare Static Function Bit_Reset(ByRef vv As Bigint, ByVal b As ULongInt) As Bigint

	' implicit step versions
	Declare Operator For ()
	Declare Operator Step()
	Declare Operator Next(ByRef end_cond As Bigint) As Integer

	' explicit step versions
	Declare Operator For (ByRef step_var As Bigint)
	Declare Operator Step(ByRef step_var As Bigint)
	Declare Operator Next(ByRef end_cond As Bigint, ByRef step_var As Bigint ) As Integer

	' operate and assign
	Declare Operator += (ByRef rhs As Bigint)
	Declare Operator -= (ByRef rhs As Bigint)
	Declare Operator *= (ByRef rhs As Bigint)
	Declare Operator \= (ByRef rhs As Bigint)
	Declare Operator Mod= (ByRef rhs As Bigint)
	Declare Operator ^= (ByRef rhs As Bigint)
	Declare Operator Shl= (ByRef rhs As LongInt)
	Declare Operator Shr= (ByRef rhs As LongInt)
	Declare Operator And= (ByRef rhs As Bigint)
	Declare Operator Or= (ByRef rhs As Bigint)
	Declare Operator Xor= (ByRef rhs As Bigint)
	Declare Operator Imp= (ByRef rhs As Bigint)
	Declare Operator Eqv= (ByRef rhs As Bigint)

End Type

'================================================================
'    OVERLOADED OPERATORS:
'================================================================

' Comparators
'-----------
Declare Operator <     (ByRef As Bigint, ByRef As Bigint) As Integer
Declare Operator >     (ByRef As Bigint, ByRef As Bigint) As Integer
Declare Operator =     (ByRef As Bigint, ByRef As Bigint) As Integer
Declare Operator <>    (ByRef a As Bigint, ByRef b As Bigint) As Integer
Declare Operator <=    (ByRef a As Bigint, ByRef b As Bigint) As Integer
Declare Operator >=    (ByRef a As Bigint, ByRef b As Bigint) As Integer

' Mathematical Operators
'-----------------------
Declare Operator +     (ByRef x As Bigint) As Bigint
Declare Operator -     (ByRef x As Bigint) As Bigint
Declare Operator +     (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator -     (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator *     (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator \     (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator Mod   (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator ^     (ByRef x As Bigint, ByRef y As LongInt) As Bigint
Declare Operator Abs   (Byref x As Bigint) As Bigint
Declare Operator Sgn   (Byref x As Bigint) As Integer

' Bitwise Operations
'-------------------
Declare Operator Not   (ByRef x As Bigint) As Bigint
Declare Operator And   (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator Or    (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator Xor   (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator Imp   (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator Eqv   (ByRef x As Bigint, ByRef y As Bigint) As Bigint
Declare Operator Shl   (Byref a As Bigint, ByVal n As LongInt) As Bigint
Declare Operator Shr   (Byref a As Bigint, ByVal n As LongInt) As Bigint

' Conversion Functions
'---------------------
Declare Function CBig Overload(a as Byte) as Bigint
Declare Function CBig Overload(a as UByte) as Bigint
Declare Function CBig Overload(a as Short) as Bigint
Declare Function CBig Overload(a as UShort) as Bigint
Declare Function CBig Overload(a as Integer) as Bigint
Declare Function CBig Overload(a as UInteger) as Bigint
Declare Function CBig Overload(a as Long) as Bigint
Declare Function CBig Overload(a as ULong) as Bigint
Declare Function CBig Overload(a as LongInt) as Bigint
Declare Function CBig Overload(a as ULongInt) as Bigint
Declare Function CBig Overload(a as Single) as Bigint
Declare Function CBig Overload(a as Double) as Bigint
Declare Function CBig Overload(a as String) as Bigint
Declare Function Bin(ByRef s As Bigint) As String
Declare Function Hex(ByRef s As Bigint) As String
Declare Function Hex(ByRef s As Bigint, ByRef n As Ulong) As String
Declare Function Uhex(ByRef s As Bigint) As String
Declare Function UHexT(ByRef s As Bigint,ByRef n As ULong) As String
Declare Function Oct(ByRef s As Bigint) As String
Declare Function MkBigint(ByRef a As Bigint) As String
Declare Function MkUBigint(ByRef a As Bigint) As String
Declare Function ValBigint(ByRef a As String) As Bigint
Declare Function ValUBigint(ByRef a As String) As Bigint
Declare Function CVBigint(ByRef aa As String) As Bigint
Declare Function CVUBigint(ByRef aa As String) As Bigint

#Include "modules/constants.bi"
#Include "modules/constructors.bi"
#Include "modules/operators.bi"
#Include "modules/operate_assign.bi"
#Include "modules/compare.bi"
#Include "modules/for_next.bi"
#Include "modules/arithfunctions.bi"
#Include "modules/bitfunctions.bi"
#Include "modules/cast.bi"
#Include "modules/conversion.bi"

