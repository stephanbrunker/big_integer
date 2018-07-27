'================================================================
'       BIGINT OPERATE AND ASSIGN
'================================================================
Operator Bigint.+= (ByRef rhs As Bigint)
	This = This + rhs
End Operator

Operator Bigint.-= (ByRef rhs As Bigint)
	This = This - rhs
End Operator

Operator Bigint.*= (ByRef rhs As Bigint)
	This = This * rhs
End Operator

Operator Bigint.\= (ByRef rhs As Bigint)
	Dim As Bigint c = This, d
	bigint.div(c,rhs,This,d)
End Operator

Operator Bigint.mod= (ByRef rhs As Bigint)
	Dim As Bigint c, d = This
	bigint.div(d,rhs,c,This)
End Operator

Operator Bigint.^= (ByRef rhs As Bigint)
	This = This ^ rhs
End Operator

Operator Bigint.shl= (ByRef rhs As LongInt)
	This = This Shl rhs
End Operator

Operator Bigint.shr= (ByRef rhs As LongInt)
	This = This Shr rhs
End Operator

Operator Bigint.and= (ByRef rhs As Bigint)
	This = This And rhs
End Operator

Operator Bigint.or= (ByRef rhs As Bigint)
	This = This Or rhs
End Operator

Operator Bigint.xor= (ByRef rhs As Bigint)
	This = This Xor rhs
End Operator

Operator Bigint.imp= (ByRef rhs As Bigint)
	This = This Imp rhs
End Operator

Operator Bigint.eqv= (ByRef rhs As Bigint)
	This = This Eqv rhs
End Operator
