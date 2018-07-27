'================================================================
'       BIGINT FOR, NEXT and STEP
'================================================================
' implicit step versions. Implicit step is 1
Operator Bigint.for( )
End Operator

Operator Bigint.step( )
	This += 1
End Operator

Operator Bigint.next( ByRef end_cond As Bigint ) As Integer
	Return This <= end_cond
End Operator

'----------------------------------------------------------------
' explicit step versions
Operator Bigint.for( ByRef step_var As Bigint )
End Operator

Operator Bigint.step( ByRef step_var As Bigint )
	This += step_var
End Operator

Operator Bigint.next( ByRef end_cond As Bigint, ByRef step_var As Bigint ) As Integer
	If step_var < 0 Then
		Return This >= end_cond
	Else
		Return This <= end_cond
	End If
End Operator
