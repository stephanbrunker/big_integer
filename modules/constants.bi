'================================================================
'        BIGINT CONSTANTS
'================================================================
' pre-defining the strings for certain values is much faster
' then generating them on the fly, also operations with them
Dim Shared As String Bigint_s0
Bigint_s0 = Chr(0,0,0,0)
Dim Shared As String Bigint_s00
Bigint_s00 = Chr(0,0,0,0,0,0,0,0)
Dim Shared As String Bigint_s1
Bigint_s1 = Chr(1,0,0,0)
Dim Shared As String Bigint_s2
Bigint_s2 = Chr(2,0,0,0)
Dim Shared As String Bigint_s_1
Bigint_s_1 = Chr(-1,-1,-1,-1)
