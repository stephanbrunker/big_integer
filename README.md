# Big Integer Extension for FreeBasic Compiler

If you want to work with arbitrary big integer values larger than 64bits, you need an extension or library. Part of the Freebasic package is or was the `largeint.bas` file and the `big_int` library. Both are not well documentated and difficult to use. At the freebasic.net forum, Richard had an idea and created an extension based on overloading the operators so that the Bigint datatype would behave like a builtin FB datatype. Everything is in code and would be compiled together with the application, so it is also easy to look into the code to know what it does and expand the functionality.

That is what I did, because Richards version was quite slow for some operations and not fully overloaded, also had standalone functions. I optimized most of the code - mostly the division and the comparation - and overloaded the datatype fully and made all the functions static member functions. Now it should be on par to the `largeint.bas` extension. Perhaps less in comparision to external libraries not written in FB, but you'll get a very clean and easy to read code in return, because you can use the Bigint type just like an Integer. It can't get easier than that. For performance-critical applications, you won't use Freebasic anyway, because the focus is on easy usage, coding and understanding of the written code, not on maximum performance in terms of processor cycles. How often you have searched for an missing `}` in C++ or PHP codes? Doesn't happen in FB ... 

There are only a few exceptions:
* the constants 0 (4 bytes), 0 (8 bytes), 1, 2 and -1 are standalone in the format `Bigint_sXX (Shared as String)` because it is not possible to make Strings static members.
* `Cast(Bigint, Any other Datatype )` doesn't work, because you would have to define that cast in all the other datatypes. That is not much a problem, because all the Constructors are defined, so an assign will automatically cast the datatype to Bigint. The `CBig()` function does exactly that.
* Because of that, `Bit`, `BitSet` and `BitReset` don't work because they use an `CAST` internally. Just call `Bigint.Bit_Value`, `Bigint.Bit_Set` and `Bigint.Bit_Reset` instead
* the `/` division and  `Sqr()` aren't implemented because this is integer mathematics. Instead, exponentiation modulus (`Bigint.modpower`) and `Bigint.factorial` are implemented.

For better readability, the Big_integer code is split into several files internally (in total 2000 lines), to bind it you have only to include `"big_integer.bi"`

## Data Storage:
The Big Integer values are stored as Strings in a two's complement format. For special operations (but not neccessary or recommended), this string can be accessed with bigint.s . The binary string is little endian in multiples of 4 byte blocks, with the most significant bit as the sign. If the operation needs to expand the string, this is done internally, likewise with bigint.prune unneccessary empty blocks are removed automatically inside the functions. 

## Usage:
The Bigint datatype can be used like an Integer datatype:

```
Dim As Bigint a,b,c
a = 30
b = 90
c = a * b
```

The mathematical operators are: `+ - * \ Mod ^ abs() sgn()`

additional functions: `Bigint.modpower(base,exponent,modulus)` `Bigint.factorial()`

also in the form of operate and assign: `+= -= *= \= Mod= ^=`

You can compare two Bigints with: `> < = <> <= >=`

And you can make bitwise operations: `Not And Or Xor Imp Eqv Shl Shr`

also with assign: `And= Or= Xor= Imp= Eqv= Shl= Shr=`

And bit manipulation:\
`Bigint.Bit_Value(bigint, bitnum)`\
`Bitint.Bit_Set(bigint, bitnum)`\
`Bigint.Bit_Reset(bigint, bitnum)`

Even a `For...Step...Next` control loop will work with Bigint values.

Constructors:
Bigints can be constructed by assigning from an `Byte`, `UByte`, `Short`, `UShort`, `Integer`, `UInteger`, `Long`, `Ulong`, `LongInt`, `ULongInt`, `Single` and `Double` 


## Conversion Functions:
The builtin conversion functions which apply to Integers will also apply to Bigints, as:
`Bin() Hex() Hex(bigint, length) Oct()`

I have added an `UHex()` and `UHexT(bigint, length)` function which will return an unsigned hexadecimal value which means if the highest bit is set, it won't be extended into the next 4 byte block which will be &hFFFFFFFF in the case of an signed hex. So, &FF means 255 and not -1, while in signed operation, 255 would be &00FF (in this bytewise example). These conversions are all in Big Endian notation with the MSB as first character.

The `MkI()` function family for a binary copy is represented by `MkBigint()` and `MkUBigint()`, also with the unsigned variant. Not surprisingly, `MkBigint()` simply returns the storage string `bigint.s`, which is already a binary string. Here, the output is little endian.

The opposite function family `CVI()` for a Bigint from an binary string is accordingly `CVBigint()` and `CVUBigint()`.

### Conversion from Strings:
The `Val()` function is overlapping with the constructor from strings. Without any prefix, the string is assumed to be an positive decimal, but the sign can be given explicitly with `+` or `-`. Other input formats can be given with the prefixes `&h` for hexadecimal, `&b` for dual, `&o` or only `&` for octal. The suffix `u` for unsigned operation is also supported. So `ValBigint()` and `ValUBigint()` will function, but simply are calling the constructor. 

## Authors:
Stephan Brunker\
Hartmut Ruske\
From the freebasic.net forum:\
Richard\
frisian

## Example
#### example_bigint_isprime.bas
I have made an example for the bigint operation by implementing the Miller-Rabin primality test. The example will randomly generate 1000 numbers, each with size 1048 bits and then test if they are prime, stopping if a prime is found. To make it simple, I used the internal `Rnd()` command for that. Simply don't use this example for real crypto ... because it is not really safely random. For that, an implementation of Salsa20 or any other cryptsafe Pseudorandom Generator is needed.

For 1024bit normally less than 1000 tries are needed. For optimation purposes, the example will use an integer version of the algorithm to generate the first 1000 primes into an array (Function `primearray()` ). It is much faster to eradicate the simple options like multiples of 2,3,5 and a testdivision through said first 1000 primes instead of running the complete test which calls to exponentiation modulus and squaring modulus repeatedly - which means 1048bit divisions as the most time-consuming operations. After the coded 100 iterations of the test to the bases of the first 100 primes the probability that the number is not a prime is neglible.  

I used a further optimized version of this code to generate 1048bit Sophie-Germain prime numbers, which are needed for the ElGamal assymmetric encryption which is based on the Diffie-Hellman protocol. For these primes 2*p + 1 is also prime and they are a lot more rare. (A hint: if you know that your number is a multiple of 2, n+1 is not)
