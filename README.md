# BinaryArithmetic

This is a collection of Haskell implementations of words and word operations that, roughly speaking, correspond to the Verilog operations. These are intended to be *concrete*, meaning that, for example, words of length 8 and related operations are represented using 
``haskell
data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving Eq 
``

These are meant to correspond to the `rewire-user` module in the ReWire language, which define words and their operations using nat-indexed vectors of `Bool`s. For the purposes of debugging and testing, it helps to keep concrete representations around.
