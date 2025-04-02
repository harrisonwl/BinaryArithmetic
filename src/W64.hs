module W64 where

import Prelude as P hiding ((||) , (&&))
import BinaryArithmetic as BA
    ( ShowBin(..),
      ShowHex(..),
      ToBits(..),
      BinaryArith(..),
      Bit(..),
      (<&&>),
      (<||>),
      (<|*|>),
      (<*>),
      negate,
      pad_or_trunc,
      fBit,
      bitplus,
      boothround,
      zero,
      hexify )

data W64 = W64 Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
               Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit Bit
                  deriving Eq

instance Show W64 where
--  show w64 = "0x" ++ xshow w64 -- "0b" ++ bshow w64
  show w64 = xshow w64

instance Num W64 where
  (+)         = (BA.+)
  (*)         = (BA.<*>)
  abs w64      = case signbit w64 of
    C -> w64
    S -> BA.negate w64
  signum w64   = case signbit w64 of
    C -> one
    S -> BA.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bnot i') BA.+ one
    where
      [b62,b61,
       b60,b59,b58,b57,b56,b55,b54,b53,b52,b51,
       b50,b49,b48,b47,b46,b45,b44,b43,b42,b41,
       b40,b39,b38,b37,b36,b35,b34,b33,b32,b31,
       b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
       b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
       b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0] = pad_or_trunc 63 i
      i'                     = W64 C b62 b61 
                                     b60 b59 b58 b57 b56 b55 b54 b53 b52 b51 
                                     b50 b49 b48 b47 b46 b45 b44 b43 b42 b41 
                                     b40 b39 b38 b37 b36 b35 b34 b33 b32 b31 
                                     b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
                                     b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
                                     b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0
  negate      = BA.negate


instance BinaryArith W64 where
  a64 + b64 = fst $ carryadd a64 b64 C
  carryadd (W64 a63 a62 a61 a60
            a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
            a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
            a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
            a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
            a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
            a09 a08 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W64 b63 b62 b61 b60
            b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
            b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
            b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
            b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
            b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
            b09 b08 b07 b06 b05 b04 b03 b02 b01 b00) c =
    (W64 c63 c62 c61 c60
            c59 c58 c57 c56 c55 c54 c53 c52 c51 c50
            c49 c48 c47 c46 c45 c44 c43 c42 c41 c40
            c39 c38 c37 c36 c35 c34 c33 c32 c31 c30
            c29 c28 c27 c26 c25 c24 c23 c22 c21 c20
            c19 c18 c17 c16 c15 c14 c13 c12 c11 c10
            c09 c08 c07 c06 c05 c04 c03 c02 c01 c00 ,
      c')
    where
      (carry0,c00) = bitplus a00 b00 c
      (carry1,c01) = bitplus a01 b01 carry0
      (carry2,c02) = bitplus a02 b02 carry1
      (carry3,c03) = bitplus a03 b03 carry2
      (carry4,c04) = bitplus a04 b04 carry3
      (carry5,c05) = bitplus a05 b05 carry4
      (carry6,c06) = bitplus a06 b06 carry5
      (carry7,c07) = bitplus a07 b07 carry6
      (carry8,c08) = bitplus a08 b08 carry7
      (carry9,c09) = bitplus a09 b09 carry8

      (carry10,c10) = bitplus a10 b10 carry9
      (carry11,c11) = bitplus a11 b11 carry10
      (carry12,c12) = bitplus a12 b12 carry11
      (carry13,c13) = bitplus a13 b13 carry12
      (carry14,c14) = bitplus a14 b14 carry13
      (carry15,c15) = bitplus a15 b15 carry14
      (carry16,c16) = bitplus a16 b16 carry15
      (carry17,c17) = bitplus a17 b17 carry16
      (carry18,c18) = bitplus a18 b18 carry17
      (carry19,c19) = bitplus a19 b19 carry18

      (carry20,c20) = bitplus a20 b20 carry19
      (carry21,c21) = bitplus a21 b21 carry20
      (carry22,c22) = bitplus a22 b22 carry21
      (carry23,c23) = bitplus a23 b23 carry22
      (carry24,c24) = bitplus a24 b24 carry23
      (carry25,c25) = bitplus a25 b25 carry24
      (carry26,c26) = bitplus a26 b26 carry25
      (carry27,c27) = bitplus a27 b27 carry26
      (carry28,c28) = bitplus a28 b28 carry27
      (carry29,c29) = bitplus a29 b29 carry28

      (carry30,c30) = bitplus a30 b30 carry29
      (carry31,c31) = bitplus a31 b31 carry30
      (carry32,c32) = bitplus a32 b32 carry31
      (carry33,c33) = bitplus a33 b33 carry32
      (carry34,c34) = bitplus a34 b34 carry33
      (carry35,c35) = bitplus a35 b35 carry34
      (carry36,c36) = bitplus a36 b36 carry35
      (carry37,c37) = bitplus a37 b37 carry36
      (carry38,c38) = bitplus a38 b38 carry37
      (carry39,c39) = bitplus a39 b39 carry38

      (carry40,c40) = bitplus a40 b40 carry39
      (carry41,c41) = bitplus a41 b41 carry40
      (carry42,c42) = bitplus a42 b42 carry41
      (carry43,c43) = bitplus a43 b43 carry42
      (carry44,c44) = bitplus a44 b44 carry43
      (carry45,c45) = bitplus a45 b45 carry44
      (carry46,c46) = bitplus a46 b46 carry45
      (carry47,c47) = bitplus a47 b47 carry46
      (carry48,c48) = bitplus a48 b48 carry47
      (carry49,c49) = bitplus a49 b49 carry48

      (carry50,c50) = bitplus a50 b50 carry49
      (carry51,c51) = bitplus a51 b51 carry50
      (carry52,c52) = bitplus a52 b52 carry51
      (carry53,c53) = bitplus a53 b53 carry52
      (carry54,c54) = bitplus a54 b54 carry53
      (carry55,c55) = bitplus a55 b55 carry54
      (carry56,c56) = bitplus a56 b56 carry55
      (carry57,c57) = bitplus a57 b57 carry56
      (carry58,c58) = bitplus a58 b58 carry57
      (carry59,c59) = bitplus a59 b59 carry58

      (carry60,c60) = bitplus a60 b60 carry59
      (carry61,c61) = bitplus a61 b61 carry60
      (carry62,c62) = bitplus a62 b62 carry61

      (c',c63)       = bitplus a63 b63 carry62

  leastbit (W64 _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ b00) = b00

  (.&.) (W64 a63 a62 a61 a60
            a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
            a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
            a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
            a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
            a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
            a09 a08 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W64 b63 b62 b61 b60
            b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
            b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
            b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
            b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
            b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
            b09 b08 b07 b06 b05 b04 b03 b02 b01 b00) =
    W64 (a63 <&&> b63) (a62 <&&> b62) (a61 <&&> b61) (a60 <&&> b60)
        (a59 <&&> b59) (a58 <&&> b58) (a57 <&&> b57) (a56 <&&> b56)
        (a55 <&&> b55) (a54 <&&> b54) (a53 <&&> b53) (a52 <&&> b52)
        (a51 <&&> b51) (a50 <&&> b50) (a49 <&&> b49) (a48 <&&> b48)
        (a47 <&&> b47) (a46 <&&> b46) (a45 <&&> b45) (a44 <&&> b44)
        (a43 <&&> b43) (a42 <&&> b42) (a41 <&&> b41) (a40 <&&> b40)
        (a39 <&&> b39) (a38 <&&> b38) (a37 <&&> b37) (a36 <&&> b36)
        (a35 <&&> b35) (a34 <&&> b34) (a33 <&&> b33) (a32 <&&> b32)
        (a31 <&&> b31) (a30 <&&> b30) (a29 <&&> b29) (a28 <&&> b28)
        (a27 <&&> b27) (a26 <&&> b26) (a25 <&&> b25) (a24 <&&> b24)
        (a23 <&&> b23) (a22 <&&> b22) (a21 <&&> b21) (a20 <&&> b20)
        (a19 <&&> b19) (a18 <&&> b18) (a17 <&&> b17) (a16 <&&> b16)
        (a15 <&&> b15) (a14 <&&> b14) (a13 <&&> b13) (a12 <&&> b12)
        (a11 <&&> b11) (a10 <&&> b10) (a09 <&&> b09) (a08 <&&> b08)
        (a07 <&&> b07) (a06 <&&> b06) (a05 <&&> b05) (a04 <&&> b04)
        (a03 <&&> b03) (a02 <&&> b02) (a01 <&&> b01) (a00 <&&> b00)


  (||) (W64 a63 a62 a61 a60
            a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
            a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
            a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
            a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
            a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
            a09 a08 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W64 b63 b62 b61 b60
            b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
            b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
            b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
            b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
            b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
            b09 b08 b07 b06 b05 b04 b03 b02 b01 b00) =
    W64 (a63 <||> b63) (a62 <||> b62) (a61 <||> b61) (a60 <||> b60)
        (a59 <||> b59) (a58 <||> b58) (a57 <||> b57) (a56 <||> b56)
        (a55 <||> b55) (a54 <||> b54) (a53 <||> b53) (a52 <||> b52)
        (a51 <||> b51) (a50 <||> b50) (a49 <||> b49) (a48 <||> b48)
        (a47 <||> b47) (a46 <||> b46) (a45 <||> b45) (a44 <||> b44)
        (a43 <||> b43) (a42 <||> b42) (a41 <||> b41) (a40 <||> b40)
        (a39 <||> b39) (a38 <||> b38) (a37 <||> b37) (a36 <||> b36)
        (a35 <||> b35) (a34 <||> b34) (a33 <||> b33) (a32 <||> b32)
        (a31 <||> b31) (a30 <||> b30) (a29 <||> b29) (a28 <||> b28)
        (a27 <||> b27) (a26 <||> b26) (a25 <||> b25) (a24 <||> b24)
        (a23 <||> b23) (a22 <||> b22) (a21 <||> b21) (a20 <||> b20)
        (a19 <||> b19) (a18 <||> b18) (a17 <||> b17) (a16 <||> b16)
        (a15 <||> b15) (a14 <||> b14) (a13 <||> b13) (a12 <||> b12)
        (a11 <||> b11) (a10 <||> b10) (a09 <||> b09) (a08 <||> b08)
        (a07 <||> b07) (a06 <||> b06) (a05 <||> b05) (a04 <||> b04)
        (a03 <||> b03) (a02 <||> b02) (a01 <||> b01) (a00 <||> b00)

  (^) (W64 a63 a62 a61 a60
            a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
            a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
            a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
            a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
            a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
            a09 a08 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W64 b63 b62 b61 b60
            b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
            b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
            b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
            b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
            b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
            b09 b08 b07 b06 b05 b04 b03 b02 b01 b00) =
    W64 (a63 <|*|> b63) (a62 <|*|> b62) (a61 <|*|> b61) (a60 <|*|> b60)
        (a59 <|*|> b59) (a58 <|*|> b58) (a57 <|*|> b57) (a56 <|*|> b56)
        (a55 <|*|> b55) (a54 <|*|> b54) (a53 <|*|> b53) (a52 <|*|> b52)
        (a51 <|*|> b51) (a50 <|*|> b50) (a49 <|*|> b49) (a48 <|*|> b48)
        (a47 <|*|> b47) (a46 <|*|> b46) (a45 <|*|> b45) (a44 <|*|> b44)
        (a43 <|*|> b43) (a42 <|*|> b42) (a41 <|*|> b41) (a40 <|*|> b40)
        (a39 <|*|> b39) (a38 <|*|> b38) (a37 <|*|> b37) (a36 <|*|> b36)
        (a35 <|*|> b35) (a34 <|*|> b34) (a33 <|*|> b33) (a32 <|*|> b32)
        (a31 <|*|> b31) (a30 <|*|> b30) (a29 <|*|> b29) (a28 <|*|> b28)
        (a27 <|*|> b27) (a26 <|*|> b26) (a25 <|*|> b25) (a24 <|*|> b24)
        (a23 <|*|> b23) (a22 <|*|> b22) (a21 <|*|> b21) (a20 <|*|> b20)
        (a19 <|*|> b19) (a18 <|*|> b18) (a17 <|*|> b17) (a16 <|*|> b16)
        (a15 <|*|> b15) (a14 <|*|> b14) (a13 <|*|> b13) (a12 <|*|> b12)
        (a11 <|*|> b11) (a10 <|*|> b10) (a09 <|*|> b09) (a08 <|*|> b08)
        (a07 <|*|> b07) (a06 <|*|> b06) (a05 <|*|> b05) (a04 <|*|> b04)
        (a03 <|*|> b03) (a02 <|*|> b02) (a01 <|*|> b01) (a00 <|*|> b00)


  bnot (W64 b63 b62 b61 b60
                  b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
                  b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
                  b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
                  b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
                  b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
                  b09 b08 b07 b06 b05 b04 b03 b02 b01 b00) =
             (W64 (fBit b63) (fBit b62) (fBit b61) (fBit b60)
                  (fBit b59) (fBit b58) (fBit b57) (fBit b56)
                  (fBit b55) (fBit b54) (fBit b53) (fBit b52)
                  (fBit b51) (fBit b50) (fBit b49) (fBit b48)
                  (fBit b47) (fBit b46) (fBit b45) (fBit b44)
                  (fBit b43) (fBit b42) (fBit b41) (fBit b40)
                  (fBit b39) (fBit b38) (fBit b37) (fBit b36)
                  (fBit b35) (fBit b34) (fBit b33) (fBit b32)
                  (fBit b31) (fBit b30) (fBit b29) (fBit b28)
                  (fBit b27) (fBit b26) (fBit b25) (fBit b24)
                  (fBit b23) (fBit b22) (fBit b21) (fBit b20)
                  (fBit b19) (fBit b18) (fBit b17) (fBit b16)
                  (fBit b15) (fBit b14) (fBit b13) (fBit b12)
                  (fBit b11) (fBit b10) (fBit b09) (fBit b08)
                  (fBit b07) (fBit b06) (fBit b05) (fBit b04)
                  (fBit b03) (fBit b02) (fBit b01) (fBit b00))
  shiftl ( 
          W64 a63 a62 a61 a60
              a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
              a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
              a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
              a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
              a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
              a09 a08 a07 a06 a05 a04 a03 a02 a01 a00
         ,
          q
         )
    = (
        a63
      ,
            W64 a62 a61 a60
                a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
                a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
                a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
                a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
                a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
                a09 a08 a07 a06 a05 a04 a03 a02 a01 a00 q
      )

  shiftr (
          q
         ,
          W64 a63 a62 a61 a60
              a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
              a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
              a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
              a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
              a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
              a09 a08 a07 a06 a05 a04 a03 a02 a01 a00
         )
    = ( W64 q a63 a62 a61 a60
            a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
            a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
            a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
            a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
            a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
            a09 a08 a07 a06 a05 a04 a03 a02 a01 
      ,
        a00 )
  rshift (a, q) = (a' , q' , lsb)
     where
       (a' , la)  = shiftr (signbit a , a)
       (q' , lsb) = shiftr (la , q)

  -- rshift (W64 a63 a62 a61 a60
  --             a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
  --             a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
  --             a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
  --             a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
  --             a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
  --             a09 a08 a07 a06 a05 a04 a03 a02 a01 a00 ,
  --         W64 b63 b62 b61 b60
  --             b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
  --             b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
  --             b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
  --             b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
  --             b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
  --             b09 b08 b07 b06 b05 b04 b03 b02 b01 b00) =
  --        (W64 a63 a63 a62 a61 a60
  --             a59 a58 a57 a56 a55 a54 a53 a52 a51 a50
  --             a49 a48 a47 a46 a45 a44 a43 a42 a41 a40
  --             a39 a38 a37 a36 a35 a34 a33 a32 a31 a30
  --             a29 a28 a27 a26 a25 a24 a23 a22 a21 a20
  --             a19 a18 a17 a16 a15 a14 a13 a12 a11 a10
  --             a09 a08 a07 a06 a05 a04 a03 a02 a01 
  --        ,
  --         W64 a00 b63 b62 b61 b60
  --             b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
  --             b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
  --             b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
  --             b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
  --             b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
  --             b09 b08 b07 b06 b05 b04 b03 b02 b01
  --        ,
  --          b00)

  zero = W64 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C
             C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C
  one  = W64 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C
             C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S

  booth (w1,w2) = booth64 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth64         = proj .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround .
                               boothround . boothround . boothround . boothround 

  signbit  (W64 b63 _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                _ _ _ _ _ _ _ _
                
           ) = b63

  lit = toW64 -- fromInteger

instance ShowHex W64 where
  xshow (W64 b63 b62 b61 
             b60 b59 b58 b57 b56 b55 b54 b53 b52 b51 
             b50 b49 b48 b47 b46 b45 b44 b43 b42 b41 
             b40 b39 b38 b37 b36 b35 b34 b33 b32 b31 
             b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
             b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
             b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = hexify bits
      where
        bits = [b63,b62,b61,
                b60,b59,b58,b57,b56,b55,b54,b53,b52,b51,
                b50,b49,b48,b47,b46,b45,b44,b43,b42,b41,
                b40,b39,b38,b37,b36,b35,b34,b33,b32,b31,
                b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
                b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
                b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]

instance ShowBin W64 where
  bshow (W64 b63 b62 b61 
             b60 b59 b58 b57 b56 b55 b54 b53 b52 b51 
             b50 b49 b48 b47 b46 b45 b44 b43 b42 b41 
             b40 b39 b38 b37 b36 b35 b34 b33 b32 b31 
             b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
             b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
             b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = foldr (++) "" (map show bits)
      where
        bits = [b63,b62,b61,
                b60,b59,b58,b57,b56,b55,b54,b53,b52,b51,
                b50,b49,b48,b47,b46,b45,b44,b43,b42,b41,
                b40,b39,b38,b37,b36,b35,b34,b33,b32,b31,
                b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
                b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
                b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]

instance ToBits W64 where
  tobits (W64 b63 b62 b61 
             b60 b59 b58 b57 b56 b55 b54 b53 b52 b51 
             b50 b49 b48 b47 b46 b45 b44 b43 b42 b41 
             b40 b39 b38 b37 b36 b35 b34 b33 b32 b31 
             b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
             b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
             b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = bits
      where
        bits = [b63,b62,b61,
                b60,b59,b58,b57,b56,b55,b54,b53,b52,b51,
                b50,b49,b48,b47,b46,b45,b44,b43,b42,b41,
                b40,b39,b38,b37,b36,b35,b34,b33,b32,b31,
                b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
                b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
                b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]
    

toW64 :: Integer -> W64
toW64 i = (W64
              b63 b62 b61 b60
              b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
              b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
              b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
              b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
              b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
              b9  b8  b7  b6  b5  b4  b3  b2  b1  b0)
      where
        [ b63 , b62 , b61 , b60 ,
          b59 , b58 , b57 , b56 , b55 , b54 , b53 , b52 , b51 , b50 ,
          b49 , b48 , b47 , b46 , b45 , b44 , b43 , b42 , b41 , b40 ,
          b39 , b38 , b37 , b36 , b35 , b34 , b33 , b32 , b31 , b30 ,
          b29 , b28 , b27 , b26 , b25 , b24 , b23 , b22 , b21 , b20 ,
          b19 , b18 , b17 , b16 , b15 , b14 , b13 , b12 , b11 , b10 ,
          b9  , b8  , b7  , b6  , b5  , b4  , b3  , b2  , b1  , b0 ]
              = pad_or_trunc 64 i


-- Hideous
fromW64 :: W64 -> Integer
fromW64 (W64
              b63 b62 b61 b60
              b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
              b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
              b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
              b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
              b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
              b9  b8  b7  b6  b5  b4  b3  b2  b1  b0)
                  =  (b2i b63 * 2 <^> 63) <+> (b2i b62 * 2 <^> 62) <+> 
                     (b2i b61 * 2 <^> 61) <+> (b2i b60 * 2 <^> 60) <+>
 ---------
                     (b2i b59 * 2 <^> 59) <+> (b2i b58 * 2 <^> 58) <+>
                     (b2i b57 * 2 <^> 57) <+> (b2i b56 * 2 <^> 56) <+>
                     (b2i b55 * 2 <^> 55) <+> (b2i b54 * 2 <^> 54) <+>
                     (b2i b53 * 2 <^> 53) <+> (b2i b52 * 2 <^> 52) <+>
                     (b2i b51 * 2 <^> 51) <+> (b2i b50 * 2 <^> 50) <+>
 ---------
                     (b2i b49 * 2 <^> 49) <+> (b2i b48 * 2 <^> 48) <+>
                     (b2i b47 * 2 <^> 47) <+> (b2i b46 * 2 <^> 46) <+>
                     (b2i b45 * 2 <^> 45) <+> (b2i b44 * 2 <^> 44) <+>
                     (b2i b43 * 2 <^> 43) <+> (b2i b42 * 2 <^> 42) <+>
                     (b2i b41 * 2 <^> 41) <+> (b2i b40 * 2 <^> 40) <+>
 ---------
                     (b2i b39 * 2 <^> 39) <+> (b2i b38 * 2 <^> 38) <+>
                     (b2i b37 * 2 <^> 37) <+> (b2i b36 * 2 <^> 36) <+>
                     (b2i b35 * 2 <^> 35) <+> (b2i b34 * 2 <^> 34) <+>
                     (b2i b33 * 2 <^> 33) <+> (b2i b32 * 2 <^> 32) <+>
                     (b2i b31 * 2 <^> 31) <+> (b2i b30 * 2 <^> 30) <+>
 ---------
                     (b2i b29 * 2 <^> 29) <+> (b2i b28 * 2 <^> 28) <+>
                     (b2i b27 * 2 <^> 27) <+> (b2i b26 * 2 <^> 26) <+>
                     (b2i b25 * 2 <^> 25) <+> (b2i b24 * 2 <^> 24) <+>
                     (b2i b23 * 2 <^> 23) <+> (b2i b22 * 2 <^> 22) <+>
                     (b2i b21 * 2 <^> 21) <+> (b2i b20 * 2 <^> 20) <+>
 ---------
                     (b2i b19 * 2 <^> 19) <+> (b2i b18 * 2 <^> 18) <+>
                     (b2i b17 * 2 <^> 17) <+> (b2i b16 * 2 <^> 16) <+>
                     (b2i b15 * 2 <^> 15) <+> (b2i b14 * 2 <^> 14) <+>
                     (b2i b13 * 2 <^> 13) <+> (b2i b12 * 2 <^> 12) <+>
                     (b2i b11 * 2 <^> 11) <+> (b2i b10 * 2 <^> 10) <+>
 ---------
                       (b2i b9 * 2 <^> 9) <+>   (b2i b8 * 2 <^> 8) <+>
                       (b2i b7 * 2 <^> 7) <+>   (b2i b6 * 2 <^> 6) <+>
                       (b2i b5 * 2 <^> 5) <+>   (b2i b4 * 2 <^> 4) <+>
                       (b2i b3 * 2 <^> 3) <+>   (b2i b2 * 2 <^> 2) <+>
                       (b2i b1 * 2 <^> 1) <+>   (b2i b0 * 2 <^> 0)
  where
    (<+>) , (<^>) :: Integer -> Integer -> Integer
    i <+> j = i P.+ j
    i <^> j = i P.^ j

b2i :: Bit -> Integer
b2i S = 1
b2i C = 0

rotateR :: W64 -> W64
rotateR (W64 b63 b62 b61 b60
             b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
             b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
             b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
             b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
             b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
             b9  b8  b7  b6  b5  b4  b3  b2  b1  b0)
              = (W64 b0 b63 b62 b61 b60
                     b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
                     b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
                     b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
                     b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
                     b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
                     b9  b8  b7  b6  b5  b4  b3  b2  b1)

rotateL :: W64 -> W64
rotateL (W64 b63 b62 b61 b60
             b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
             b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
             b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
             b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
             b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
             b9  b8  b7  b6  b5  b4  b3  b2  b1  b0)
              = (W64 b62 b61 b60
                     b59 b58 b57 b56 b55 b54 b53 b52 b51 b50
                     b49 b48 b47 b46 b45 b44 b43 b42 b41 b40
                     b39 b38 b37 b36 b35 b34 b33 b32 b31 b30
                     b29 b28 b27 b26 b25 b24 b23 b22 b21 b20
                     b19 b18 b17 b16 b15 b14 b13 b12 b11 b10
                     b9  b8  b7  b6  b5  b4  b3  b2  b1  b0  b63)

rotR :: W64 -> Int -> W64
rotR w64 n | n <= 0    = w64
           | otherwise = rotR (rotateR w64) (n-1)

rotL :: W64 -> Int -> W64
rotL w64 n | n <= 0    = w64
           | otherwise = rotL (rotateL w64) (n-1)

conv64 :: W64 -> [Bool] -> W64
conv64 w []        = w
conv64 w (b:bs) = conv64 w' bs
  where
    (_ , w') = shiftl (w , bool2bit b)
    bool2bit :: Bool -> Bit
    bool2bit True  = S
    bool2bit False = C
