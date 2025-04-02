module W32 where

-- import Prelude hiding ((^))
import BinaryArithmetic

data W32 = W32 !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit
               !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit !Bit
                  deriving Eq

instance Show W32 where
  show = xshow

instance Num W32 where
  (+)         = (BinaryArithmetic.+)
  (*)         = (BinaryArithmetic.<*>)
  abs w32      = case signbit w32 of
    C -> w32
    S -> BinaryArithmetic.negate w32
  signum w32   = case signbit w32 of
    C -> one
    S -> BinaryArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bnot i') BinaryArithmetic.+ one
    where
      [b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
       b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
       b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0] = pad_or_trunc 31 i
      i'                     = W32 C b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0
  negate      = BinaryArithmetic.negate

instance BinaryArith W32 where
  a32 + b32 = fst (carryadd a32 b32 C)
  carryadd (W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16 a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 a0) (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) c = (W32 c31 c30 c29 c28 c27 c26 c25 c24 c23 c22 c21 c20 c19 c18 c17 c16 c15 c14 c13 c12 c11 c10 c9 c8 c7 c6 c5 c4 c3 c2 c1 c0 , c')
    where
      (carry0,c0) = bitplus a0 b0 c
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (carry3,c3) = bitplus a3 b3 carry2
      (carry4,c4) = bitplus a4 b4 carry3
      (carry5,c5) = bitplus a5 b5 carry4
      (carry6,c6) = bitplus a6 b6 carry5
      (carry7,c7) = bitplus a7 b7 carry6
      (carry8,c8) = bitplus a8 b8 carry7
      (carry9,c9) = bitplus a9 b9 carry8

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
      (c',c31)      = bitplus a31 b31 carry30

  leastbit (W32 b31 b30 b29 b28
                b27 b26 b25 b24
                b23 b22 b21 b20
                b19 b18 b17 b16
                b15 b14 b13 b12
                b11 b10 b9 b8
                b7 b6 b5 b4
                b3 b2 b1 b0) = b0

  bnot (W32 b31 b30 b29 b28
                b27 b26 b25 b24
                b23 b22 b21 b20
                b19 b18 b17 b16
                b15 b14 b13 b12
                b11 b10 b9 b8
                b7 b6 b5 b4
                b3 b2 b1 b0) = (W32 (fBit b31) (fBit b30) (fBit b29) (fBit b28)
                                    (fBit b27) (fBit b26) (fBit b25) (fBit b24)
                                    (fBit b23) (fBit b22) (fBit b21) (fBit b20)
                                    (fBit b19) (fBit b18) (fBit b17) (fBit b16)
                                    (fBit b15) (fBit b14) (fBit b13) (fBit b12)
                                    (fBit b11) (fBit b10) (fBit b9) (fBit b8)
                                    (fBit b7) (fBit b6) (fBit b5) (fBit b4)
                                    (fBit b3) (fBit b2) (fBit b1) (fBit b0))
  shiftr (q , W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
                  a15 a14 a13 a12 a11 a10 a9  a8  a7  a6  a5  a4  a3  a2  a1  a0)
    = (
       W32 q   a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17
           a16 a15 a14 a13 a12 a11 a10 a9  a8  a7  a6  a5  a4  a3  a2  a1 
      ,
       a0
      )
  rshift (a, q) = (a' , q' , lsb)
     where
       (a' , la)  = shiftr (signbit a , a)
       (q' , lsb) = shiftr (la , q)
       
  shiftl ( 
              W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
                  a15 a14 a13 a12 a11 a10 a9  a8  a7  a6  a5  a4  a3  a2  a1  a0
         ,
          q
         )
    = (
        a31
      ,
            W32 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
                a15 a14 a13 a12 a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 a0 q
      )

  zero = W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C
  one  = W32 C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C S
  booth (w1,w2) = booth32 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth32         = proj .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround
  signbit (W32 b31 b30 b29 b28
                b27 b26 b25 b24
                b23 b22 b21 b20
                b19 b18 b17 b16
                b15 b14 b13 b12
                b11 b10 b9 b8
                b7 b6 b5 b4
                b3 b2 b1 b0) = b31
  -- | Bitwise Inclusive Or
  (||) (W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
            a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01 a00)
       (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16
            b15 b14 b13 b12 b11 b10 b09 b08 b07 b06 b05 b04 b03 b02 b01 b00)
        = W32 (a31 <||> b31) (a30 <||> b30) (a29 <||> b29) (a28 <||> b28)
              (a27 <||> b27) (a26 <||> b26) (a25 <||> b25) (a24 <||> b24)
              (a23 <||> b23) (a22 <||> b22) (a21 <||> b21) (a20 <||> b20)
              (a19 <||> b19) (a18 <||> b18) (a17 <||> b17) (a16 <||> b16)
              (a15 <||> b15) (a14 <||> b14) (a13 <||> b13) (a12 <||> b12)
              (a11 <||> b11) (a10 <||> b10) (a09 <||> b09) (a08 <||> b08)
              (a07 <||> b07) (a06 <||> b06) (a05 <||> b05) (a04 <||> b04)
              (a03 <||> b03) (a02 <||> b02) (a01 <||> b01) (a00 <||> b00)
  -- | Bitwise Exclusive Or.
  (^) (W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
            a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01 a00)
       (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16
            b15 b14 b13 b12 b11 b10 b09 b08 b07 b06 b05 b04 b03 b02 b01 b00)
        = W32 (a31 <|*|> b31) (a30 <|*|> b30) (a29 <|*|> b29) (a28 <|*|> b28)
              (a27 <|*|> b27) (a26 <|*|> b26) (a25 <|*|> b25) (a24 <|*|> b24)
              (a23 <|*|> b23) (a22 <|*|> b22) (a21 <|*|> b21) (a20 <|*|> b20)
              (a19 <|*|> b19) (a18 <|*|> b18) (a17 <|*|> b17) (a16 <|*|> b16)
              (a15 <|*|> b15) (a14 <|*|> b14) (a13 <|*|> b13) (a12 <|*|> b12)
              (a11 <|*|> b11) (a10 <|*|> b10) (a09 <|*|> b09) (a08 <|*|> b08)
              (a07 <|*|> b07) (a06 <|*|> b06) (a05 <|*|> b05) (a04 <|*|> b04)
              (a03 <|*|> b03) (a02 <|*|> b02) (a01 <|*|> b01) (a00 <|*|> b00)
  (.&.) (W32 a31 a30 a29 a28 a27 a26 a25 a24 a23 a22 a21 a20 a19 a18 a17 a16
              a15 a14 a13 a12 a11 a10 a09 a08 a07 a06 a05 a04 a03 a02 a01 a00)
       (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16
            b15 b14 b13 b12 b11 b10 b09 b08 b07 b06 b05 b04 b03 b02 b01 b00)
        = W32 (a31 <&&> b31) (a30 <&&> b30) (a29 <&&> b29) (a28 <&&> b28)
              (a27 <&&> b27) (a26 <&&> b26) (a25 <&&> b25) (a24 <&&> b24)
              (a23 <&&> b23) (a22 <&&> b22) (a21 <&&> b21) (a20 <&&> b20)
              (a19 <&&> b19) (a18 <&&> b18) (a17 <&&> b17) (a16 <&&> b16)
              (a15 <&&> b15) (a14 <&&> b14) (a13 <&&> b13) (a12 <&&> b12)
              (a11 <&&> b11) (a10 <&&> b10) (a09 <&&> b09) (a08 <&&> b08)
              (a07 <&&> b07) (a06 <&&> b06) (a05 <&&> b05) (a04 <&&> b04)
              (a03 <&&> b03) (a02 <&&> b02) (a01 <&&> b01) (a00 <&&> b00)
  lit = toW32 -- fromInteger
    
instance ShowBin W32 where
  bshow (W32 b31 
             b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
             b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
             b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = foldr (++) "" (map show bits)
      where
        bits = [b31,
                b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
                b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
                b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]


rotateR :: W32 -> W32
rotateR (W32 b31 b30 b29 b28 b27 b26 b25 b24
             b23 b22 b21 b20 b19 b18 b17 b16
             b15 b14 b13 b12 b11 b10 b9  b8
             b7  b6  b5  b4  b3  b2  b1  b0) = 
                (W32 b0 b31 b30 b29 b28 b27 b26 b25 b24
                     b23 b22 b21 b20 b19 b18 b17 b16
                     b15 b14 b13 b12 b11 b10 b9  b8
                     b7  b6  b5  b4  b3  b2  b1)

rotateL :: W32 -> W32
rotateL (W32 b31 b30 b29 b28 b27 b26 b25 b24
             b23 b22 b21 b20 b19 b18 b17 b16
             b15 b14 b13 b12 b11 b10 b9  b8
             b7  b6  b5  b4  b3  b2  b1  b0) = 
                (W32 b30 b29 b28 b27 b26 b25 b24
                     b23 b22 b21 b20 b19 b18 b17 b16
                     b15 b14 b13 b12 b11 b10 b9  b8
                     b7  b6  b5  b4  b3  b2  b1 b0 b31)

rotR :: W32 -> Int -> W32
rotR w32 n | n <= 0    = w32
           | otherwise = rotR (rotateR w32) (n-1)

rotL :: W32 -> Int -> W32
rotL w32 n | n <= 0    = w32
           | otherwise = rotL (rotateL w32) (n-1)

instance ShowHex W32 where
  xshow (W32 b31 b30 b29 b28
                b27 b26 b25 b24
                b23 b22 b21 b20
                b19 b18 b17 b16
                b15 b14 b13 b12
                b11 b10 b9 b8
                b7 b6 b5 b4
                b3 b2 b1 b0) = '0' : 'x' : hexify bits
           where
             bits = [ b31 , b30 , b29 , b28
                      , b27 , b26 , b25 , b24
                      , b23 , b22 , b21 , b20
                      , b19 , b18 , b17 , b16
                      , b15 , b14 , b13 , b12
                      , b11 , b10 , b9 , b8
                      , b7 , b6 , b5 , b4
                      , b3 , b2 , b1 , b0 ]


instance ToBits W32 where
  tobits (W32 b31 
              b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
              b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
              b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0) = bits
      where
        bits = [b31,
                b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,
                b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,
                b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]

{-
x0 :: W32
x0 = fromInteger 99

e0 :: W32
e0 = fromInteger $ 1 + 2 + 4 + 8 + 16 + 32 + 64 + 128 + 256 + 512 + 1024
sanitye0 = [e0 >>. 0 , e0 >>. 1 ,e0 >>. 2 , e0 >>. 3 , e0 >>. 4 ,e0 >>. 5]
xsanitye0 = map xshow sanitye0
-}

toW32 :: Integer -> W32
toW32 i = (W32 b31 
              b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 
              b20 b19 b18 b17 b16 b15 b14 b13 b12 b11 
              b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
      where
        [b31,b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,b20,b19,b18,b17,
         b16,b15,b14,b13,b12,b11,b10, b9, b8, b7, b6, b5, b4, b3, b2, b1, b0] = pad_or_trunc 32 i

-- Hideous
fromW32 :: W32 -> Integer
fromW32 (W32 b31 b30 b29 b28 b27 b26 b25 b24 b23 b22 b21 b20 b19 b18 b17 b16
             b15 b14 b13 b12 b11 b10 b9 b8 b7 b6 b5 b4 b3 b2 b1 b0)
                  = (b2i b31 * 2 Prelude.^ 31) Prelude.+ (b2i b30 * 2 Prelude.^ 30)  Prelude.+ (b2i b29 * 2 Prelude.^ 29) Prelude.+ (b2i b28 * 2 Prelude.^ 28) Prelude.+
                    (b2i b27 * 2 Prelude.^ 27) Prelude.+ (b2i b26 * 2 Prelude.^ 26)  Prelude.+ (b2i b25 * 2 Prelude.^ 25) Prelude.+ (b2i b24 * 2 Prelude.^ 24) Prelude.+
                    (b2i b23 * 2 Prelude.^ 23) Prelude.+ (b2i b22 * 2 Prelude.^ 22)  Prelude.+ (b2i b21 * 2 Prelude.^ 21) Prelude.+ (b2i b20 * 2 Prelude.^ 20) Prelude.+
                    (b2i b19 * 2 Prelude.^ 19) Prelude.+ (b2i b18 * 2 Prelude.^ 18)  Prelude.+ (b2i b17 * 2 Prelude.^ 17) Prelude.+ (b2i b16 * 2 Prelude.^ 16) Prelude.+
                    (b2i b15 * 2 Prelude.^ 15) Prelude.+ (b2i b14 * 2 Prelude.^ 14)  Prelude.+ (b2i b13 * 2 Prelude.^ 13) Prelude.+ (b2i b12 * 2 Prelude.^ 12) Prelude.+
                    (b2i b11 * 2 Prelude.^ 11) Prelude.+ (b2i b10 * 2 Prelude.^ 10)  Prelude.+ (b2i b9 * 2 Prelude.^ 9) Prelude.+ (b2i b8 * 2 Prelude.^ 8) Prelude.+
                    (b2i b7 * 2 Prelude.^ 7) Prelude.+ (b2i b6 * 2 Prelude.^ 6)  Prelude.+ (b2i b5 * 2 Prelude.^ 5) Prelude.+ (b2i b4 * 2 Prelude.^ 4) Prelude.+
                    (b2i b3 * 2 Prelude.^ 3) Prelude.+ (b2i b2 * 2 Prelude.^ 2)  Prelude.+ (b2i b1 * 2 Prelude.^ 1) Prelude.+ (b2i b0 * 2 Prelude.^ 0)

b2i :: Bit -> Integer
b2i S = 1
b2i C = 0


type W32x4 = (W32, W32, W32, W32)

toW32x4 :: (Integer, Integer, Integer, Integer) -> W32x4
toW32x4 (a, b, c, d) = (toW32 a, toW32 b, toW32 c, toW32 d)

type W32x16 = (W32, W32, W32, W32, W32, W32, W32, W32, W32, W32, W32, W32, W32, W32, W32, W32)

toW32x16 :: (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer,
             Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
             -> W32x16
toW32x16 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  = (toW32 a, toW32 b, toW32 c, toW32 d, toW32 e, toW32 f, toW32 g, toW32 h,
     toW32 i, toW32 j, toW32 k, toW32 l, toW32 m, toW32 n, toW32 o, toW32 p)

fromW32x16 :: W32x16 -> (Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer,
                         Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer)
fromW32x16 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
  = (fromW32 a, fromW32 b, fromW32 c, fromW32 d, fromW32 e, fromW32 f, fromW32 g, fromW32 h,
     fromW32 i, fromW32 j, fromW32 k, fromW32 l, fromW32 m, fromW32 n, fromW32 o, fromW32 p)
  
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
          Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p) =>
         Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
    "( " ++ show a  ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ", \n" ++
    "  " ++ show e  ++ ", " ++ show f ++ ", " ++ show g ++ ", " ++ show h ++ ", \n" ++
    "  " ++ show i  ++ ", " ++ show j ++ ", " ++ show k ++ ", " ++ show l ++ ", \n" ++
    "  " ++ show m  ++ ", " ++ show n ++ ", " ++ show o ++ ", " ++ show p ++ ")\n"

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h,
          Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p) =>
         Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
  (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) == (a', b', c', d', e', f', g', h', i', j', k', l', m', n', o', p')
       = a==a' Prelude.&& b==b' Prelude.&& c==c' Prelude.&& d==d' Prelude.&& e==e' Prelude.&& f==f' Prelude.&& g==g' Prelude.&& h==h'
               Prelude.&& i==i' Prelude.&& j==j' Prelude.&& k==k' Prelude.&& l==l' Prelude.&& m==m' Prelude.&& n==n' Prelude.&& o==o' Prelude.&& p==p'

myand :: W32 -> W32 -> W32
myand = (.&.)
