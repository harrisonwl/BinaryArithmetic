module W8 where

-- import Prelude hiding ((+))
import BinaryArithmetic

data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving Eq 

-- instance Show W8 where
--   show (W8 b7 b6 b5 b4 b3 b2 b1 b0) = "0b" ++ show b7 ++ show b6 ++ show b5 ++ show b4 ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance ShowHex W8 where
  xshow (W8 b7 b6 b5 b4 b3 b2 b1 b0) = hexify bits
           where
             bits = [ b7 , b6 , b5 , b4 , b3 , b2 , b1 , b0 ]

instance Show W8 where
  show w = '0' : 'x' : xshow w

instance Num W8 where
  (+)         = (BinaryArithmetic.+)
  (*)         = (BinaryArithmetic.<*>)
  abs w8      = case signbit w8 of
    C -> w8
    S -> BinaryArithmetic.negate w8
  signum w8   = case signbit w8 of
    C -> one
    S -> BinaryArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bnot i') BinaryArithmetic.+ one
    where
      [b6,b5,b4,b3,b2,b1,b0] = pad_or_trunc 7 i
      i'                     = W8 C b6 b5 b4 b3 b2 b1 b0
  negate      = BinaryArithmetic.negate

instance BinaryArith W8 where
  a8 + b8 = fst $ carryadd a8 b8 C
  carryadd (W8 a7 a6 a5 a4 a3 a2 a1 a0) (W8 b7 b6 b5 b4 b3 b2 b1 b0) c = (W8 c7 c6 c5 c4 c3 c2 c1 c0 , c')
    where
      (carry0,c0) = bitplus a0 b0 c
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (carry3,c3) = bitplus a3 b3 carry2
      (carry4,c4) = bitplus a4 b4 carry3
      (carry5,c5) = bitplus a5 b5 carry4
      (carry6,c6) = bitplus a6 b6 carry5
      (c',c7)     = bitplus a7 b7 carry6
  leastbit (W8 _ _ _ _ _ _ _ b)  = b
  bnot (W8 a7 a6 a5 a4 a3 a2 a1 a0)
        = W8 (fBit a7) (fBit a6) (fBit a5) (fBit a4)
             (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  shiftr (q , W8 a7 a6 a5 a4 a3 a2 a1 a0) = (W8 q a7 a6 a5 a4 a3 a2 a1 , a0)
  rshift (a, q) = (a' , q' , lsb)
     where
       (a' , la)  = shiftr (signbit a , a)
       (q' , lsb) = shiftr (la , q)

--   rshift (W8 a7 a6 a5 a4 a3 a2 a1 a0, W8 q7 q6 q5 q4 q3 q2 q1 q0)
--         = (W8 a7 a7 a6 a5 a4 a3 a2 a1, W8 a0 q7 q6 q5 q4 q3 q2 q1, q0)
  zero                        = W8 C C C C C C C C
  one                         = W8 C C C C C C C S
  booth (w1,w2) = booth8 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth8         = proj .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround . boothround
  signbit (W8 b7 _ _ _  _ _ _ _) = b7
  (||) (W8 a07 a06 a05 a04 a03 a02 a01 a00)
       (W8 b07 b06 b05 b04 b03 b02 b01 b00)
        = W8 (a07 <||> b07) (a06 <||> b06) (a05 <||> b05) (a04 <||> b04)
             (a03 <||> b03) (a02 <||> b02) (a01 <||> b01) (a00 <||> b00)
  (^) (W8 a07 a06 a05 a04 a03 a02 a01 a00)
       (W8 b07 b06 b05 b04 b03 b02 b01 b00)
        = W8 (a07 <||> b07) (a06 <||> b06) (a05 <||> b05) (a04 <||> b04)
             (a03 <||> b03) (a02 <||> b02) (a01 <||> b01) (a00 <||> b00)
  (.&.) (W8 a07 a06 a05 a04 a03 a02 a01 a00)
       (W8 b07 b06 b05 b04 b03 b02 b01 b00)
        = W8 (a07 <&&> b07) (a06 <&&> b06) (a05 <&&> b05) (a04 <&&> b04)
             (a03 <&&> b03) (a02 <&&> b02) (a01 <&&> b01) (a00 <&&> b00)
  shiftl ( 
              W8 a7  a6  a5  a4  a3  a2  a1  a0
         ,
          q
         )
    = (
        a7
      ,
            W8 a6 a5 a4 a3 a2 a1 a0 q
      )
  lit = toW8 -- fromInteger


toW8 :: Integer -> W8
toW8 i = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
      where
        [b7, b6, b5, b4, b3, b2, b1, b0] = pad_or_trunc 8 i
