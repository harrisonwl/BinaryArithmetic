module W6 where

import BinaryArithmetic

data W6 = W6 Bit Bit Bit Bit Bit Bit deriving Eq

-- instance Show W8 where
--   show (W8 b7 b6 b5 b4 b3 b2 b1 b0) = "0b" ++ show b7 ++ show b6 ++ show b5 ++ show b4 ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance ShowHex W6 where
  xshow (W6 b5 b4 b3 b2 b1 b0) = hexify bits
           where
             bits = [ b5 , b4 , b3 , b2 , b1 , b0 ]

instance Show W6 where
  show w = '0' : 'x' : xshow w

instance Num W6 where
  (+)         = (BinaryArithmetic.+)
  (*)         = (BinaryArithmetic.<*>)
  abs w6      = case signbit w6 of
    C -> w6
    S -> BinaryArithmetic.negate w6
  signum w6   = case signbit w6 of
    C -> one
    S -> BinaryArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bnot i') BinaryArithmetic.+ one
    where
      [b4,b3,b2,b1,b0] = pad_or_trunc 5 i
      i'                     = W6 C b4 b3 b2 b1 b0
  negate      = BinaryArithmetic.negate

instance BinaryArith W6 where
  a6 + b6 = fst $ carryadd a6 b6 C
  carryadd (W6 a5 a4 a3 a2 a1 a0) (W6 b5 b4 b3 b2 b1 b0) c = (W6 c5 c4 c3 c2 c1 c0 , c')
    where
      (carry0,c0) = bitplus a0 b0 c
      (carry1,c1) = bitplus a1 b1 carry0
      (carry2,c2) = bitplus a2 b2 carry1
      (carry3,c3) = bitplus a3 b3 carry2
      (carry4,c4) = bitplus a4 b4 carry3
      (c',c5)     = bitplus a5 b5 carry4
  leastbit (W6 _ _ _ _ _ b)  = b
  bnot (W6 a5 a4 a3 a2 a1 a0)
        = W6 (fBit a5) (fBit a4)
             (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  shiftr (q , W6 a5 a4 a3 a2 a1 a0) = (W6 q a5 a4 a3 a2 a1 , a0)
  rshift (a, q) = (a' , q' , lsb)
     where
       (a' , la)  = shiftr (signbit a , a)
       (q' , lsb) = shiftr (la , q)

  zero                        = W6 C C C C C C
  one                         = W6 C C C C C S
  booth (w1,w2) = booth6 (zero,w1,leastbit w1,w2)
      where proj (x,y,_,_) = (x,y)
            booth6         = proj .
                               boothround .
                               boothround .
                               boothround .
                               boothround .
                               boothround . boothround
  signbit (W6 b5 _ _ _ _ _) = b5
  (||) (W6 a05 a04 a03 a02 a01 a00)
       (W6 b05 b04 b03 b02 b01 b00)
        = W6 (a05 <||> b05) (a04 <||> b04)
             (a03 <||> b03) (a02 <||> b02) (a01 <||> b01) (a00 <||> b00)
  (^) (W6 a05 a04 a03 a02 a01 a00)
       (W6 b05 b04 b03 b02 b01 b00)
        = W6 (a05 <||> b05) (a04 <||> b04)
             (a03 <||> b03) (a02 <||> b02) (a01 <||> b01) (a00 <||> b00)
  (.&.) (W6 a05 a04 a03 a02 a01 a00)
       (W6 b05 b04 b03 b02 b01 b00)
        = W6 (a05 <&&> b05) (a04 <&&> b04)
             (a03 <&&> b03) (a02 <&&> b02) (a01 <&&> b01) (a00 <&&> b00)
  shiftl ( 
              W6 a5 a4 a3 a2 a1 a0
         ,
          q
         )
    = (
        a5
      ,
            W6 a4 a3 a2 a1 a0 q
      )
  lit = toW6 -- fromInteger 

toW6 :: Integer -> W6
toW6 i = (W6 b5 b4 b3 b2 b1 b0)
      where
        [b5, b4, b3, b2, b1, b0] = pad_or_trunc 6 i
