module W8 where

import BinaryArithmetic

data W8 = W8 Bit Bit Bit Bit Bit Bit Bit Bit deriving Eq 

---instance Show W8 where
--  show (W8 b7 b6 b5 b4 b3 b2 b1 b0) = "0b" ++ show b7 ++ show b6 ++ show b5 ++ show b4 ++ show b3 ++ show b2 ++ show b1 ++ show b0

instance ShowHex W8 where
  xshow (W8 b7 b6 b5 b4 b3 b2 b1 b0) = hexify bits
           where
             bits = [ b7 , b6 , b5 , b4 , b3 , b2 , b1 , b0 ]

-- instance Show W8 where
--   show w8 = "x" ++ xshow w8

instance Show W8 where
  show w8 = show (val w8)


val :: W8 -> Int
val (W8 b7 b6 b5 b4 b3 b2 b1 b0) = v b7 * 2^7 + v b6 * 2^6 + v b5 * 2^5 + v b4 * 2^4
                                     + v b3 * 2^3 + v b2 * 2^2 + v b1 * 2^1 + v b0 * 2^0
   where
     v C = 0
     v S = 1


toW8 :: Integer -> W8
toW8 i = (W8 b7 b6 b5 b4 b3 b2 b1 b0)
      where
        [b7, b6, b5, b4, b3, b2, b1, b0] = pad_or_trunc 8 i

-- toBits :: Integer -> String
-- toBits i = sourceW8 (b7, b6, b5, b4, b3, b2, b1, b0)
--       where
--         [b7, b6, b5, b4, b3, b2, b1, b0] = pad_or_trunc 8 i
--         sourceW8 :: (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit) -> String
--         sourceW8 (b7, b6, b5, b4, b3, b2, b1, b0) = "(W8 " ++ show b7 ++ " " ++ show b6 ++ " " ++ show b5 ++ " " ++ show b4 ++ " "
--                                                            ++ show b3 ++ " " ++ show b2 ++ " " ++ show b1 ++ " " ++ show b0 ++ ")"

-- toBits4 (x, y ,w, z) = "tau = (" ++ toBits x ++" , " ++ toBits y ++ " , " ++ toBits w ++ " , " ++ toBits z ++ ")"


-- instance Show W8 where
--   show w = '0' : 'x' : xshow w

instance Num W8 where
  (+)         = (<+>)
  (*)         = (BinaryArithmetic.<*>)
  abs w8      = case signbit w8 of
    C -> w8
    S -> BinaryArithmetic.negate w8
  signum w8   = case signbit w8 of
    C -> one
    S -> BinaryArithmetic.negate one
  fromInteger i | i>=0 = i'
                | i<0  = (bitwiseneg i') <+> one
    where
      [b6,b5,b4,b3,b2,b1,b0] = pad_or_trunc 7 i
      i'                     = W8 C b6 b5 b4 b3 b2 b1 b0
  negate      = BinaryArithmetic.negate

instance BinaryArith W8 where
  a8 <+> b8 = fst $ carryadd a8 b8 C
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
  bitwiseneg (W8 a7 a6 a5 a4 a3 a2 a1 a0)
        = W8 (fBit a7) (fBit a6) (fBit a5) (fBit a4)
             (fBit a3) (fBit a2) (fBit a1) (fBit a0)
  shiftr (q , W8 a7 a6 a5 a4 a3 a2 a1 a0) = (W8 q a7 a6 a5 a4 a3 a2 a1 , a0)
  rshift (a, q) = (a' , q' , lsb)
     where
       (a' , la)  = shiftr (signbit a , a)
       (q' , lsb) = shiftr (la , q)

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

  (<||>)
    (W8 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W8 b07 b06 b05 b04 b03 b02 b01 b00) =
      W8 (a07 >||< b07) (a06 >||< b06) (a05 >||< b05) (a04 >||< b04)
         (a03 >||< b03) (a02 >||< b02) (a01 >||< b01) (a00 >||< b00)

  (<&&>)
    (W8 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W8 b07 b06 b05 b04 b03 b02 b01 b00) =
      W8
        (a07 >&&< b07) (a06 >&&< b06) (a05 >&&< b05) (a04 >&&< b04)
        (a03 >&&< b03) (a02 >&&< b02) (a01 >&&< b01) (a00 >&&< b00)

  (<^>)
    (W8 a07 a06 a05 a04 a03 a02 a01 a00) 
    (W8 b07 b06 b05 b04 b03 b02 b01 b00) =
      W8
        (a07 >^< b07) (a06 >^< b06) (a05 >^< b05) (a04 >^< b04)
        (a03 >^< b03) (a02 >^< b02) (a01 >^< b01) (a00 >^< b00)

  shiftl ( 
          W8 a07 a06 a05 a04 a03 a02 a01 a00
         ,
          q
         )
    = (
        a07
      ,
            W8 a06 a05 a04 a03 a02 a01 a00 q
      )

{-
-- The ugly details behind generating carryadd64
mkvar a i = a ++ show i

mkcarry a b c ci i = "   (" ++ (mkvar c i) ++ "," ++ (mkvar ci (i+1)) ++ ") = carryadd " ++ (mkvar a i) ++ " " ++ (mkvar b i) ++ " " ++ (mkvar ci i)

mkadd a b c i = "    " ++ (mkvar c i) ++ " = " ++ (mkvar a i) ++ " + " ++ (mkvar b i) 

--     (c63 , co63) = carryadd a63 b63 ci63

pattern v = "(X64 " ++ foldr1 (\ v vs -> v ++ " " ++ vs) (map (mkvar v) ix) ++ ")"
  where
    ix = reverse [0..63]

whereclause = grunt ix
  where
    ix = [0..63]
--    rix = reverse ix
    grunt []       = []
--     grunt (i : is) = mkcarry "a" "b" "c" "ci" i ++ "\n" ++ grunt is 
    grunt (i : is) = mkadd "a" "b" "c" i ++ "\n" ++ grunt is 
--    vars a = map (mkvar a) ix

-}

