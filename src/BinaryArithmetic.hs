module BinaryArithmetic where 

import Prelude hiding ((||) , (&&) , negate)

data Bit = C | S deriving (Eq,Read)
instance Show Bit where
  show C = "0"
  show S = "1"

(>&&<) :: Bit -> Bit -> Bit
S >&&< S = S
_ >&&< _ = C

(>||<) :: Bit -> Bit -> Bit
C >||< C = C
_ >||< _ = S

(>^<) :: Bit -> Bit -> Bit
C >^< S = S
S >^< C = S
_ >^< _ = C

class BinaryArith w where
   (<+>)      :: w -> w -> w          -- definable by carryadd
   carryadd   :: w -> w -> Bit -> (w , Bit)
   leastbit   :: w -> Bit
   -- (&&)      :: w -> w -> w          -- bitwise and
   -- (||)      :: w -> w -> w          -- inclusive or
   -- (|*|)      :: w -> w -> w         -- exclusive or
   (<||>)     :: w -> w -> w  -- Bitwise OR
   (<&&>)     :: w -> w -> w  -- Bitwise AND
   (<^>)      :: w -> w -> w  -- Bitwise XOR
   bitwiseneg :: w -> w
   shiftl     :: (w , Bit) -> (Bit , w) 
   shiftr     :: (Bit, w) -> (w, Bit) 
   rshift     :: (w,w) -> (w,w,Bit)   -- definable by shiftr 
   zero, one  :: w
   booth      :: (w,w) -> (w,w)
   signbit    :: w -> Bit

class BinaryArith w => ToBits w where
  tobits :: w -> [Bit]

showBits :: [Bit] -> String
showBits bs = "0b" ++ foldr (++) "" (map show bs)

(>>>) :: BinaryArith w => w -> Int -> w
w >>> n = compose (nrepeat n rs) w
   where
     rs :: BinaryArith w => w -> w
     rs w = let
              (_ , w1 , _) = rshift (zero , w)
            in
              w1
     compose [] = id
     compose (f : fs) = f . compose fs
     nrepeat 0 x = []
     nrepeat n x = x : nrepeat (n - 1) x

-- watch for fenceposts! 
pad_or_trunc :: Int -> Integer -> [Bit]
pad_or_trunc d i | l==d = revbits
                 | l<d  = pad (d-l) revbits
                 | l>d  = reverse $ take d bits
  where
    bits    = int2bin i
    l       = length bits
    revbits = reverse bits
    pad :: Int -> [Bit] -> [Bit]
    pad n bits | n==0      = bits
               | n>0       = C : pad (n-1) bits
               | otherwise = error "can't happen"
    
-- Produces Bits in little endian form.
int2bin :: Integer -> [Bit]
int2bin i | i==0      = []
          | otherwise = b : int2bin (i `div` 2)
              where b = case i `mod` 2 of
                      0 -> C
                      1 -> S
                      _ -> error "can't happen"

-- left shift b; ignore msb
(<<) :: BinaryArith w => w -> Bit -> w
w << b = snd (shiftl (w , b))

-- flips the Bit.
fBit :: Bit -> Bit
fBit C = S
fBit S = C

bitplus :: Bit -> Bit -> Bit -> (Bit,Bit)
bitplus C C C = (C,C)
bitplus C C S = (C,S)
bitplus C S C = (C,S)
bitplus C S S = (S,C)
bitplus S C C = (C,S)
bitplus S C S = (S,C)
bitplus S S C = (S,C)
bitplus S S S = (S,S)

negate :: BinaryArith w => w -> w
negate w = (bitwiseneg w) <+> one

(<->) :: BinaryArith w => w -> w -> w
w1 <-> w2 = w1 <+> (negate w2)

(<*>) :: BinaryArith w => w -> w -> w
w1 <*> w2 = snd (booth (w1,w2))

boothround :: BinaryArith t => (t, t, Bit, t) -> (t, t, Bit, t)
boothround (a,q,q_1,m) = let
    q_0 = leastbit q in
    case (q_0,q_1) of
      (C,C) -> (a',q',q_1',m)
        where
          (a',q',q_1') = rshift (a,q)
        
      (S,C) -> (a'',q',q_1',m)           -- A - M
        where
          a'            = a <-> m
          (a'',q',q_1') = rshift (a',q)

      (C,S) -> (a'', q',q_1',m)     -- A + M
        where
          a'            = a <+> m
          (a'',q',q_1') = rshift (a',q)

      (S,S) -> (a',q',q_1',m)
        where
          (a',q',q_1')  = rshift (a,q)

fours :: a -> [a] -> [(a,a,a,a)]
fours d []                       = []
fours d (x1 : x2 : x3 : x4 : xs) = (x4 , x3 , x2 , x1) : fours d xs
fours d (x1 : x2 : x3 : [])      = (d , x3 , x2 , x1) : []
fours d (x1 : x2 : [])           = (d , d , x2 , x1) : []
fours d (x1 : [])                = (d , d , d , x1) : []

toHex :: (Bit , Bit , Bit , Bit) -> Char
toHex (C , C , C , C) = '0'
toHex (C , C , C , S) = '1'
toHex (C , C , S , C) = '2'
toHex (C , C , S , S) = '3'
toHex (C , S , C , C) = '4'
toHex (C , S , C , S) = '5'
toHex (C , S , S , C) = '6'
toHex (C , S , S , S) = '7'
toHex (S , C , C , C) = '8'
toHex (S , C , C , S) = '9'
toHex (S , C , S , C) = 'a'
toHex (S , C , S , S) = 'b'
toHex (S , S , C , C) = 'c'
toHex (S , S , C , S) = 'd'
toHex (S , S , S , C) = 'e'
toHex (S , S , S , S) = 'f'

class ShowHex w where
  xshow :: w -> String

class ShowBin w where
  bshow :: w -> String

hexify :: [Bit] -> String
hexify bits = map toHex (reverse (fours C (reverse bits)))

