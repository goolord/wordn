{-# LANGUAGE
    MagicHash
  , TypeOperators
  , DataKinds
  , KindSignatures
  , TypeFamilies
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
#-}

module WordN 
  ( WordN(..)
  , NoMask(..)
  , maskWordN
  , wordNMask
  , preserveBits
  , preserveBitsR
  ) where

import Data.Bits
import GHC.TypeNats
import GHC.Types
import GHC.Enum
import GHC.Prim
import GHC.Real
import qualified WordSize

data Proxy (n :: Nat) = Proxy

newtype WordN (n :: Nat) = WordN { getWord :: WordSize.T }

instance (KnownNat n, n <= WordSize.MaxBits) => Num (WordN n) where
  (WordN l) + (WordN r) = maskWordN (l + r)
  (WordN l) * (WordN r) = maskWordN (l * r)
  (WordN l) - (WordN r) = maskWordN (l - r)
  negate (WordN x)   = maskWordN $ negate x
  abs w = w
  signum (WordN x) 
    | x == 0 = 0
    | otherwise = 1
  fromInteger i = maskWordN $ fromInteger i 

instance (n <= WordSize.MaxBits) => Show (WordN n) where
  show (WordN w) = show w

deriving instance (n <= WordSize.MaxBits) => Eq (WordN n)
deriving instance (n <= WordSize.MaxBits) => Ord (WordN n)
deriving instance (KnownNat n, n <= WordSize.MaxBits) => Real (WordN n)

deriving instance (KnownNat n, n <= WordSize.MaxBits) => Integral (WordN n)

instance (KnownNat n, n <= WordSize.MaxBits) => Enum (WordN n) where
  succ x
    | x /= maxBound = x + 1
    | otherwise     = succError "WordN"

  pred x
    | x /= minBound = x - 1
    | otherwise     = predError "WordN"

  toEnum i@(I# i#)
    | i >= 0    = WordN $ WordSize.liftWord (int2Word# i#)
    | otherwise = toEnumError "WordN" i (minBound::WordN n, maxBound::WordN n)

  fromEnum x@(WordN x')
    | x <= fromIntegral (maxBound :: Int) = I# (word2Int# (WordSize.unliftWord x'))
    | otherwise = fromEnumError "WordN" x

  enumFrom       = integralEnumFrom
  enumFromThen   = integralEnumFromThen
  enumFromTo     = integralEnumFromTo
  enumFromThenTo = integralEnumFromThenTo

instance forall n. (KnownNat n, n <= WordSize.MaxBits) => Bounded (WordN n) where
  minBound = 0
  maxBound = wordNMask

instance (KnownNat n, n <= WordSize.MaxBits) => Bits (WordN n) where
    (WordN l) .&. (WordN r) = WordN $ l .&. r
    (WordN l) .|. (WordN r) = WordN $ l .|. r
    xor (WordN l) (WordN r) = WordN $ xor l r
    complement x = x `xor` wordNMask
    bit n | n < (fromIntegral $ natVal (Proxy :: Proxy n))
          = WordN $ bit n
          | otherwise = WordN 0
    setBit (WordN x) n | n < (fromIntegral $ natVal (Proxy :: Proxy n))
                    = WordN $ setBit x n
                    | otherwise = WordN x
    clearBit (WordN x) n = WordN $ clearBit x n
    complementBit (WordN x) n | n < (fromIntegral $ natVal (Proxy :: Proxy n))
                           = WordN $ complementBit x n
                           | otherwise = WordN x
    testBit (WordN x) n = testBit x n
    bitSize _ = fromIntegral $ natVal (Proxy :: Proxy n)
    bitSizeMaybe _ = Just $ fromIntegral $ natVal (Proxy :: Proxy n)
    isSigned _ = False 
    shiftL (WordN x) n = maskWordN $ shiftL x n
    shiftR (WordN x) n = WordN $ shiftR x n
    rotateL (WordN x) n = WordN $
        (shiftL x n' .&. getWord (wordNMask :: WordN n)) .|. shiftR x (w-n')
        where n' = n `mod` w
              w = fromIntegral $ natVal (Proxy :: Proxy n)
    rotateR (WordN x) n = WordN $
        shiftR x n' .|. (shiftL x (w-n') .&. getWord (wordNMask :: WordN n))
        where n' = n `mod` w
              w  = fromIntegral $ natVal (Proxy :: Proxy n)
    popCount (WordN x) = popCount x

instance (KnownNat n, n <= WordSize.MaxBits) => FiniteBits (WordN n) where
    finiteBitSize _ = fromIntegral $ natVal (Proxy :: Proxy n) 
    countLeadingZeros (WordN x) =
        subWordClz (fromIntegral $ natVal (Proxy :: Proxy n)) x
    countTrailingZeros (WordN x) =
        subWordCtz (fromIntegral $ natVal (Proxy :: Proxy n)) x

--------------------------------------------------------------------------

newtype NoMask (n :: Nat) = NoMask { noMask :: WordSize.T }

instance forall n. (KnownNat n , n <= WordSize.MaxBits) => Show (NoMask n) where
  show (NoMask w) = show (maskWordN w :: WordN n)

deriving instance (n <= WordSize.MaxBits) => Eq (NoMask n)
deriving instance (n <= WordSize.MaxBits) => Ord (NoMask n)

instance forall n. (KnownNat n, n <= WordSize.MaxBits) => Bounded (NoMask n) where
  minBound = NoMask 0
  maxBound = NoMask $ getWord (wordNMask :: WordN n)

{-# INLINE wordNMask #-}
-- | An (WordN n) with all the bits set, used for masking.
wordNMask :: forall n. (KnownNat n) => WordN n
wordNMask = WordN . flip (-) 1 . bit $ fromIntegral bits
  where 
  bits = natVal (Proxy :: Proxy n)

{-# INLINE maskWordN #-}
-- | Smart constructor for WrapWords which masks off the unused upper bits.
maskWordN :: forall n. (KnownNat n) => WordSize.T -> WordN n
maskWordN w = WordN $ w .&. getWord (wordNMask :: WordN n)

{-# INLINE preserveBits #-}
preserveBits :: forall n. KnownNat n => (WordSize.T -> WordSize.T) -> NoMask n -> NoMask n
preserveBits f w = preserveBitsR (\_ w2 -> f w2) (NoMask 0) w

{-# INLINE preserveBitsR #-}
preserveBitsR :: forall n. KnownNat n => (WordSize.T -> WordSize.T -> WordSize.T) -> NoMask n -> NoMask n -> NoMask n
preserveBitsR f (NoMask w1) (NoMask w2) = 
  let w1' = getWord (maskWordN w1 :: WordN n)
      w2' = getWord (maskWordN w2 :: WordN n)
      mask = getWord (wordNMask :: WordN n)
      upper = w2 .&. complement mask
   in NoMask $ (f w1' w2') .|. upper

-- | Count the leading zeros on a @w@-bit wide word.
subWordClz :: Int -> WordSize.T -> Int
subWordClz w x = countLeadingZeros x + w - finiteBitSize x

-- | Count the trailing zeros on a @w@-bit wide word.
subWordCtz :: Int -> WordSize.T -> Int
subWordCtz w x = min (countTrailingZeros x) w

