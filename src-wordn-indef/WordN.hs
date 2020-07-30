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
  , maskWordN
  , wordNMask
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
  (WordN l) + (WordN r) = maskWordN $ (l + r)
  (WordN l) * (WordN r) = maskWordN $ (l * r)
  (WordN l) - (WordN r) = maskWordN $ (l - r)
  negate (WordN x)   = maskWordN $ negate x
  abs w = w
  signum (WordN x) | x == 0    = 0
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

  enumFrom          = integralEnumFrom
  enumFromThen      = integralEnumFromThen
  enumFromTo        = integralEnumFromTo
  enumFromThenTo    = integralEnumFromThenTo

instance forall n. (KnownNat n, n <= WordSize.MaxBits) => Bounded (WordN n) where
  minBound = 0
  maxBound = wordNMask

{-# INLINE wordNMask #-}
-- | An (WordN n) with all the bits set, used for masking.
wordNMask :: forall n. (KnownNat n) => WordN n
wordNMask = WordN . (flip (-) 1) . bit $ fromIntegral bits
  where 
  bits = natVal (Proxy :: Proxy n)

{-# INLINE maskWordN #-}
-- | Smart constructor for WrapWords which masks off the unused upper bits.
maskWordN :: forall n. (KnownNat n) => WordSize.T -> WordN n
maskWordN w = WordN $ w .&. getWord (wordNMask :: WordN n)
