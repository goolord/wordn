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

module WordN where

import GHC.TypeNats
import GHC.Types
import GHC.Enum
import GHC.Prim
import GHC.Real
import qualified WordSize

data Proxy (n :: Nat) = Proxy

newtype WordN (n :: Nat) = WordN WordSize.T

deriving instance (n <= WordSize.MaxBits) => Num (WordN n)
deriving instance (n <= WordSize.MaxBits) => Show (WordN n)
deriving instance (n <= WordSize.MaxBits) => Eq (WordN n)
deriving instance (n <= WordSize.MaxBits) => Ord (WordN n)
deriving instance (n <= WordSize.MaxBits) => Real (WordN n)
deriving instance (KnownNat n, n <= WordSize.MaxBits) => Integral (WordN n)
instance (KnownNat n, n <= WordSize.MaxBits) => Enum (WordN n) where
  succ x
    | x /= maxBound = x + 1
    | otherwise     = succError "WordN"
  pred x
    | x /= minBound = x - 1
    | otherwise     = predError "WordN"
  toEnum i@(I# i#)
    | i >= 0        = WordN $ WordSize.liftWord (int2Word# i#)
    | otherwise     = toEnumError "WordN" i (minBound::WordN n, maxBound::WordN n)
  fromEnum x@(WordN x')
    | x <= fromIntegral (maxBound :: Int) = I# (word2Int# (WordSize.unliftWord x'))
    | otherwise     = fromEnumError "WordN" x
  enumFrom            = integralEnumFrom
  enumFromThen        = integralEnumFromThen
  enumFromTo          = integralEnumFromTo
  enumFromThenTo      = integralEnumFromThenTo

instance forall n. (KnownNat n, n <= WordSize.MaxBits) => Bounded (WordN n) where
  minBound = 0
  maxBound = (2 ^ bits) - 1
    where 
    bits = natVal (Proxy :: Proxy n)

