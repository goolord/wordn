{-# LANGUAGE
    MagicHash
  , DataKinds
#-}

module Impl.Word64 where

import Data.Word
import GHC.Exts
import GHC.Word

type T = Word64
type R = 'Word64Rep
type T# = Word64#
type MaxBits = 64

{-# INLINE liftWord #-}
liftWord :: Word# -> Word64
liftWord = W64#

{-# INLINE unliftWord #-}
unliftWord :: Word64 -> Word#
unliftWord (W64# w) = w
