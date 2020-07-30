{-# LANGUAGE
    MagicHash
  , DataKinds
#-}

module Impl.Word32 where

import Data.Word
import GHC.Exts
import GHC.Word

type T = Word32
type R = 'Word32Rep
type T# = Word32#
type MaxBits = 32

{-# INLINE liftWord #-}
liftWord :: Word# -> Word32
liftWord = W32#

{-# INLINE unliftWord #-}
unliftWord :: Word32 -> Word#
unliftWord (W32# w) = w
