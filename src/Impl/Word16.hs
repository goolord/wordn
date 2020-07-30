{-# LANGUAGE
    MagicHash
  , DataKinds
#-}

module Impl.Word16 where

import Data.Word
import GHC.Exts
import GHC.Word

type T = Word16
type R = 'Word16Rep
type T# = Word16#
type MaxBits = 16

{-# INLINE liftWord #-}
liftWord :: Word# -> Word16
liftWord = W16#

{-# INLINE unliftWord #-}
unliftWord :: Word16 -> Word#
unliftWord (W16# w) = w
