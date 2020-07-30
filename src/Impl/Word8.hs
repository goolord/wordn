{-# LANGUAGE
    MagicHash
  , DataKinds
#-}

module Impl.Word8 where

import Data.Word
import GHC.Exts
import GHC.Word

type T = Word8
type R = 'Word8Rep
type T# = Word8#
type MaxBits = 8

{-# INLINE liftWord #-}
liftWord :: Word# -> Word8
liftWord = W8#

{-# INLINE unliftWord #-}
unliftWord :: Word8 -> Word#
unliftWord (W8# w) = w
