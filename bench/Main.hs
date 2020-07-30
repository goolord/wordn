{-# LANGUAGE
    BangPatterns
  , GADTs
  , DeriveGeneric
  , StandaloneDeriving
  , MagicHash
  , DataKinds
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

import Gauge.Main (defaultMain, bench, nf)

import Data.Word
import Control.DeepSeq
import Unsafe.Coerce
import Control.Exception
import qualified WordN.Word8 as Word8
import qualified Weigh
import qualified Data.Word.Odd as Odd

deriving instance NFData (Word8.WordN n)

type OddWord8 = Odd.OddWord Word8 (Odd.Lit 8)

instance NFData a => NFData (Odd.OddWord a n) where
  rnf x = rnf $ (unsafeCoerce x :: a)

main :: IO ()
main = do
  defaultMain
    [
    ]
  putStr "Memory usage :"
  Weigh.mainWith $ do
    Weigh.action "Word8" w8
    Weigh.action "WordN 8" wn8
    Weigh.action "OddWord 8 Word8" oddword8

{-# NOINLINE w8 #-}
w8 :: IO Word8
w8 = evaluate maxBound

{-# NOINLINE wn8 #-}
wn8 :: IO (Word8.WordN 8)
wn8 = evaluate maxBound

{-# NOINLINE oddword8 #-}
oddword8 :: IO OddWord8
oddword8 = evaluate maxBound
