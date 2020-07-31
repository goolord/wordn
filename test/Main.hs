{-# LANGUAGE 
    MagicHash 
  , TemplateHaskell
  , TypeApplications
  , DataKinds
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

module Main where

import Test.Tasty
-- import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes
import Data.Proxy
import GHC.TypeLits
import qualified WordN.Word8 as Word8

instance KnownNat n => Arbitrary (Word8.WordN n) where
  arbitrary = Word8.maskWordN <$> arbitrary

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, typeclassLaws]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [
  ]

typeclassLaws :: TestTree
typeclassLaws = testGroup "Typeclass Laws"
  [ testLaws $ numLaws         (Proxy @(Word8.WordN 8))
  , testLaws $ eqLaws          (Proxy @(Word8.WordN 8))
  , testLaws $ bitsLaws        (Proxy @(Word8.WordN 8))
  , testLaws $ bitsLaws        (Proxy @(Word8.WordN 8))
  , testLaws $ integralLaws    (Proxy @(Word8.WordN 8))
  , testLaws $ ordLaws         (Proxy @(Word8.WordN 8))
  , testLaws $ boundedEnumLaws (Proxy @(Word8.WordN 8))
  ]

unwrap :: Either a b -> b
unwrap = either (error "unwrap") id

testLaws :: Laws -> TestTree
testLaws (Laws tc lp) = testProperties tc lp
