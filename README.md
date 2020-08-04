# wordn
aiming to be like `OddWord` without its Nat code, monomorphised using backpack

## Benchmarks
```
Memory usage at -O0:
Case             Allocated  GCs
Word8                   16    0
WordN 8                136    0
OddWord 8 Word8        592    0
```
I want to benchmark at `-O2`, but I can't figure out how to without the values being optimized away entirely.

## Tests
<details>

<summary> All 51 tests passed (0.02s) </summary>

<p>

```
Tests
  Typeclass Laws
    Num
      Additive Commutativity:                         OK
        +++ OK, passed 100 tests.
      Additive Left Identity:                         OK
        +++ OK, passed 100 tests.
      Additive Right Identity:                        OK
        +++ OK, passed 100 tests.
      Multiplicative Associativity:                   OK
        +++ OK, passed 100 tests.
      Multiplicative Left Identity:                   OK
        +++ OK, passed 100 tests.
      Multiplicative Right Identity:                  OK
        +++ OK, passed 100 tests.
      Multiplication Left Distributes Over Addition:  OK
        +++ OK, passed 100 tests.
      Multiplication Right Distributes Over Addition: OK
        +++ OK, passed 100 tests.
      Multiplicative Left Annihilation:               OK
        +++ OK, passed 100 tests.
      Multiplicative Right Annihilation:              OK
        +++ OK, passed 100 tests.
      Additive Inverse:                               OK
        +++ OK, passed 100 tests.
      Subtraction:                                    OK
        +++ OK, passed 100 tests.
    Eq
      Transitive:                                     OK
        +++ OK, passed 100 tests.
      Symmetric:                                      OK
        +++ OK, passed 100 tests.
      Reflexive:                                      OK
        +++ OK, passed 100 tests.
    Bits
      Conjunction Idempotence:                        OK
        +++ OK, passed 100 tests.
      Disjunction Idempotence:                        OK
        +++ OK, passed 100 tests.
      Double Complement:                              OK
        +++ OK, passed 100 tests.
      Set Bit:                                        OK
        +++ OK, passed 100 tests.
      Clear Bit:                                      OK
        +++ OK, passed 100 tests.
      Complement Bit:                                 OK
        +++ OK, passed 100 tests.
      Clear Zero:                                     OK
        +++ OK, passed 100 tests.
      Set Zero:                                       OK
        +++ OK, passed 100 tests.
      Test Zero:                                      OK
        +++ OK, passed 100 tests.
      Pop Zero:                                       OK
        +++ OK, passed 100 tests.
      Right Rotation:                                 OK
        +++ OK, passed 100 tests.
      Left Rotation:                                  OK
        +++ OK, passed 100 tests.
      Count Leading Zeros of Zero:                    OK
        +++ OK, passed 100 tests.
      Count Trailing Zeros of Zero:                   OK
        +++ OK, passed 100 tests.
    Bits
      Conjunction Idempotence:                        OK
        +++ OK, passed 100 tests.
      Disjunction Idempotence:                        OK
        +++ OK, passed 100 tests.
      Double Complement:                              OK
        +++ OK, passed 100 tests.
      Set Bit:                                        OK
        +++ OK, passed 100 tests.
      Clear Bit:                                      OK
        +++ OK, passed 100 tests.
      Complement Bit:                                 OK
        +++ OK, passed 100 tests.
      Clear Zero:                                     OK
        +++ OK, passed 100 tests.
      Set Zero:                                       OK
        +++ OK, passed 100 tests.
      Test Zero:                                      OK
        +++ OK, passed 100 tests.
      Pop Zero:                                       OK
        +++ OK, passed 100 tests.
      Right Rotation:                                 OK
        +++ OK, passed 100 tests.
      Left Rotation:                                  OK
        +++ OK, passed 100 tests.
      Count Leading Zeros of Zero:                    OK
        +++ OK, passed 100 tests.
      Count Trailing Zeros of Zero:                   OK
        +++ OK, passed 100 tests.
    Integral
      Quotient Remainder:                             OK
        +++ OK, passed 100 tests; 2 discarded.
      Division Modulus:                               OK
        +++ OK, passed 100 tests; 6 discarded.
      Integer Roundtrip:                              OK
        +++ OK, passed 100 tests.
    Ord
      Antisymmetry:                                   OK
        +++ OK, passed 100 tests.
      Transitivity:                                   OK
        +++ OK, passed 100 tests.
      Totality:                                       OK
        +++ OK, passed 100 tests.
    Enum
      Succ Pred Identity:                             OK
        +++ OK, passed 100 tests; 4 discarded.
      Pred Succ Identity:                             OK
        +++ OK, passed 100 tests.
```

</p>
</details>

## Example Usage
```haskell
{-# LANGUAGE DataKinds, BinaryLiterals #-}

import qualified WordN.Word8 as Word8

type Bit = Word8.WordN 1

zero :: Bit
zero = 0b0

one :: Bit
one = 0b1

-- Literal 2 is out of the Bit range 0..1
-- two :: Bit
-- two = 0b10 

type BitField = Word8.NoMask 7
-- uppermost bit unaffected in serialization / arithmetic
-- show (0b10000000) == "0"
```
