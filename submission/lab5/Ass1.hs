module Ass1 where
import Data.List
import Lecture5
import Test.QuickCheck
import Data.Bits

-- Excercise 1 Time spent: 2h
{-
    The 2 functions exM' and exM are the implementation of the algorithm described in
    https://en.wikipedia.org/wiki/Modular_exponentiation#Pseudocode. We initially check
    whether the modulus is equal to 1 and if that is the case then the result is always 0.
    If the modulus is not equal to 1 then we call the exM' which is a recursive function
    that terminates when the exponent becomes 0. The algorithm is described in details at
    the provided link. 
-}
exM' :: Integer -> Integer -> Integer -> Integer -> Integer
exM' _ 0 _ result = result 
exM' base exponent modulus result = let 
    newExponent = shiftR exponent 1
    newBase = (base ^ 2) `mod` modulus
    newResult = if exponent `mod` 2 == 1 then (result * base) `mod` modulus else result
    in exM' newBase newExponent modulus newResult


exM :: Integer -> Integer -> Integer -> Integer
exM base exponent 1 = 0
exM base exponent modulus = exM' (base `mod` modulus) exponent modulus 1

{-
    We check our implementation with the implementation of expM which is the "naive" way
    of doing modular exponentiation. The results yielded should be the same but the performance
    of our implementation is significantly improved compared to expM.

    For example:
    base = 122293321
    exponent = 21312332
    modulus = 32133

    providing these values to expM gives as a result for 5.42sec while exM gives us the same
    result in 0.00sec (GHCi cannot measure such a low value). The result of both functions is 4723.
-}
exMProp :: Integer -> Integer -> Integer -> Bool
exMProp base exponent modulus = Lecture5.exM base exponent modulus == expM base exponent modulus

genPositiveIntegers2 :: Gen Integer
genPositiveIntegers2 = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

-- The test does not work because the generator above applies the check only to the first
-- generated value
quickCheckExM = quickCheckResult $ forAll genPositiveIntegers2 exMProp