module Ass2 where
import Lecture5
import Data.List
import Test.QuickCheck

-- Excercise 2 Time spent: 40mins
{-
    A custom generator for QuickCheck with generates positive Ints.
-}
genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (>0)

{-
    A composite number is a number that is not prime (meaning it has divisors
    different from 1 and itself).
-}
composite :: [Integer]
composite = [x | x <- [2..], not (prime x)]


{-
    If a number is composite, then it should have divisors different from 1
    and itself.
-}
compositeProp :: Integer -> Bool
compositeProp x = not $ null [d | d <- [2..(x-1)], x `mod` d == 0]


{-
    We can use QuickCheck to test the composite function by using a randomly
    generated integer from QuickCheck and using it as the element we want to
    retrieve from the output of composite. Meaning that if QuickCheck gives
    us the number 5, then we take the 5th element of the output of composite
    and then check if compositeProp holds for it. Since QuickCheck generates
    random numbers as well, we use the absolute value, so we do not have to
    create a custom generator.
-}
compositeTest = quickCheckResult $ forAll genPositiveIntegers (\x -> compositeProp (composite!!x))