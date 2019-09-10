module Ass6 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Returns the first n primes
limitedPrimes :: Int -> [Integer]
limitedPrimes n = take n [x | x <- [1..], prime x]

-- Returns the multiplication of a list of primes
getMultipliedPrimes :: [Integer] -> Integer
getMultipliedPrimes xs = product xs

-- Returns (P1 * P2 * Pn) + 1 which is supposedly a prime number as well
getExpectedPrime :: Int -> Integer
getExpectedPrime n = getMultipliedPrimes (limitedPrimes n) + 1

-- Returns the first value of n where the getExpectedPrime function does not
-- generate a prime
-- Result: the smallest value of n is 6
testExpectedPrime :: Int
testExpectedPrime = head [x | x <- [2..12], not (prime (getExpectedPrime x))]
