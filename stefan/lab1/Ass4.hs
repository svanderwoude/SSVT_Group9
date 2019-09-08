-- LAB 1
-- Assignment 4
-- Time: 15 minutes (TODO update with test)

module Ass4 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Find all reversable primes in the range [1..n]
findReversablePrimes :: Integer -> [Integer]
findReversablePrimes n = [x | x <- [1..n], prime x, prime (reversal x)]

-- Find all reversable primes in the range [1..10000]
findReversablePrimesLimited :: [Integer]
findReversablePrimesLimited = findReversablePrimes 10000

-- Test
-- TODO
