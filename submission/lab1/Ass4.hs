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

-- Test a single prime for reversability
testReversablePrimes :: Integer -> Bool
testReversablePrimes n = prime n && prime (reversal n)

-- Test a set of primes for reversability
testReversablePrimesSet :: [Integer] -> Bool
testReversablePrimesSet xs = all testReversablePrimes xs
