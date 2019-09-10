module Propreverse where
--Antonino Sauleo
  
import Data.Char
import Data.List
import Debug.Trace
import HelperCode


-- Random tests will delete :)
prop_RevRev xs = reverse (reverse xs) == xs
  where types = xs::[Int]

sumOdds' :: Integer -> Integer
sumOdds' n = sum [ 2*k - 1 | k <- [1..n] ]

sumOdds :: Integer -> Integer
sumOdds n = n^2

testSumOdds :: Integer -> Bool
testSumOdds n = sumOdds' n == sumOdds n

-- Lab 1
-- Redo exercises 2 and 3 of Workshop 1 by writing QuickCheck tests for these statements.)
-- Workshop excercise 2 
sumSquaresone :: Integer -> Integer
sumSquaresone n = sum [ k^2 | k <- [1..n] ]

sumSquares :: Integer -> Integer
sumSquares n = (n*(n+1)*(2*n+1)) `div` 6

testSumSquares :: Integer -> Bool
testSumSquares n = sumSquaresone n == sumSquares n

-- Workshop excercise 3
sumCubesOne :: Integer -> Integer
sumCubesOne n = sum [ k^3 | k <- [1..n] ]

sumCubes :: Integer -> Integer
sumCubes n = (n*(n+1) `div` 2)^2

testSumCubes :: Integer -> Bool
testSumCubes n = sumCubesOne n == sumCubes n

-- Lab 2 
-- (Redo exercise 4 of Workshop 1 by replacing sets by lists, 
-- and testing the property for integer lists of the form [1..n]. 
-- You can use subsequences :: [a] -> [[a]] for the list of all 
-- subsequences of a given list.)

setOne :: Integer -> Int
setOne n = 2^n

setSubsequence :: Integer -> [[Integer]]
setSubsequence n = subsequences [1..n]

testPowerSet :: Integer -> Bool
testPowerSet n = setOne n == length(setSubsequence n)

-- Lab 3 
-- (Redo exercise 5 of Workshop 1 by replacing sets by lists, 
-- and testing the property for integer lists of the form [1..n].)

numberOfPermutations :: Integer -> [[Integer]]
numberOfPermutations n = permutations [1..n]

numberOfPermutations2 :: Integer -> Integer
numberOfPermutations2 0 = 1
numberOfPermutations2 n = n * numberOfPermutations2 (n - 1)

testNumberOfPermutations :: Integer -> Bool
testNumberOfPermutations n = numberOfPermutations2 n == toInteger(length $ numberOfPermutations n)


-- Lab 4 Does it include negatives?
-- The natural number 13 has the property that it is prime and its reversal, 
-- the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

-- primeReversal :: Integer -> [Integer]
-- primeReversal n = [ x | x <- [1..n], prime x, prime (reversal x) ]

-- STILL WORKING ON IT
-- isPrime :: Integer -> [Integer]
-- isPrime k = [ x | x <- [2..k - 1], k > 1, k `mod` x == 0]

-- testPrimeReversal :: Integer -> Bool
-- testPrimeReversal n = isPrime n == primeReversal n


