-- LAB 1
-- Assignment 5
-- Time:

module Ass5 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Returns the first n primes
limitedPrimes :: Int -> [Integer]
limitedPrimes n = take n [x | x <- [1..], prime x]

-- findAnswer :: [Integer] -> Int
-- findAnswer primeList = 
