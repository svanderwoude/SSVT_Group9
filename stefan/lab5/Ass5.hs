-- LAB 5
-- Assignment 5
-- Time: 15 minutes (so far)

module Ass5 where
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Checks whether a Mersenne prime exists for prime n. Thus it checks whether
-- 2^n - 1 is also a prime number.
checkMersennePrime :: Integer -> Bool
checkMersennePrime n = prime ((2^n) - 1)

-- TODO everything else
