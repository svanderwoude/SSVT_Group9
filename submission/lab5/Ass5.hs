module Ass5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture5


-- Exercise 5
-- You can use the Miller-Rabin primality check to discover some large Mersenne primes. The recipe: take a prime p, 
-- and use the Miller-Rabin algorithm to check whether 2p−1 is also prime. Find information about Mersenne primes on 
-- internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.

-- Checks whether a Mersenne prime exists for prime n. Thus it checks whether
-- 2^n - 1 is also a prime number. Using this function my laptop could only hold numbers until prime 31, after than
-- that it would begin lagging.
checkMersennePrime :: Integer -> Bool
checkMersennePrime n = prime ((2^n) - 1)
-- Returns a list of exponents prime numbers listed by Mersenne.
realPrimes :: [Integer]
realPrimes = [x | x <- [2..32], prime x, checkMersennePrime x]
-- Returns a list of the resulting Mersenne primes
mersenneP :: [Integer]
mersenneP = [ (2^x) - 1 | x <- [2..32], prime x, checkMersennePrime x]
 

-- https://en.wikipedia.org/wiki/Mersenne_prime
-- The best method presently known for testing the primality of Mersenne numbers is the Lucas–Lehmer primality test. 
-- The Lucas-Lehmer test is an efficient deterministic primality test for determining if a Mersenne number M_n is prime.
s mp 1 = 4 `mod` mp
s mp n = ((s mp $ n-1)^2-2) `mod` mp
lucasLehmer 2 = True
lucasLehmer p = s (2^p-1) (p-1) == 0

-- The Sieve of Eratosthenes is a simple algorithm that finds the prime numbers up to a given integer.
-- Using this function, together with the Lucas-Lehmer test the numbers could hold more or less until 20, but depending on the laptop
-- this number could increase.
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

-- Method to print Miller-Rabin primeMR function result
useBool :: IO Bool -> IO ()
useBool a = do
    b <- a
    putStr (show b ++ " | ")

useAllBools :: [IO Bool] -> IO()
useAllBools [] = putStrLn "end"
useAllBools (x:xs) = do
    useBool x
    useAllBools xs

-- The Miller-Rabin test is an efficient probabilistic primality test based on strong pseudoprimes.
millerR :: Int -> [Integer] -> [IO Bool]
millerR _ [] = []
millerR i (x:xs) = primeMR i x : millerR (i+1) xs