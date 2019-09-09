module Main where

import Lib
import Propreverse
import Test.QuickCheck
import Data.Char
import Data.List
import Debug.Trace


-- For testing 
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

--Small Positive Integer generator for tests
genSmallIntegers :: Gen Integer
genSmallIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x < 10)

--Small Positive Integer generator for tests
genSmallIntegersForPrime :: Gen Integer
genSmallIntegersForPrime = abs <$> (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x < 10000)


main :: IO ()
main = do
    --putStrLn "\n== Proof (Success) ==\n"
    --quickCheck prop_RevRev
    --putStrLn "\n=== Proof induction (Fail) ==\n"
    --quickCheck testSumOdds

    -- Test Lab 1 - Workshop excercise 2 ---- 43 min.
    putStrLn "\n==LAB 1 workshop#2 ==\n"
    quickCheck $ forAll genPositiveIntegers testSumSquares
    putStrLn "\n\n"

    -- Test Lab 1 - Workshop excercise 3 ---- 6 min.
    putStrLn "\n==LAB 1 workshop#3 ==\n"
    quickCheck $ forAll genPositiveIntegers testSumCubes
    putStrLn "\n\n"

    -- Test Lab 2 - Workshop excercise 4 ---- 1 hour 15 min.
    putStrLn "\n==LAB 2 workshop#4 ==\n"
    quickCheck $ forAll genSmallIntegers testPowerSet
    putStrLn "\n\n"

    -- Test Lab 3 - Workshop excercise 4 ----  35 min.
    putStrLn "\n==LAB 2 workshop#4 ==\n"
    quickCheck $ forAll genSmallIntegers testNumberOfPermutations
    putStrLn "\n\n"

    -- Test Lab 4 - Workshop excercise 5 ----  xx min.
    -- putStrLn "\n==LAB 2 workshop#4 ==\n"
    -- quickCheck $ forAll genSmallIntegersForPrime testPrimeReversal
    -- putStrLn "\n\n"






