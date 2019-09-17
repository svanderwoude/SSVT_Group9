module Main where
-- Antonino Sauleo
--import Lib
--import Propreverse
import Lab2
import Test.QuickCheck
import Data.Char
import Data.List
import Debug.Trace
import System.Random


-- For testing 
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

--Small Positive Integer generator for tests
genSmallIntegers :: Gen Integer
genSmallIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x < 24)

--Small Positive Integer generator for tests
genSmallIntegersForPrime :: Gen Integer
genSmallIntegersForPrime = abs <$> (arbitrary :: Gen Integer) `suchThat` (\x -> x > 0 && x < 10000)


main :: IO ()
main = do
    -- Assignment 2
    -- Excercise 1
    putStrLn "\n==Ex 1 ==\n"
    x <- fmap (countQ (Qt 0 0 0 0 0)) (probs 10000)
    print x
    
    putStrLn "\n\n"

    -- Excercise 2
    putStrLn "\n==Ex 2 ==\n"
    printTriangleTypeIO 0 0 0 NotaTriangle
    printTriangleTypeIO 0 0 1 NotaTriangle
    printTriangleTypeIO 0 1 1 NotaTriangle
    printTriangleTypeIO (-1) 1 0 NotaTriangle
    printTriangleTypeIO 1 1 3 NotaTriangle
    printTriangleTypeIO 5 5 5 Equilateral
    printTriangleTypeIO 1 1 2 Isosceles
    printTriangleTypeIO 3 4 5 Rectangular
    printTriangleTypeIO 1 2 3 Other
    putStrLn "\n\n"

    -- Excercise 3
    putStrLn "\n==Ex 3 ==\n"
    putStrLn "== A ==\n"
    let domain = [(-10)..10]
    print $ "Test 1 is " ++ if stronger domain e1 even then "stronger" else "weaker"
    print $ "Test 2 is " ++ if stronger domain e2 even then "stronger" else "weaker"
    print $ "Test 3 is " ++ if stronger domain e3 even then "stronger" else "weaker"
    print $ "Test 4 is " ++ if stronger domain e4 even then "stronger" else "weaker"
    putStrLn "\n\n"
    -- BBBBBBB
    --exerciseThreeB


    -- Excercise 6
    putStrLn "\n==Ex 6 ==\n"
    printRot13 3 "haskell is fun"
    putStrLn "\n\n"

    -- Excercise 7
    putStrLn "\n==Ex 7 ==\n"
    printvalidateIBAN "GB82 WEST 1234 5698 7654 32"
    printvalidateIBAN "GB82 WEST 1234 5698 7654 3"
    putStrLn "\n\n"
    putStrLn "Testing valid IBAN numbers"
    ibanCheckerValidList
    putStrLn "\n\n"
    putStrLn "\nTesting invalid IBAN numbers"
    ibanCheckerInvalidList

