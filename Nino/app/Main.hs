module Main where
-- Antonino Sauleo

import Lab5
import Lecture5
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
    --putStrLn "\n==Ex 1 ==\n"
    -- x <- fmap (countQ (Qt 0 0 0 0 0)) (probs 10000)
    -- print x
    
    -- putStrLn "\n\n"

    -- -- Excercise 2
    -- putStrLn "\n==Ex 2 ==\n"
    -- printTriangleTypeIO 0 0 0 NotaTriangle
    -- printTriangleTypeIO 0 0 1 NotaTriangle
    -- printTriangleTypeIO 0 1 1 NotaTriangle
    -- printTriangleTypeIO (-1) 1 0 NotaTriangle
    -- printTriangleTypeIO 1 1 3 NotaTriangle
    -- printTriangleTypeIO 5 5 5 Equilateral
    -- printTriangleTypeIO 1 1 2 Isosceles
    -- printTriangleTypeIO 3 4 5 Rectangular
    -- printTriangleTypeIO 1 2 3 Other
    -- putStrLn "\n\n"

    -- -- Excercise 3
    -- putStrLn "\n==Ex 3 ==\n"
    -- putStrLn "== A ==\n"
    -- let domain = [(-10)..10]
    -- print $ "Test 1 is " ++ if stronger domain e1 even then "stronger" else "weaker"
    -- print $ "Test 2 is " ++ if stronger domain e2 even then "stronger" else "weaker"
    -- print $ "Test 3 is " ++ if stronger domain e3 even then "stronger" else "weaker"
    -- print $ "Test 4 is " ++ if stronger domain e4 even then "stronger" else "weaker"
    -- putStrLn "\n\n"
    -- -- BBBBBBB
    -- --exerciseThreeB


    -- -- Excercise 6
    -- putStrLn "\n==Ex 6 ==\n"
    -- printRot13 3 "haskell is fun"
    -- putStrLn "\n\n"

    -- -- Excercise 7
    -- putStrLn "\n==Ex 7 ==\n"
    -- printvalidateIBAN "GB82 WEST 1234 5698 7654 32"
    -- printvalidateIBAN "GB82 WEST 1234 5698 7654 3"
    -- putStrLn "\n\n"
    -- putStrLn "Testing valid IBAN numbers"
    -- --ibanCheckerValidList
    -- putStrLn "\n\n"
    -- --putStrLn "\nTesting invalid IBAN numbers"
    -- --ibanCheckerInvalidList


    -- -- Assigment #3
    -- putStrLn "\n==Ex 1 ==\n"
    -- putStrLn "Contradiction is : "
    -- let contradiction = Cnj [p, (Neg p)]
    -- print $ testContradiction contradiction
    
    -- putStrLn "\nTautology is : "
    -- let tauto = Dsj [p, (Neg p)]
    -- print $ testTautology tauto
    -- putStrLn "\n\n"

    -- putStrLn "\nEntails is : "
    -- --let ent = (Dsj [p, Neg q])
    -- --putStrLn "\n\n"

    -- putStrLn "\n==Ex 5 ==\n"
    -- let formula = Impl (Cnj [Neg (Dsj [Prop 1, Prop 2]), Prop 3]) (Prop 4)
    -- printnsub formula
    -- printnsub contradiction
    -- printnsub tauto
    -- print $ sub formula

    --putStrLn "\n==Ex 2 ==\n"
    --print $ composites


    putStrLn "\n==Ex 5 ==\n"

    putStrLn "List of Mersenne Primes (exponents): "
    print $ realPrimes

    putStrLn "\nList of Mersenne Primes: "
    print $ mersenneP

    -- Verify list of mersenne primes using Lucas-Lehmer test and checkMersennePrime
    let printMersennes = (\x -> putStrLn $ "\nAfter Lucas-Lehmer test + checkMersennePrime: " ++ show x)
    printMersennes $ filter lucasLehmer $ realPrimes

    -- Verify whether the numbers that realPrimes found are genuine Mersenne primes using Miller-Rabin test.
    let testMersennes = millerR 1 realPrimes
    useAllBools (testMersennes)

    -- From the results we can say realPrimes (using checkMersennePrime and prime) returns mersenne primes.
    -- however it can take a lot of processing power due to its exponential complexity.

    putStrLn "\n====\n"

    -- Prints list of mersenne primes using Sieve of Eratosthenes and Lucas-Lehmer test
    -- This function
    let printMersennes = \x -> putStrLn $ "After Lucas-Lehmer + Sieve of Eratosthenes: " ++ show x
    printMersennes $ take 20 $ filter lucasLehmer $ sieve [2..]

    -- Verify whether the numbers that Sieve of Eratosthenes and Lucas-Lehmer test found are genuine Mersenne primes.
    let testMersennesVersion2 = millerR 1 $ take 20 $ filter lucasLehmer $ sieve [2..]
    useAllBools (testMersennesVersion2)

    -- From the results we can say Sieve of Eratosthenes and Lucas-Lehmer test returns genuine mersenne primes.
    -- and it showed a better performance overall.
    
    -- We found that a prime number in order to be a Mersenne prime it requires both ((2^p) - 1) to be a prime and p to be a prime.
