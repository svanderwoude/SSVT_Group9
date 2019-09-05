module Lab1 where
import Data.List
import Test.QuickCheck 

-- Common
genIntegersLargerThan1 :: Gen Integer
genIntegersLargerThan1 = abs <$> (arbitrary :: Gen Integer) `suchThat` (>1)

fac :: (Integral a) => a -> a
fac n = product [1..n]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]


-- Excercise 1
exc1n1 :: Integer -> Integer
exc1n1 n = sum [ x ^ 2 | x <- [1..n] ]

exc1n2 :: Integer -> Integer
exc1n2 n = (n * (n + 1) * (2*n + 1)) `div` 6

exc1testN :: Integer -> Bool
exc1testN n = exc1n1 n == exc1n2 n

exc1quickCheckTestN = quickCheckResult $ forAll genIntegersLargerThan1 exc1testN

exc1n3 :: Integer -> Integer
exc1n3 n = sum [ x^3 | x <- [1..n] ]

exc1n4 :: Integer -> Integer
exc1n4 n = (n * (n + 1) `div` 2) ^ 2

exc1testN2 :: Integer -> Bool
exc1testN2 n = exc1n3 n == exc1n4 n

exc1quickCheckTestN2 = quickCheckResult $ forAll genIntegersLargerThan1 exc1testN2


-- Excercise 2
exc2PA :: Integer -> [[Integer]]
exc2PA n = subsequences [1..n]

exc2testPA :: Integer -> Bool
exc2testPA n = length (exc2PA n) == 2 ^ n

exc2quickCheckTestPA = quickCheckResult $ forAll genIntegersLargerThan1 exc2testPA


-- Excercise 3
exc3perm :: Integer -> [[Integer]]
exc3perm n = permutations [1..n]

exc3testPerm :: Integer -> Bool
exc3testPerm n = toInteger (length $ exc3perm n) == fac n

exc3quickCheckTestPerm = quickCheckResult $ forAll genIntegersLargerThan1 exc3testPerm


-- Excercise 4
reversal :: Integer -> Integer
reversal = read . reverse . show

exc4PrimeReversal :: [Integer]
exc4PrimeReversal = [ x | x <- [1..10000], prime x, prime $ reversal x ]

-- TODO: figure out the testing part

