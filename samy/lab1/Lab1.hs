
module Lab1 where
import Data.List
import Test.QuickCheck
-- import Test.QuickCheck.Gen


prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

--

-- ex 1

sumpowers' :: Integer -> Integer
sumpowers' n = sum [x^2 | x <- [1..n]]

sumpowers :: Integer -> Integer
sumpowers n = div (n * (n + 1) * (2 * n + 1)) 6


predicateExercise2 :: Integer -> Bool
predicateExercise2 n =  sumpowers (abs n) == sumpowers' (abs n)

-- ToDo: Same for ex3


--ex 2
-- powerset :: [Integer] -> Integer


powerset l = fromIntegral(length (subsequences l))

predicateOneExercise4 :: Integer -> Bool
predicateOneExercise4 n = n == fromIntegral(length [1..n])

predicateTwoExercise4 :: Integer -> Bool
predicateTwoExercise4 n = (powerset [1..n]) == 2^n

conditionExercise4 :: Integer -> Bool
conditionExercise4 n =  (predicateOneExercise4 n) --> (predicateTwoExercise4 n)

testExercise4 = quickCheckResult (\ n -> n > 0 && n < 25 --> conditionExercise4 n)


-- ToDo: Answer questions.

-- Is the property hard to test? If you find that it is, can you given a reason why?

-- Give your thoughts on the following issue: when you perform the test for exercise 4, what are you testing actually? Are you checking a mathematical fact? Or are you testing whether subsequences satisfies a part of its specification? Or are you testing something else still?



-- Exercise 3

-- Source: https://youtu.be/02_H3LjqMr8?t=2221
-- if n == 0 then product [1..0] ->
factorial :: Int -> Int
factorial n = product [1..n]

-- I did not use the perms function as haskell Data.List library has a builtin permutation function.
-- Found here:
-- https://stackoverflow.com/questions/40097116/get-all-permutations-of-a-list-in-haskell
testExercise5 = quickCheckResult (\ n -> n > 0 && n < 5  --> length (permutations [1..n]) == factorial n)



-- Exercise 4
primeList = takeWhile (<10000) primes
primeListThree = filter (\n -> elem (reversal n) primeList) primeList


-- How would you test the function?
-- Generate all primes
--
-- ToDo: Answer questions.


-- Exercise 5
-- https://stackoverflow.com/questions/4597820/does-haskell-have-list-slices-i-e-python
-- Get the primes from n to n + 101

getPrimesFromTo n = take 101 (filter prime [n..])
-- For each prime, check if the sum of the next 101 primes creates a prime, if not then move on untill it is found
rejectedPrimes = takeWhile (\n -> not (prime (sum (getPrimesFromTo n)))) primes
slice from to xs = take (to - from) (drop from xs)
sliceOf101Primes = slice (length rejectedPrimes) ((length rejectedPrimes) + 101) primes
primeAnswerEx5 = sum(sliceOf101Primes)


-- primeList101 = map (\n -> prime (sum (getPrimesFromTo n))) primes

-- primeList101 = takeWhile (\n -> not (prime (sum (getPrimesFromTo n)))) primes

-- Exercise 6
conjuctureSix :: Int -> [Integer] -> [Integer]
conjuctureSix n total
 | not (prime (product(take n primes) + 1)) = conjuctureSix (n + 1) (total ++ [product(take n primes) + 1])
 | otherwise = conjuctureSix (n +1)  total

conjuctureSixMin :: [Integer]
conjuctureSixMin = take 1 (conjuctureSix 0 [])


-- Excerise
accuses :: Boy -> Boy -> Bool
accuses Matthew boyTwo = not (boyTwo == Mattthew) && not (boyTwo == Carl)
accuses Peter boyTwo = boyTwo == Mattthew || boyTwo == Jack
accuses Peter Matthew = True
