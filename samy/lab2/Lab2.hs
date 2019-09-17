
module Lab2 where

import Data.List
import Data.Char
import System.Random
import System.IO.Unsafe
import Test.QuickCheck


infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)


-- Ex 1
ranges :: [Float] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
ranges [] (a, b, c, d) = (a, b, c, d)
ranges probabilities (a, b, c, d)
  | head probabilities < 0.25 = ranges (tail probabilities) (a+1,b,c,d)
  | head probabilities < 0.5  = ranges (tail probabilities) (a,b+1,c,d)
  | head probabilities < 0.75 = ranges (tail probabilities) (a,b,c+1,d)
  | head probabilities < 1.0  = ranges (tail probabilities) (a,b,c,d+1)

countQuartiles amountOfValues = do
                                randomizedValues <- probs amountOfValues
                                let distributionOfRanges = ranges randomizedValues (0,0,0,0)
                                return distributionOfRanges


getPrecentage part total = 100 * part `div` total

checkWithinThreshold percentage thesholdPercentage
 | (percentage >= 25 - thesholdPercentage)  && (percentage <= 25 + thesholdPercentage) = True
 | otherwise = False

checkDifferenceRanges :: Int -> (Int,Int,Int,Int) -> Bool
checkDifferenceRanges threshold (a, b, c, d) = precentageA && precentageB && precentageC && precentageD
    where
    sumRanges = a + b + c + d
    precentageA = checkWithinThreshold (getPrecentage a sumRanges) threshold
    precentageB = checkWithinThreshold (getPrecentage b sumRanges) threshold
    precentageC = checkWithinThreshold (getPrecentage c sumRanges) threshold
    precentageD = checkWithinThreshold (getPrecentage d sumRanges) threshold


-- Positive Integer generator for tests
genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Int) (> 1000)

-- 3 as threshold
testValidityQuartiles amountOfValues = checkDifferenceRanges 3 (unsafePerformIO(countQuartiles amountOfValues))

-- +++ OK, passed 100 tests.
-- Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-- quickCheckResult $ forAll genPositiveIntegers testValidityQuartiles




-- Ex 2 Recognizing triangles

isPythagoras :: Integer -> Integer -> Integer -> Bool
isPythagoras a b c = (a^2 + b^2) == c^2


triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
  | (a + b) < c || (a + c) < b || (b + c) < a = NoTriangle
  | a == b && b == c = Equilateral
  | a == b || b == c || a == c = Isosceles
  | isPythagoras a b c || isPythagoras b c a || isPythagoras c a b = Rectangular
  | otherwise = Other


-- Ex 3 Testing properties strength


p1, p2, p3, p4 :: Int -> Bool
p1 x = even x && x > 3
p2 x = even x || x > 3
p3 x = (even x && x > 3) || even x
p4 = even

domain = [-10..10]


-- Ex 4 Recognizing Permutations
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation listOne listTwo = elem listTwo (permutations listOne)

-- Next, define some testable properties for this function, and use a number of well-chosen lists to test
-- isPermutation. You may assume that your input lists do not contain duplicates. What does this mean
-- for your testing procedure?

-- Same permutation has same length
permutationLengthProperty :: Eq a => [a] -> [a] -> Bool
permutationLengthProperty listOne listTwo = length listOne == length listTwo

-- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs


permutationRotationProperty ::  Eq a => [a] -> Int -> Bool
permutationRotationProperty listOne numberRotations = isPermutation listOne (rotate numberRotations listOne)


permutationAssociativityProperty :: Eq a => [a] -> [a] -> Bool
permutationAssociativityProperty listOne listTwo = isPermutation listOne listTwo --> isPermutation listTwo listOne
