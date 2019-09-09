-- LAB 1
-- Assignment 1
-- Time: 1 hour (including installing and initial setup of QuickCheck)

module Ass1 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Assignment 2 of the workshop document
-- Recursive left side of the equation
workshopAssTwoLeft :: Integer -> Integer
workshopAssTwoLeft 0 = 0
workshopAssTwoLeft n = n^2 + workshopAssTwoLeft (n - 1)

-- Right side of the equation
workshopAssTwoRight :: Integer -> Integer
workshopAssTwoRight n = div (n * (n + 1) * ((2*n) + 1)) 6

-- Test
testWorkshopAssTwo :: Integer -> Bool
testWorkshopAssTwo n = workshopAssTwoLeft n == workshopAssTwoRight n


-- Assignment 3 of the workshop document
-- Recursive left side of the equation
workshopAssThreeLeft :: Integer -> Integer
workshopAssThreeLeft 0 = 0
workshopAssThreeLeft n = n^3 + workshopAssThreeLeft (n - 1)

-- Right side of the equation
workshopAssThreeRight :: Integer -> Integer
workshopAssThreeRight n = (div (n * (n + 1)) 2)^2

-- Test
testWorkshopAssThree :: Integer -> Bool
testWorkshopAssThree n = workshopAssThreeLeft n == workshopAssThreeRight n
