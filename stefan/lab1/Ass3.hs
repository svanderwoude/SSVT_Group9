-- LAB 1
-- Assignment 3
-- Time: 30 minutes

module Ass3 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Assignment 5 of the workshop document
-- Recursive left side of the equation (factorial)
testWorkshopAssFiveLeft :: Integer -> Integer
testWorkshopAssFiveLeft 0 = 1
testWorkshopAssFiveLeft n = n * testWorkshopAssFiveLeft (n - 1)

-- Test including left and right side of the equation
testWorkshopAssFive :: Integer -> Bool
testWorkshopAssFive n = testWorkshopAssFiveLeft n == genericLength (permutations [1..n])
