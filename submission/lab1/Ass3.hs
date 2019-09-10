module Ass3 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Assignment 5 of the workshop document: Product of a list == Factorial
testWorkshopAssFiveLeft :: Int -> Int
testWorkshopAssFiveLeft n = product [1..n]

-- Test including left and right side of the equation
testWorkshopAssFive :: Integer -> Bool
testWorkshopAssFive n = testWorkshopAssFiveLeft (fromIntegral n) == genericLength (permutations [1..n])
