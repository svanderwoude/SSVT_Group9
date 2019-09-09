-- LAB 1
-- Assignment 2
-- Time: 30 minutes

module Ass2 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Assignment 4 of the workshop document
powerset :: [x] -> [[x]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

-- Test including left and right side of the equation
testWorkshopAssFour :: Integer -> Bool
testWorkshopAssFour n = 2^n == length (powerset [1..n])
