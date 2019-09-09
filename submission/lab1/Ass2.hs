module Ass2 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Assignment 4 of the workshop document
-- Test including left and right side of the equation
testWorkshopAssFour :: Integer -> Bool
testWorkshopAssFour n = 2^n == length (subsequences [1..n])
