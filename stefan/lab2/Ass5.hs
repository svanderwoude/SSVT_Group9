-- LAB 2
-- Assignment 5
-- Time: 15 minutes (+ TODO testing)

module Ass5 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

-- Check if one list is a derangement of the other list
isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement xs ys = all (\x -> elemIndex x xs /= elemIndex x ys) xs

-- Generate all derangements from a list [0..n-1]
deran :: Integer -> [[Integer]]
deran n = [p | p <- permutations [0..n-1], isDerangement p [0..n-1]]
