module Ass5 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck
import Data.List

-- Check if one list is a derangement of the other list
isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement xs ys = isPermutation xs ys && all (\x -> elemIndex x xs /= elemIndex x ys) xs

-- Generate all derangements from a list [0..n-1]
deran :: Integer -> [[Integer]]
deran n = [p | p <- permutations [0..n-1], isDerangement p [0..n-1]]

-- Test
testDerangement :: [Integer] -> [Integer] -> Bool -> Bool
testDerangement xs ys e = isDerangement xs ys == e

testDerangementIO :: [Integer] -> [Integer] -> Bool -> IO()
testDerangementIO xs ys e = putStrLn ("Check if " ++ show xs ++ " and " ++ show ys ++ " are derangements: " ++ (show $ isDerangement xs ys) ++ " (expected " ++ show e ++ ")")
