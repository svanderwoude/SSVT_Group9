module Ass5 where
import Ass4
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

-- Generate all derangements for a given list
deranWithList :: [Integer] -> [[Int]]
deranWithList x = [map fromIntegral p | p <- permutations x, isDerangement p x]

-- Test
testDerangement :: [Integer] -> [Integer] -> Bool -> Bool
testDerangement xs ys e = isDerangement xs ys == e

testDerangementIO :: [Integer] -> [Integer] -> Bool -> IO()
testDerangementIO xs ys e = putStrLn ("Check if " ++ show xs ++ " and " ++ show ys ++ " are derangements: " ++ (show $ isDerangement xs ys) ++ " (expected " ++ show e ++ ")")

-- Properties
-- Includes all properties of Ass4, and with one more below
-- source: https://codegolf.stackexchange.com/a/113942
isDerangementPropertyOne :: Eq a => [a] -> [a] -> Bool
isDerangementPropertyOne xs ys = xs /= ys

-- Combined properties
isDerangementProperties :: Eq a => [a] -> [a] -> Bool
isDerangementProperties xs ys = permutationProperties xs ys && isDerangementPropertyOne xs ys

-- Uses part of Ass4 as well
-- Generate a random list and get all its permutations except for the exact same list,
-- then verify the deran properties
quickCheckIsDerangement = quickCheckResult (\(RandomIntListSmall xs) -> let list = nub xs in all (isDerangementProperties xs) (deranWithList (map toInteger xs)))
