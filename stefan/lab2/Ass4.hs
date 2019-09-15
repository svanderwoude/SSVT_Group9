-- LAB 2
-- Assignment 4
-- Time: 45 minutes (+ TODO QuickCheck)

module Ass4 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

-- Assuming that the input lists do not contain duplicates, and not
-- allowed to use Ord as well.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (\x -> elem x ys) xs

testIsPermutation :: [Int] -> [Int] -> Bool -> Bool
testIsPermutation xs ys e = isPermutation xs ys == e

testIsPermutationIO :: [Int] -> [Int] -> Bool -> IO()
testIsPermutationIO xs ys e = putStrLn ("Check if " ++ show xs ++ " and " ++ show ys ++ " are permutations: " ++ (show $ isPermutation xs ys) ++ " (expected " ++ show e ++ ")")

-- Test using QuickCheck
