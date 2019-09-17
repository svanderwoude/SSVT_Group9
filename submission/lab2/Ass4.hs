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

-- Properties:
-- 1. All elements in list xs are also in list ys
isPermutationPropOne :: Eq a => [a] -> [a] -> Bool
isPermutationPropOne xs ys = all (\x -> elem x ys) xs

-- 2. All elements in list ys are also in list xs
-- (implied by combination of 1 and 3 as well)
isPermutationPropTwo :: Eq a => [a] -> [a] -> Bool
isPermutationPropTwo xs ys = all (\y -> elem y xs) ys

-- 3. List xs and ys are the same size
isPermutationPropThree :: Eq a => [a] -> [a] -> Bool
isPermutationPropThree xs ys = length xs == length ys

-- Combine all properties
permutationProperties :: Eq a => [a] -> [a] -> Bool
permutationProperties xs ys = isPermutationPropOne xs ys && isPermutationPropTwo xs ys && isPermutationPropThree xs ys
​
data RandomIntListSmall = RandomIntListSmall [Int] deriving Show
instance Arbitrary RandomIntListSmall where
    arbitrary = fmap RandomIntListSmall (sublistOf [1..10])
​
-- Generate a random list and get all its permutations, then verify the permutation properties
quickCheckPermutations = quickCheckResult (\(RandomIntListSmall xs) -> let list = nub xs in all (permutationProperties xs) (permutations xs
