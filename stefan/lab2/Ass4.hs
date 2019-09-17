-- LAB 2
-- Assignment 4
-- Time: 45 minutes (+ TODO QuickCheck)

module Ass4 where
import Data.List
import HelperCodeLab2
import System.Random
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

-- Test using QuickCheck
-- getRandomInt :: Int -> IO Int
-- getRandomInt n = getStdRandom (randomR (0,n))

-- getRandomInt' :: Int -> IO Int
-- getRandomInt' n = getStdRandom (randomR (-n,n))

-- randomFlip :: Int -> IO Int
-- randomFlip x = do 
--     b <- getRandomInt 1
--     if b==0 then return x else return (-x)

-- getIntL :: Int -> Int -> IO [Int]
-- getIntL _ 0 = return []
-- getIntL k n = do
--     x <- getRandomInt k
--     y <- randomFlip x
--     xs <- getIntL k (n-1)
--     return (y:xs)

-- genIntList :: IO [Int]
-- genIntList = do 
--     k <- getRandomInt' 20
--     n <- getRandomInt 10
--     getIntL k n

-- testR :: Int -> Int -> ([Int] -> [Int] -> Bool) -> IO ()
-- testR k n f = if k == n then print (show n ++ " tests passed")
--                 else do
--                   xs <- genIntList
--                   ys <- shuffle xs [1]
--                   if f xs ys then
--                     do print ("pass on: " ++ show xs ++ ", " ++ show ys)
--                        testR (k+1) n f
--                   else error ("failed test on: " ++ show xs ++ ", " ++ show ys)
