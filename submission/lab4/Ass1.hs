module Ass1 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd


-- Prerequisites:
-- Read or reread Chapter 4 and 5 of The Haskell Road.
-- Make a list of questions on specific points that cause difficulty of understanding.



--
-- Assignment 1
--


-- Implement a random data generator for the datatype Set Int, where Set is as defined in SetOrd.hs.
-- Do this from scratch.
-- The following 4 functions compose the random set generator...
-- ToDO:  Elaborate on edge cases and why you chose ranges

genListLength :: IO Int
genListLength = do
    n <- randomRIO (0,100)
    return n


-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (1,6)
    rs <- randomList (n-1)
    return (r:rs)

list2setIO :: (Monad m, Ord a) => m [a] -> m (Set a)
list2setIO list = do
    a <- list
    return $ list2set a

genSetsFromScratch :: IO (Set Int)
genSetsFromScratch  = do
    n <- genListLength
    list <- randomList n
    return $ list2set list


-- Next give a version that uses QuickCheck to random test this datatype
-- *Lab4> generate arbitrary :: IO (Set Int)
-- {-30,-28,-27,-26,-18,-15,-13,-12,-10,-3,-1,0,9,10,11,12,15,17,20,23,24,29,30}
-- *Lab4> generate arbitrary :: IO (Set Int)
-- {-21,-20,-9,-8,-4,1,7,11,23,25,26,27,30}
-- *Lab4> generate arbitrary :: IO (Set Int)
-- {-19,-13,-11,-10,-2,1,3,7,12,15,16,20,27,29}
-- *Lab4> generate arbitrary :: IO (Set Int)
-- {-21,-1,30}

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary
