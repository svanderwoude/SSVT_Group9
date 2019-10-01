module Ass1 where
import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{-
    Below we find four functions which combined are able to generate a random
    set of integers, as defined by the datatype Set from SetOrd.hs
    We use two functions to do so, first of all we generate a random number which
    will be used as the length of the Set to be created. Secondly we use a random
    list generator in order to get the actual random numbers. Combined these are
    used in genSetsFromScratch to generate random Sets.
-}

-- Since we want to be able to manually review whether the function returns
-- correct results, we decided to cap our length to 100 since it allows for
-- visual checking and faster processing. This number can be increased, however
-- we believe that an upper bound of 100 should be enough to show the functionality.
genListLength :: IO Int
genListLength = do
    n <- randomRIO (0,100)
    return n

-- randomList takes a list of size n and appends a random number r recursively
-- until the list is empty and returns the new list.
-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
-- We only allow for numbers of up to n, as seen in randomRIO (0,n). We could
-- also make this value greater but it is also purposely chosen for readability.
randomList :: Int -> IO([Int])
randomList 0 = return []
randomList n = do
    r  <- randomRIO (0,n)
    rs <- randomList (n-1)
    return (r:rs)

-- This function is the main generator of the random Sets. It makes use of
-- genListLength and randomList to generate Sets of random length and with
-- random values. The set will be ordered and will contain no duplicates, following
-- the implementation of Set in SetOrd.hs
genSetsFromScratch :: IO (Set Int)
genSetsFromScratch  = do
    n <- genListLength
    list <- randomList n
    return $ list2set list

{-
    We can also create such a random Set generator using a QuickCheck
    generator. Below we can see that we first generate a list using built-in
    functionality, to the convert it to a set using the provided list2set
    method in SetOrd.hs
-}
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
