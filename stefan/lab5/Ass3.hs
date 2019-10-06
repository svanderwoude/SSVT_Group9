-- LAB 5
-- Assignment 3
-- Time: 45 minutes (so far)

module Ass3 where
import Ass2
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Tries to find a number in a list of integers which fools Fermat's primality
-- test as implemented in primeTestsF in Lecture5.hs
--
-- By increasing the value of k we increase the accuracy of Fermat's primality
-- test, meaning that we are less likely to find numbers that fool the algorithm.
-- With a value of k=1 we see a lot of fooled numbers, whilst a value of k=3
-- or higher values of k result in less fooled numbers. The test also has some
-- randomness, which results in different results each iteration. By increasing
-- the value of k a lot we can cause the randomness to more often pick a random
-- value that is high enough to make sure the test is not fooled.
--
-- For example when setting k=10, we see that we mostly find no fooled numbers,
-- (1000 composite numbers), but if we do it often gets fooled with the number
-- 1105, which happens to be a Carmichael number.
testPrimeTestsFIO :: Int -> [Integer] -> IO()
testPrimeTestsFIO _ [] = print "No fooling number found"
testPrimeTestsFIO k (x:xs) = do
    fooled <- primeTestsF k x
    if (fooled == True) then print x else
        testPrimeTestsFIO k xs

-- TODO test
