-- LAB 2
-- Assignment 4
-- Time: 30 minutes (+ TODO testing)

module Ass4 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

-- Assuming that the input lists do not contain duplicates, and not
-- allowed to use Ord as well.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (\x -> elem x ys) xs
