-- LAB 5
-- Assignment 4
-- Time: 15 minutes (so far)

module Ass4 where
import Ass3
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Generates Carmichael numbers of some sort
carmichael :: [Integer]
carmichael = [(6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1)]

-- TODO tests, can we feed this into the test of Ass3 in order to test if it
-- fools the algorithm presented?
