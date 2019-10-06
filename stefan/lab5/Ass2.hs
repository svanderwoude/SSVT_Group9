-- LAB 5
-- Assignment 2
-- Time: 15 minutes (so far)

module Ass2 where
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Generates an infinite list of composite natural numbers
composites :: [Integer]
composites = [x | x <- [1..], not (prime x)]

-- TODO, how would we test this?
