-- LAB 5
-- Assignment 1
-- Time: 15 minutes (so far)

module Ass1 where
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Implements a memory-efficient algorithm for computing modular exponentiation
-- as explained at https://bit.ly/35a7OGj

-- TODO this is not the correct algorithm i think... :/
exM :: Integer -> Integer -> Integer -> Integer
exM b e m = exM' b e m 1 1

exM' :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
exM' b e m e' c
    | e' < e = exM' b e m (e'+1) ((c * b) `mod` m)
    | otherwise = (c * b) `mod` m
