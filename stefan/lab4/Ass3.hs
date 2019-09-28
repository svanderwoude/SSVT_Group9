-- LAB 4
-- Assignment 3
-- Time: 45 minutes

module Ass3 where
import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

type Rel a = [(a, a)]

-- Finds the symmetric closure of a relation
symClos :: Ord a => Rel a -> Rel a
symClos r = symClos' r []

symClos' :: Ord a => Rel a -> Rel a -> Rel a
symClos' [] clos = clos
symClos' (x:xs) clos = clos ++ (symClos'' [x]) ++ (symClos' xs clos)

symClos'' :: Ord a => Rel a -> Rel a
symClos'' [(a, b)] = [(a, b)] ++ [(b, a)]
