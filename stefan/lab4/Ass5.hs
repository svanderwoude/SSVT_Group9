-- LAB 4
-- Assignment 5
-- Time: 30 minutes

module Ass5 where
import Ass3
import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y == w]

-- Finds the transitive closure of a relation
trClos :: Ord a => Rel a -> Rel a
trClos r = sort (trClos' r [])

-- Loop recursively until the input Rel is the same as the output Rel from (@@)
trClos' :: Ord a => Rel a -> Rel a -> Rel a
trClos' pre post
    | pre == post = post
    | otherwise = trClos' (nub (pre ++ (pre @@ pre))) pre
