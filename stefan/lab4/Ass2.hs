-- LAB 4
-- Assignment 2
-- Time: 45 minutes (so far)

module Ass2 where
import Data.List
import SetOrd
import System.Random
import Test.QuickCheck

-- Set intersection operation
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection a b = setIntersection' a b (Set [])

setIntersection' :: (Ord a) => Set a -> Set a -> Set a -> Set a
setIntersection' (Set []) b intersection = intersection
setIntersection' (Set (x:xs)) b intersection
    | inSet x b = setIntersection' (list2set xs) b (insertSet x intersection)
    | otherwise = setIntersection' (list2set xs) b intersection

-- Set union operation
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion a b = setUnion' (setDifference a b) (setIntersection a b)

setUnion' :: (Ord a) => Set a -> Set a -> Set a
setUnion' (Set []) union = union
setUnion' (Set (x:xs)) union = setUnion' (list2set xs) (insertSet x union)

-- Set difference operation
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference a b = setDifference'' (setDifference' a b (Set [])) (setDifference' b a (Set []))

setDifference' :: (Ord a) => Set a -> Set a -> Set a -> Set a
setDifference' (Set []) b difference = difference
setDifference' (Set (x:xs)) b difference
    | not (inSet x b) = setDifference' (list2set xs) b (insertSet x difference)
    | otherwise = setDifference' (list2set xs) b difference

setDifference'' :: (Ord a) => Set a -> Set a -> Set a
setDifference'' (Set []) combined = combined
setDifference'' (Set (x:xs)) combined = setDifference'' (list2set xs) (insertSet x combined)
