module Ass3 where
import Data.List
import HelperCodeLab4
import System.Random
import Test.QuickCheck



-- ======================================================================================


-- Excercise 3 -> Time spent: 20min
type Rel a = [(a,a)]

{-
    What is a closure of a relation?
    
    In general, let R be a relation on a set A. R may or may not have some property P, i.e. reflexivity,
    symmetry, or transitivity. If there is a relation S with property P containing R such that S is a subset of
    every relation with property P containing R, then S is called the closure of R with respect to P.    

    What is a symmetric closure of a relation?

    Rs = R ∪ R−1 = R ∪ {(b, a) | (a, b) ∈ R}.

    The symmetric closure of R, Rs, is equal to the union of R with the transpose of R. In other words, the symmetric 
    closure must have (b, a) for every (a, b) in R. This means we have to create a union 
    between the given relation and a second relation where every tuple is reversed.
-}
symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r `union` [(x,y) | (y,x) <- r]


-- ======================================================================================
