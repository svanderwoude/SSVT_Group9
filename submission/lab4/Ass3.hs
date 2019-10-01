module Ass3 where
import Data.List
import HelperCodeLab4
import System.Random
import Test.QuickCheck



-- ======================================================================================


-- Excercise 3 -> Time spent: 20min
type Rel a = [(a,a)]

{-
    The symmetric closure is equal to the union of R with the transpose of R. This means
    we have to create a union between the given relation and a second relation where
    every tuple is reversed.
-}
symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r `union` [(x,y) | (y,x) <- r]


-- ======================================================================================
