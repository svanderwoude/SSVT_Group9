module Ass5 where
import Ass3
import Data.List
import HelperCodeLab4
import System.Random
import Test.QuickCheck


-- Excercise 5 -> Time spent: 30min

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{-
    To calculate the transitive closure, we have to keep applying the @@ operator to the
    relation and unify the result with the previous iteration until we come to a point
    where we cannot add any new items. To implement this, we use fp function which will
    do the unification and the application of @@ until the same result is yielded twice.
    The result should be sorted.
-}
trClos :: Ord a => Rel a -> Rel a
trClos r = sort $ fp (\x -> x `union` (x @@ x)) r
