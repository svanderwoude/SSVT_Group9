module Lab4 where

import Data.Char
import Data.List
import Debug.Trace
import System.Random
import Test.QuickCheck 
import Data.Maybe
import SetOrd2

-- Excercise 3
-- implement a function that gives the symmetric closure of a relation, where the relation is represented as an ordered list of pairs.
-- 1 h 15 min

type Rel a = [(a, a)]

flipit :: Ord a => Rel a -> Rel a
flipit [(a, b)] = [(b, a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = symClos1 r []

symClos1 :: Ord a => Rel a -> Rel a -> Rel a
symClos1 [] clos = clos
symClos1 (x:xs) clos = clos ++ [x] ++ (flipit [x]) ++ (symClos1 xs clos)



-- Excercise 4
-- Write a function for checking whether a relation is serial:

-- isSerial :: Eq a => [a] -> Rel a -> Bool
-- isSerial = error "not yet implemented"

-- 
--totalR :: Set a -> Rel a
--totalR (Set xs) = Set [(x,y) | x <- xs, y <- xs ]


-- Excercise 5
-- define a function that gives the transitive closure of a relation, represented as an ordered list of pairs

-- infixr 5 @@

-- (@@) :: Eq a => Rel a -> Rel a -> Rel a
-- r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- trClos :: Ord a => Rel a -> Rel a 
-- trClos r = sort (trClos' r [])

-- trClos' :: Ord a => Rel a -> Rel a -> Rel a
-- trClos' closure 
--   | closure == closureUntilNow = closure
--   | otherwise                  = trClos' closureUntilNow
--   where closureUntilNow = 
--           (nub (closure ++ (closure @@ closure))) closure

