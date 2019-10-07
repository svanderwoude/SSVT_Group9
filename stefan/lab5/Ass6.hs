-- LAB 5
-- Assignment 6
-- Time:

module Ass6 where
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Tree generators taken from assignment
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function
