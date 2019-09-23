-- LAB 3
-- Assignment 5
-- Time: 30 minutes (so far)

module Ass5 where
import Data.List
import HelperCodeLab3
import SetOrd
import System.Random
import Test.QuickCheck

-- TODO How can we test?


-- Computes the exact number of sub-formulae of the formula f
nsub :: Form -> Int
nsub f = nsub' (sub f) 0

-- Loop through each sub-formula in the Set Form created by the sub method in
-- order to count the number of sub-formulae in the formula f initially provided
-- from nsub
nsub' :: Set Form -> Int -> Int
nsub' (Set []) n = n
nsub' (Set (x:xs)) n = nsub' (Set xs) (n + 1)

-- Provided in assignment
sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- TODO tests
