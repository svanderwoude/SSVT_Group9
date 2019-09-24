module Ass3 where
import Ass1
import Ass4
import Data.List
import HelperCodeLab3
import System.IO.Unsafe
import System.Random
import Test.QuickCheck

-- How to convert a boolean formula into CNF form?
-- Step 1: remove arrows from the formula
-- We can remove all arrows from the formula using the provided arrowfree method
--
-- Step 2: conversion to negation normal form
-- We can convert the formula to negation normal form using the provided nnf
-- method
--
-- Step 3:
-- First, we clean up --> and <-> and we push negations inwards such that only atoms are negated.
cnf :: Form -> Form
cnf f = cnf' ((nnf.arrowfree) f)

cnf' :: Form -> Form
-- A property results in itself
cnf' (Prop x) = Prop x
-- A negated property results in itself. It is not possible to have a negated formula (Cnj|Dsj), since we call
-- this function with NNF propisition already
cnf' (Neg p) = Neg p
-- If there is Cnj over Cnj, this can be replaced with a single Cnj. For example, P && (Q && R) is equal to
-- P && Q && R. The pattern match does not seem to work
cnf' (Cnj ((Cnj xs):fs)) = Cnj (map cnf' (xs ++ fs))
cnf' (Cnj fs) = Cnj (map cnf fs)
-- If there is Dsj over Cnj, we have to convert it by applying distributive law. For example, P || (Q && R), should
-- result in (P || Q) && (P || R). This pattern would only match a case where the Cnj is the first element, however
-- if it is in any place of the list of the disjunction, the same rule should be applied (Not sure how to do that).
cnf' (Dsj ((Cnj xs):fs)) = Cnj (map (\(x, f) -> Dsj [cnf' x, cnf' f]) [(x, f) | x <- xs, f <- fs])
-- Any other Dsj is just itself with converting its members to cnf.
cnf' (Dsj fs) = Dsj (map cnf' fs)

-- isCnf is used in converting to cnf as a form must be checked over and over
-- untill it is cnf, if not it has to be rerun (as this runs inward it can take
-- a long time for long forms). DOESN'T CHECK CORRECTLY
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Prop x)) = True
isCnf (Neg _) = False
isCnf (Dsj xs) = not (any containsCnj xs)  -- Correct?
isCnf (Cnj xs) = not (any containsCnj xs)  -- Correct?

-- Simple function that checks if an element is a conjucntion.
containsCnj :: Form -> Bool
containsCnj (Cnj xs) = True
containsCnj _ = False

toCnf :: Form -> Form
toCnf f = while (not . isCnf) cnf f

-- Test
-- Custom datatype used to generate and store random forms
data RandomForm = RandomForm Form deriving (Show)
instance Arbitrary RandomForm where
    arbitrary = do
        -- It's best not to use unsafePerformIO, but no other solution was found
        let f = unsafePerformIO formGenerator
        return (RandomForm f)

-- See if a form and it's CNF variant are equivalent
testCNF :: Form -> Bool
testCNF f = do
    let g = toCnf f
    equiv f g

testCnfIO :: Form -> Form -> IO()
testCnfIO f g = putStrLn ("Testing whether " ++ show f ++ " is converted into our expected CNF formula: " ++ show (toCnf f == g))

-- Generate a random form and see if it and it's CNF variant are equivalent
quickCheckCNF = quickCheckResult (\(RandomForm f) -> testCNF f)
