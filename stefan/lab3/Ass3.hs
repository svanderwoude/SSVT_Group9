-- LAB 3
-- Assignment 3
-- Time: 1 hour

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
-- Step 3: determine
convertToCNF :: Form -> Form
convertToCNF f = nnf (arrowfree f)


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
    let g = convertToCNF f
    equiv f g

-- Generate a random form and see if it and it's CNF variant are equivalent
quickCheckCNF = quickCheckResult (\(RandomForm f) -> testCNF f)
