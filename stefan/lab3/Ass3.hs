-- LAB 3
-- Assignment 3
-- Time: 15 minutes (so far)

module Ass3 where
import Data.List
import HelperCodeLab3
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
