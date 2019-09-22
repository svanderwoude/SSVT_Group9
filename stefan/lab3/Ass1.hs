-- LAB 3
-- Assignment 1
-- Time: 1h 30m (so far)

module Ass1 where
import Data.List
import HelperCodeLab3
import System.Random
import Test.QuickCheck

-- A formula f is a contradiction if it is not satisfied by any valuation
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- A formula f is a tautology if it is satisfied by all valuations
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- A formula y logically entails a formula x if and only if all the valuations
-- that satisfy y also satisfies x.
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

-- Formula x and y are equivalent if they produce the same outcomes for all
-- valuations.
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

-- Tests
-- Test contradiction by seeing if the full list of results does indeed produce
-- the same result as our definition of contradiction.
-- TODO negative results & dynamic using assignment 4
testContradiction :: Bool
testContradiction = do
    let f = Cnj [p, (Neg p)] -- Ideally use random generator from 4
    contradiction f == all (\v -> not (evl v f)) (allVals f)

-- Test tautology by seeing if our definition produces the same result as the
-- full list of results.
-- TODO negative results & dynamic using assignment 4
testTautology :: Bool
testTautology = do
    let f = Dsj [p, (Neg p)] -- Ideally use random generator from 4
    tautology f == all (\v -> evl v f) (allVals f)

-- TODO
testEntails :: Bool
testEntails = False

-- Test equivalence by seeing if two formulas that are equal return the same
-- results.
-- TODO negative results & dynamic using assignment 4
testEquiv :: Bool
testEquiv = do
    let f1 = Dsj [p, (Neg p)]
    let f2 = Dsj [(Neg p), p]
    equiv f1 f2
