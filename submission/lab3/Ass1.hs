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
testContradictionIO :: Form -> Bool -> IO()
testContradictionIO f e = putStrLn ("Testing whether " ++ show f ++ " meets our expectations regarding being a contradition: " ++ show (contradiction f == e))

-- Test tautology by seeing if our definition produces the same result as
-- expected.
testTautologyIO :: Form -> Bool -> IO()
testTautologyIO f e = putStrLn ("Testing whether " ++ show f ++ " meets our expectations regarding being a tautology: " ++ show (tautology f == e))

-- Test logical entailment by seeing if our definition produces the same result
-- as expected.
testEntailsIO :: Form -> Form -> Bool -> IO()
testEntailsIO f g e = putStrLn ("Testing whether " ++ show f ++ " and " ++ show g ++ " meet our expectations regarding being logically entailing: " ++ show (entails f g == e))

-- Test equivalence by seeing if two formulas that are equal return the same
-- results.
testEquivIO :: Form -> Form -> Bool -> IO()
testEquivIO f g e = putStrLn ("Testing whether " ++ show f ++ " and " ++ show g ++ " meet our expectations regarding being equivalent: " ++ show (equiv f g == e))
