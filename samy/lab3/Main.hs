module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


form_one :: Form
form_one = Impl (Cnj [Neg (Dsj [Prop 1, Prop 2]), Prop 3]) (Prop 4)

contradiction :: Form -> Bool
contradiction form = not $ any (\ v -> evl v form) (allVals form)

tautology :: Form -> Bool
tautology form =  all (\ v -> evl v form) (allVals form)

-- | logical entailment
-- true -> false
-- entails :: Form -> Form -> Bool


-- entails :: Form -> Form -> Bool

-- | logical equivalence -> same truth table

-- Two statements X and Y are logically equivalent if X <-> Y is a tautology
-- Source: http://sites.millersville.edu/bikenaga/math-proof/truth-tables/truth-tables.html
equiv :: Form -> Form -> Bool
equiv form_one form_two = tautology (Equiv form_one form_two)
