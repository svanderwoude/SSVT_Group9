module Ass2 where
import Data.List
import HelperCodeLab3
import System.Random
import Test.QuickCheck

-- In order to test the parser we will have to check three things:
-- 1. Invalid formula strings return no formula objects
-- 2. Valid formula strings return valid formula objects
-- 3. Valid formula strings return equivalent formula objects
--
-- There are a few ways to do this, firstly we could create manual (unit) tests
-- comparing expected output with real output. We would have to test all types
-- of operators (sub-formulas) for correct output. Since post-condition 3
-- above also implies that number 2 is correct, however we can still verify this
-- by feeding the resulting formula with the provided allVals method.
--
-- Secondly, instead of providing manual tests, we could also use automatically
-- generated forms using the form generator of assignment 4. If we do this, we
-- do not test the individual sub-formulas, but check if randomly generated
-- formulas will be generated correctly. In this case we will also have to
-- assume that this generator works correctly, which we can assume since it
-- has been verified by the accompanying tests of assignment 4. This automated
-- testing does not check for invalid formulas, but we could manipulate the
-- generated formulas to do this as well.


-- Function providing manual testing, used in MainFile
testManual :: String -> [Form] -> Bool
testManual s e = parse s == e
