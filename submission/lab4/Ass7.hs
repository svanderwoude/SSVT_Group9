module Ass7 where
import Ass3
import Ass5
import Data.List
import HelperCodeLab4
import SetOrd
import System.Random
import Test.QuickCheck

-- Excercise 7 -> Time spent: 40min

{-
    QuickCheck is very good for generating counter-examples, therefore we define a function
    that tests whether the symmetric closure of a transitive closure of a relation is the
    same as the transitive closure of the symmetric closure for that relation.

    Then we can use QuickCheck to verify whether that is the case.
-}
testTrClosSymClos :: Rel Int -> Bool
testTrClosSymClos rs = symClos (trClos rs) == trClos (symClos rs)


{-
    QuickCheck fails since our apparently (symClos (trClos rs)) and (trClos (symClos rs))
    are not equal. An example can be:

    R = [(1,2),(1,3)]
    symClos (trClos R) = [(1,2),(1,3),(2,1),(3,1)]
    trClos (symClos R) = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
-}
quickCheckTrClosSymClos = quickCheckResult testTrClosSymClos
