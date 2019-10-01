module Ass7 where
import Ass3
import Ass5
import Data.List
import HelperCodeLab4
import SetOrd
import System.Random
import Test.QuickCheck

{-
    In order to figure out whether the symmetric closure of a transitive closure
    is the same as the transitive closure of a symmetric closure we could try to
    find an example of this ourselves, however this would be inefficient. A
    better way to do this is to try to find a counter-example using QuickChick
    which will falsify the statement that both are equal. Thus below we define
    a function that checks if the result of both operations on either side of
    the equation is the same. We will then use QuickCheck to generate random
    relations in an attempt to find a counter-example.
-}
testTrClosSymClos :: Rel Int -> Bool
testTrClosSymClos rs = symClos (trClos rs) == trClos (symClos rs)

{-
    Below is the QuickCheck function used to test testTrClossSymClos.
-}
quickCheckTrClosSymClos = quickCheckResult testTrClosSymClos

{-
    QuickCheck fails since our apparently (symClos (trClos rs)) and
    (trClos (symClos rs)) do not produce the same results. One such
    counter-example is:

    R = [(1,2),(1,3)]
    symClos (trClos R) = [(1,2),(1,3),(2,1),(3,1)]
    trClos (symClos R) = [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]

    Here we can see that it is not true that the symmetric closure of a
    transitive closure is the same as the transitive closure of a symmetric
    closure. We can therefore illustrate that there is in fact a difference
    between the two. Though we can also say that for some cases this property
    does hold.
-}
