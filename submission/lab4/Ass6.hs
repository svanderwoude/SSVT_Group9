module Ass6 where
import Data.List
import HelperCodeLab4
import System.Random
import Test.QuickCheck

-- Excercise 6 -> Time spent: 1h 30min
{-
    Generate random relations by taking 2 randomly generated integer lists and zipping
    them.
-}
genRandomRel :: IO (Rel Int)
genRandomRel = do
    list1 <- genIntList
    list2 <- genIntList
    return (zip list1 list2)


{-
    If an element of the relation is (x,y), then every x in the relation, should exist
    as a y in the relation too.
-}
symClosProp1 :: Ord a => Rel a -> Bool
symClosProp1 rs = all (\(x,y) -> any (\(x1,y1) -> x == y1) rs) rs

{-
    If an element of the relation is (x,y), then every y in the relation, should exist
    as a x in the relation too.
-}
symClosProp2 :: Ord a => Rel a -> Bool
symClosProp2 rs = all (\(x,y) -> any (\(x1,y1) -> y == x1) rs) rs

{-
    Apply symClos to a given Rel Int and check whether the 2 properties hold for the
    output.
-}
symClosTest :: Rel Int -> Bool
symClosTest rs = let symClosure = symClos rs
    in symClosProp1 symClosure && symClosProp2 symClosure

{-
    Tests a property of a relation.
-}
testRelation :: (Rel Int -> Bool) -> Int -> IO ()
testRelation p 0 = putStrLn "Finished tests!"
testRelation p n = do
    rel <- genRandomRel
    putStrLn ("Test " ++ show n ++ " : " ++ show (p rel)) 
    testRelation p (n-1)

{-
    We test the 2 properties for symmetric closure defined above with the custom test
    function testRelation.
-}
testSymClos = testRelation symClosTest 100

quickCheckSymClos = quickCheckResult symClosTest

{-
    Check if for every pair (x,y) in the relation, for which exists another pair (y,z),
    there is also a pair (x,z).
-}
trClosProp1 :: Rel Int -> Bool
trClosProp1 rs = null [ (x, y1) | (x,y) <- rs, (x1,y1) <- rs, y == x1 && (x,y1) `notElem` rs ]

{-
    Apply trClos to a relation and check that the trClosProp1 applies to the output.
-}
trClosTest :: Rel Int -> Bool
trClosTest rs = trClosProp1 $ trClos rs

{-
    Test the property using the custom defined function and using QuickCheck.
-}
testTrClos = testRelation  trClosTest 100

quickCheckTrClos = quickCheckResult trClosTest