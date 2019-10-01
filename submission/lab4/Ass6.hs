module Ass6 where
import Ass3
import Ass5
import Data.List
import HelperCodeLab4
import SetOrd
import System.Random
import Test.QuickCheck

{-
    The genRandomRel function generates a random relation by using genIntList to
    generate two random lists, and zipping them together to create a valid
    relation of integers.
-}
genRandomRel :: IO (Rel Int)
genRandomRel = do
    list1 <- genIntList
    list2 <- genIntList
    return (zip list1 list2)

{-
    SymClos property 1: for every x in a relation R = [(x,y)], the value of x
    should also be present as value of y in another relation. So, for every
    x in (x,i) there should also be a (j,x).
-}
symClosProp1 :: Ord a => Rel a -> Bool
symClosProp1 rs = all (\(x,y) -> any (\(x1,y1) -> x == y1) rs) rs

{-
    SymClos property 2: for every y in a relation R = [(x,y)], the value of y
    should also be present as value of x in another relation. So, for every
    y in (i,y) there should also be a (y,j).
-}
symClosProp2 :: Ord a => Rel a -> Bool
symClosProp2 rs = all (\(x,y) -> any (\(x1,y1) -> y == x1) rs) rs

{-
    Here we test whether it is true that for every symmetric closure of a
    relation the two properties defined above hold.
-}
symClosTest :: Rel Int -> Bool
symClosTest rs = let symClosure = symClos rs
    in symClosProp1 symClosure && symClosProp2 symClosure

{-
    A helper function to test a property of a relation, resulting in an IO
    output that tells us whether the provided test holds or not.
-}
testRelation :: (Rel Int -> Bool) -> Int -> IO ()
testRelation p 0 = putStrLn "Finished tests!"
testRelation p n = do
    rel <- genRandomRel
    putStrLn ("Test " ++ show n ++ " : " ++ show (p rel)) 
    testRelation p (n-1)

{-
    We test the 2 properties for symmetric closure defined above with the custom
    test function testRelation to see if the properties hold and the
    implementation is correct.
-}
testSymClos = testRelation symClosTest 100
quickCheckSymClos = quickCheckResult symClosTest

{-
    TrClos property 1: for every pair (x,y) in a relation R = [(x,y)], for which
    there also exists a pair (y,z) in R, there should also be a pair (x,z).
-}
trClosProp1 :: Rel Int -> Bool
trClosProp1 rs = null [ (x, y1) | (x,y) <- rs, (x1,y1) <- rs, y == x1 && (x,y1) `notElem` rs ]

{-
    Here we test whether it is true that for every transitive closure of a
    relation the property above holds.
-}
trClosTest :: Rel Int -> Bool
trClosTest rs = trClosProp1 $ trClos rs

{-
    We test the property for transitive closure defined above with the custom
    test function testRelation to see if the property holds and the implementation
    is correct.
-}
testTrClos = testRelation  trClosTest 100
quickCheckTrClos = quickCheckResult trClosTest
