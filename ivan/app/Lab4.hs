module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck   
import SetOrd
import Lecture4

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \ x -> p x && q x

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- Excercise 1 -> Time spent: 1h 15min

{-
    We generate a random Set Int by using the function for generating a random
    list from Lecture 2 and then applying list2set to convert the generated list
    to a Set. list2set will filter out duplicates and will order the Set, therefore
    the random list generator should not take care of that.

    For convenience, the numbers in the list are up to 1000 so they can be easily
    visualized in the console and the length of the generated list is not more than 20.
    Note: the set can have less members than the generated list as duplicates are
    removed.
-}
genSet :: IO (Set Int)
genSet = list2set <$> genIntList

genIntList :: IO [Int]
genIntList = do
    k <- getRandomInt 1000
    n <- getRandomInt 20
    getIntL k n

getIntL :: Int -> Int -> IO [Int]
getIntL _ 0 = return []
getIntL k n = do
    x <- getRandomInt k
    y <- randomFlip x
    xs <- getIntL k (n-1)
    return (y:xs)

randomFlip :: Int -> IO Int
randomFlip x = do
    b <- getRandomInt 1
    if b==0 then return x else return (-x)

getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

{-
    QuickCheck generator uses the arbitrary function for generating a list of Int and then
    we pass it as a parameter to list2set.
-}
setGenerator :: Gen (Set Int)
setGenerator = list2set <$> (arbitrary :: Gen [Int]) 

instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = list2set <$> arbitrary
    

-- Set properties true for every ordered set
{- 
    Check whether the list's length is equal to the list's length after all duplicates
    are removed. If that is the case, then no duplicates were present in the original
    list.
-}
hasNoDuplicates :: Ord a => Set a -> Bool 
hasNoDuplicates (Set xs) = length xs == length (nub xs)

-- The list must be ordered as Set maintains ordering of the elements
isOrderedSet :: Ord a => Set a -> Bool
isOrderedSet (Set xs) = isOrdered xs

{-
    Note that we do not check >= but strictly > since the Set cannot contain duplicates,
    so it is never possible to have a case where 1 element is equal to another.
-}
isOrdered :: Ord a => [a] -> Bool
isOrdered [] = True
isOrdered (x:xs) = all (> x) xs && isOrdered xs

baseSetOrdProps :: Ord a => Set a -> Bool
baseSetOrdProps = isOrderedSet Lab4..&&. hasNoDuplicates


-- ======================================================================================


-- Excercise 2 -> Time spent: 2h
{-
    An intersection of 2 sets is the set that contains all elements that exist in both
    lists.
-}
intersectionSets :: Ord a => Set a -> Set a -> Set a
intersectionSets (Set ls1) s2 = Set [x | x <- ls1, x `inSet` s2]

{-
    The set must be ordered, must not contain any duplicates and should have only the
    elements that exist in both sets. We use the Haskell implementation of intersect for
    lists as we can accept that its implementation is correct and we sort the result, then
    we compare that the resulting list is the same as the list of the intersection set.
-}
intersectionSetsProps :: Ord a => Set a -> Set a -> Set a -> Bool
intersectionSetsProps (Set ls1) (Set ls2) s3@(Set ls3) =
    sort (ls1 `intersect` ls2) == ls3 && baseSetOrdProps s3

-- test with the custom generator
intersectionSetsTest = quickCheckResult $ forAll setGenerator (\s1 s2 -> intersectionSetsProps s1 s2 (intersectionSets s1 s2))



{-
    A union of 2 sets is the set that contains all elements from both sets. We can get
    that by concatenating the 2 lists of each set and applying list2set, such that
    duplicates are filtered and the items are ordered.
-}
unionSets :: Ord a => Set a -> Set a -> Set a
unionSets (Set ls1) (Set ls2) = list2set (ls1 ++ ls2)

{-
    The set must be ordered, must not contain any duplicates and should contain all
    elements from both sets. To check the last condition we use the HAskell implementation of union,
    then we make sure that the resulting list is equal to the list of the union set.
-}
unionSetsProps :: Ord a => Set a -> Set a -> Set a -> Bool
unionSetsProps (Set ls1) (Set ls2) s3@(Set ls3) = sort (ls1 `union` ls2) == ls3 && baseSetOrdProps s3

-- test with the custom generator
unionSetsTest = quickCheckResult $ forAll setGenerator (\s1 s2 -> unionSetsProps s1 s2 (unionSets s1 s2))


{-
    A difference of 2 sets is the set that contains all elements from set 1 that are
    not in set 2 and all elements from set 2 that are not present in set 1. We can get
    that by getting the list difference of the lists of the given sets. Since the list
    difference operator (\\) is not associative, we have to apply it twice in order to
    get all items.
-}
differenceSets :: Ord a => Set a -> Set a -> Set a
differenceSets (Set ls1) (Set ls2) = list2set $ (ls1 \\ ls2) ++ (ls2 \\ ls1)

{-
    The set must be ordered, must not contain any duplicates and should contain all
    elements from set 1 that are not present in set 2 and all elements in set 2 that
    are not present in set 1. We check the last condition by verifying that all
    elements of the difference set are present either in set 1 or in set 2 but not in
    both.
-}
differenceSetsProps :: Ord a => Set a -> Set a -> Set a -> Bool
differenceSetsProps (Set ls1) (Set ls2) s3@(Set ls3) =
    all (\l3 -> (l3 `elem` ls1) `xor` (l3 `elem` ls2)) ls3 && baseSetOrdProps s3

-- test with the custom generator
differenceSetsTest = quickCheckResult $ forAll setGenerator (\s1 s2 -> differenceSetsProps s1 s2 (differenceSets s1 s2))



-- ======================================================================================


-- Excercise 3 -> Time spent: 20min
type Rel a = [(a,a)]

{-
    The symmetric closure is equal to the union of R with the transpose of R. This means
    we have to create a union between the given relation and a second relation where
    every tuple is reversed.
-}
symClos :: Ord a => Rel a -> Rel a
symClos r = sort $ r `union` [(x,y) | (y,x) <- r] 


-- ======================================================================================


-- Excercise 4 -> Time spent: 50min (so far)

{-
    A serial relation is one where for every x in the domain, there is a y, for which xRy.
    To check that we write a function which says that for all elements in the domain, there
    should be a tuple in the relation, where the element of the domain exists as X, X /= Y
    and Y is also part of the domain. 
-}
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial ds rs = all (\d -> any (\(x,y) -> d == x && d /= y && y `elem` ds) rs) ds 


-- ======================================================================================


-- Excercise 5 -> Time spent: 30min

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

{-
    To calculate the transitive closure, we have to keep applying the @@ operator to the
    relation and unify the result with the previous iteration until we come to a point
    where we cannot add any new items. To implement this, we use fp function which will
    do the unification and the application of @@ until the same result is yielded twice.
    The result should be sorted.
-}
trClos :: Ord a => Rel a -> Rel a 
trClos r = sort $ fp (\x -> x `union` (x @@ x)) r


-- ======================================================================================


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
testTrClos = testRelation trClosTest 100

quickCheckTrClos = quickCheckResult trClosTest


-- ======================================================================================


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