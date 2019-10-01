module Ass2 where
import Data.List
import HelperCodeLab4
import SetOrd
import System.Random
import Test.QuickCheck


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
baseSetOrdProps = isOrderedSet HelperCodeLab4..&&. hasNoDuplicates


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
