-- LAB 5
-- Assignment 6
-- Time:

module Ass6 where
import Data.List
import Lecture5
import System.Random
import Test.QuickCheck

-- Tree generators taken from assignment
tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function


-- 1. Can you check that the number pairs (x,y) that occur in tree1 n are
-- precisely the pairs in the set {(x,y)|1<=x<=n,1<=y<=n with x,y co-prime}?
--
-- Here are some example runs for values n 1,2,3:
-- The pair generated from tree1 1 is:
-- [(1,1)]
-- The 3 pairs generated from tree1 2 are:
-- [(1,1), (2,1), (1,2)]
-- The 7 pairs generated from tree1 3 are:
-- [(1,1), (2,1), (3,2), (2,3), (1,2), (3,1), (1,3)]
--
-- What exactly does the second part - with x,y co-prime - mean?
-- This means that the two numbers in any pair are considered co-primes, which
-- means that the only positive integer that divides them is 1: their greatest
-- common divisor (gcd) is 1.
--
-- In order to get a sense of how the tree pairs are generated, it is best to
-- create a visual representation of how the tree works. If we look at the
-- example of tree1 2, we get the following tree:
--
--    (1,1)      LEVEL 1
--   /     \
-- (2,1)  (1,2)  LEVEL 2
--
-- As we can see, it generates a tree with n levels, with each level containing
-- 2 times the number of nodes (LEVEL 2 = LEVEL 1 * 2) due to it being a
-- binary tree. Below we can see that this also applies to tree1 3, which
-- contains three layers.
--
--          (1,1)           LEVEL 1
--         /     \
--      (2,1)   (1,2)       LEVEL 2
--     /   |     |   \
-- (3,2) (2,3) (3,1) (1,3)  LEVEL 3
--
-- One interesting observation we can make is that all direct children of a node
-- contain mirrored pairs (e.g. (2,1) and (1,2)). Due to the implementation, this
-- property carries over for every single node in the tree. We can see that each
-- left-branching child-node increases the x-value of the node's pair, whilst
-- the right-branching nodes increase the y-values. Due to this implementation
-- we can say that the "mirroring" effect carries on for every value of n. Below
-- we can also see that it applies to tree1 4:
--
--           (1,1)                   LEVEL 1
--         /       \
--      (2,1)      (1,2)             LEVEL 2
--     /   |      /     \
-- (3,2) (2,3) (3,1)    (1,3)        LEVEL 3
--             /   \      |  \
--          (4,3) (3,4) (4,1) (1,4)  LEVEL 4
--
-- However, as we can see the assumption we made before is incorrect. The tree
-- does NOT generate two times the number of nodes of the level above. In order
-- to figure out why, we have to inspect the implementation of tree1. We can
-- see the cause of the branching effect increasing either x or y. But we can
-- also see that there is a condition set to node creationg, which says that
-- a node pair's values combined cannot exceed the value of n. We can verify
-- that this is correct by checking all nodes in the trees above. We can
-- observe that all "leaf" nodes exceed the value of n, which explains that they
-- become leaf nodes and don't generate any branch nodes.
--
-- But how can we use this to verify that all (x,y) pairs are co-primes?
-- If we observe the tree-growth, we can see that the tree only generates
-- "points" in coordinates which are all visible from the origin without
-- "intersecting" any other points when drawing a line. This means that they
-- are co-primes, as explaied in the last part of:
-- https://en.wikipedia.org/wiki/Coprime_integers#Properties
--
-- We can test if this hypothesis is correct by checking whether all node-pairs
-- that are generated by the tree1 are in fact coprimes using a coprime checker.
-- We can do so by using the provided coprime function from Lecture5.hs. And to
-- make it even faster we can use the coprimes function to check the complete
-- list of nodes at once.


-- Converts a Tree to a list of all element values
treeToList :: Tree a -> [a]
treeToList (T x xs) = x : concatMap treeToList xs

-- Generate a tree using tree1 n, convert all values (nodes) to a list, and then
-- use this list to check if all pairs of x and y are coprimes.
testTree1Coprime :: Integer -> Bool
testTree1Coprime n = all (\(x,y) -> coprime x y) (treeToList (tree1 n))

-- QuickCheck test
quickCheckTree1 = quickCheck $ forAll genPositiveIntegers $ testTree1Coprime


-- Tree2 n works almost the same as tree1 n, however instead of using the value
-- of x as the y-argument in the generated left node, it uses the value of y.
-- This means that we no lonoger have the "mirroring" effect of tree1 that was
-- created by using the x value in this spot. However, if we observe the
-- generated trees closely, we can see that the same pairs are still present
-- in the tree. However, instead of being direct siblings, they are further
-- apart in the tree. This does however not impact the actual pair results,
-- which can lead us to conclude that tree2 will only generate coprime pairs,
-- just like tree1 does. Below we see two examples trees generated by tree2,
-- namely tree2 3 and tree2 4 respectively:
--
--          (1,1)           LEVEL 1
--         /     \
--      (2,1)   (1,2)       LEVEL 2
--     /   |     |   \
-- (3,1) (2,3) (3,2) (1,3)  LEVEL 3
--
--
--           (1,1)             LEVEL 1
--         /       \
--      (2,1)      (1,2)       LEVEL 2
--     /    \       /   \
-- (3,1)   (2,3) (3,2)  (1,3)  LEVEL 3
--   |  \              /  |
-- (4,1) (3,4)    (4,3) (1,4)  LEVEL 4
--
-- As we can see, the value pairs are still the same as in tree1, but the tree
-- is more balanced. We can actually see that it appears to mirrored completely
-- from the root node, instead of individually for each node and it's children.
-- Because the pairs are the same as in tree1 we can assume that the outcome is
-- the same, but we can verify this using an individual testing function.


-- Generate a tree using tree2 n, convert all values (nodes) to a list, and then
-- use this list to check if all pairs of x and y are coprimes.
testTree2Coprime :: Integer -> Bool
testTree2Coprime n = all (\(x,y) -> coprime x y) (treeToList (tree2 n))

-- QuickCheck test
quickCheckTree2 = quickCheck $ forAll genPositiveIntegers $ testTree2Coprime


-- Positive Integer generator for tests (from assignment 1)
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Integer) (> 0)