module MainFile where
import Ass1
import Ass2
import Ass3
import Ass4
import Ass5
import Ass6
import Ass7
import Data.List
import HelperCodeLab2
import Test.QuickCheck

main = do
    -- Assignment 1
    putStrLn "==== ASSIGNMENT 1 ===="

    -- Assignment 2
    putStrLn "\n==== ASSIGNMENT 2 ===="
    testTriangleTypeIO 0 1 1 NoTriangle
    testTriangleTypeIO 1 1 (-1) NoTriangle
    testTriangleTypeIO 1 1 3 NoTriangle
    testTriangleTypeIO 1 1 1 Equilateral
    testTriangleTypeIO 1 1 2 Isosceles
    testTriangleTypeIO 3 4 5 Rectangular
    testTriangleTypeIO 1 2 3 Other

    -- Assignment 3
    -- output: [
    --     even x || x > 3,
    --     even x,
    --     (even x && x > 3) || even x,
    --     (even x && x > 3) || even x,
    --     even x && x > 3]
    -- Note: strength of 2, 3 and 4 are equal (manual review)
    putStrLn "\n==== ASSIGNMENT 3 ===="
    print (sortBy compareProperties [exc3_1, exc3_2, exc3_3, exc3_4, exc3_5])

    -- Assignment 4
    -- Since there are no duplicates in the input lists we create a stricter
    -- input list, but in turn make the function easier since you do not have
    -- to count occurrences of numbers, but can simply look at indices.
    putStrLn "\n==== ASSIGNMENT 4 ===="
    testIsPermutationIO [1,2,3] [2,1,3] True
    testIsPermutationIO [1,2] [1,2] True
    testIsPermutationIO [1,2] [1,2,3] False
    testIsPermutationIO [1,2,3,4] [1,2] False
    testIsPermutationIO [1,2,3] [2,3,4] False

    -- Assignment 5
    putStrLn "\n==== ASSIGNMENT 5 ===="
    testDerangementIO [1,2,3] [3,1,2] True
    testDerangementIO [1,2,3] [3,2,1] False
    testDerangementIO [1,2,3] [2,3,4] False
    -- TODO QuickCheck? & definitions

    -- Assignment 6
    putStrLn "\n==== ASSIGNMENT 6 ===="

    -- Assignment 7
    putStrLn "\n==== ASSIGNMENT 7 ===="
