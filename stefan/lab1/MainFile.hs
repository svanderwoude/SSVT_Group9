module MainFile where
import Ass1
import Ass2
import Ass3
import Ass4
import Ass5
import Ass6
import Ass7
import Ass8
import HelperCodeLab1
import Test.QuickCheck

main = do
    -- Assignment 1
    putStrLn "==== ASSIGNMENT 1 ===="
    quickCheckResult $ forAll genPositiveIntegers testWorkshopAssTwo
    quickCheckResult $ forAll genPositiveIntegers testWorkshopAssThree

    -- Assignment 2
    putStrLn "==== ASSIGNMENT 2 ===="
    quickCheckResult $ forAll genSmallPositiveIntegers testWorkshopAssFour

    -- Assignment 3
    putStrLn "==== ASSIGNMENT 3 ===="
    quickCheckResult $ forAll genTinyPositiveIntegers testWorkshopAssFive

    -- Assignment 4
    putStrLn "==== ASSIGNMENT 4 ===="
    -- findReversablePrimesLimited
    putStrLn "+++ OK, but no tests executed."

    -- Assignment 5
    putStrLn "==== ASSIGNMENT 5 ===="
    putStrLn "--- FAILED, no tests executed."

    -- Assignment 6
    putStrLn "==== ASSIGNMENT 6 ===="
    putStrLn ("The smallest value of n is: " ++ show testExpectedPrime)

    -- Assignment 7
    putStrLn "==== ASSIGNMENT 7 ===="
    putStrLn "+++ OK, but no tests executed."

    -- Assignment 8
    putStrLn "==== ASSIGNMENT 8 ===="
    putStrLn "+++ OK, but no tests executed."
