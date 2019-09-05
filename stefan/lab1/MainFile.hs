module MainFile where
import Ass1
import Ass2
import Ass3
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
