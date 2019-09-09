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

-- TODO: Check if the code compiles and the test run correctly before handing in.

main = do
    -- Assignment 1
    putStrLn "==== ASSIGNMENT 1 ===="
    quickCheckResult $ forAll genPositiveIntegers testWorkshopAssTwo
    quickCheckResult $ forAll genPositiveIntegers testWorkshopAssThree




    -- Assignment 2
    putStrLn "==== ASSIGNMENT 2 ===="
    quickCheckResult $ forAll genSmallPositiveIntegers testWorkshopAssFour
    -- TODO: Add answer on questions - Nino
    --       Question: Is the property hard to test? If you find that it is, can you given a reason why?
    --       Answer:

    --      Question: Give your thoughts on the following issue: when you perform the test
    --                for exercise 4, what are you testing actually? Are you checking a mathematical
    --                fact? Or are you testing whether subsequences satisfies a part of its
    --                specification? Or are you testing something else still?
    --       Answer:




    -- Assignment 3
    putStrLn "==== ASSIGNMENT 3 ===="
    quickCheckResult $ forAll genTinyPositiveIntegers testWorkshopAssFive
    -- TODO: Add answer on questions - Nino
    --       Question: Is the property hard to test? If you find that it is, can you given a reason why?
    --       Answer:

    --      Question: Again, give your thoughts on the following issue: when you perform the test
    --                for exercise 5, what are you testing actually? Are you checking a mathematical
    --                fact? Or are you testing whether subsequences satisfies a part of its
    --                specification? Or are you testing something else still?
    --       Answer:



    -- TODO: Check testing error -> Stefan
    -- Assignment 4
    putStrLn "==== ASSIGNMENT 4 ===="
    show (testReversablePrimesSet (findReversablePrimes 1000))
    -- TODO: Add answer on questions - Ninoz
    --       Question: How would you test this function, by the way?
    --       Answer:

    -- Assignment 5
    putStrLn "==== ASSIGNMENT 5 ===="
    show exc5sumFirst101Primes
    -- TODO: Add answer on questions - Samy
    --       Question: Do you have to test that your answer is correct? How could this be checked?
    --       Answer:

    -- Assignment 6
    putStrLn "==== ASSIGNMENT 6 ===="
    putStrLn ("The smallest value of n is: " ++ show testExpectedPrime)

    -- Assignment 7
    putStrLn "==== ASSIGNMENT 7 ===="
    -- TODO: Write unit tests - Stefan


    -- Assignment 8
    putStrLn "==== ASSIGNMENT 8 ===="
    -- TODO: - Stefan
