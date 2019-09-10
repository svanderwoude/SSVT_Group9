-- Lab 1 submission
-- Run using ghci: 'ghci MainFile.hs'
-- Then in the shell execute the command 'main' for all tests
--
-- Results consist of answers to the questions (in comments),
-- QuickCheck test results (1, 2, 3)
-- Calculated values as answers (4, 5, 6, 8)
-- Own extra test results (4, 7)

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
    putStrLn "\n==== ASSIGNMENT 2 ===="
    quickCheckResult $ forAll genSmallPositiveIntegers testWorkshopAssFour
    --       Question: Is the property hard to test? If you find that it is, can you given a reason why?
    --       Answer: One of the reasons the property is hard to test is because of the computational power
    --               it would require for larger numbers, that's why we used small integers.

    --      Question: Give your thoughts on the following issue: when you perform the test
    --                for exercise 4, what are you testing actually? Are you checking a mathematical
    --                fact? Or are you testing whether subsequences satisfies a part of its
    --                specification? Or are you testing something else still?
    --       Answer: In exercise 4 we try to test whether subsequences satisfies a part of its
    --               specification. If the order of the elements in each subsequence does not matter, 
    --               then every subsequence is an element of the powerset of the given sequence. 
    --               If a set A contains n elements, then the powerset of A contains 2𝑛 elements.


    -- Assignment 3
    putStrLn "\n==== ASSIGNMENT 3 ===="
    quickCheckResult $ forAll genTinyPositiveIntegers testWorkshopAssFive
    --       Question: Is the property hard to test? If you find that it is, can you given a reason why?
    --       Answer: One of the reasons the property is hard to test is because of the computational power
    --               it would require for larger numbers, that's why we used small integers.

    --      Question: Again, give your thoughts on the following issue: when you perform the test
    --                for exercise 5, what are you testing actually? Are you checking a mathematical
    --                fact? Or are you testing whether perms satisfies a part of its
    --                specification? Or are you testing something else still?
    --       Answer: In exercise 5 we try to check for a mathematical fact i.e. there are n! permutations for a 
    --               list of n distinct objects. 


    -- Assignment 4
    putStrLn "\n==== ASSIGNMENT 4 ===="
    putStrLn "All reversable primes < 10000:"
    print (findReversablePrimes 10000)

    if (testReversablePrimesSet (findReversablePrimes 10000)) == True
        then putStrLn "+++ OK, passed own test."
        else putStrLn "--- ERROR, failed own test."
    --       Question: How would you test this function, by the way?
    --       Answer: The function takes a number (in this case 10000) and returns a list with numbers that are primes
    --       (with and without reversal), then to test this function we take this list of prime numbers and check if
    --       they satisfy prime condition (with and without reversal) and if all numbers inside the list are true then
    --       the funtion works


    -- Assignment 5
    putStrLn "\n==== ASSIGNMENT 5 ===="
    putStrLn "Smallest prime number that is a sum of 101 consecutive primes:"
    print exc5sumFirst101Primes
    --       Question: Do you have to test that your answer is correct? How could this be checked?
    --       Answer: Testing can be done by creating the sets of 101 consecutive primes starting at the first prime (2).
    --               For each of these sets a unit test can be created, combining the set with if the set produces a prime or not.
    --               The goal of the unit test is to see if the sets that do not produce a prime, indeed do not produce a prime.
    --               And vise versa, the sets that do sum up to a prime, indeed produce a prime.


    -- Assignment 6
    putStrLn "\n==== ASSIGNMENT 6 ===="
    putStrLn "The smallest value of n is:"
    print testExpectedPrime


    -- Assignment 7
    -- Lists of card numbers which are either valid or invalid for each type.
    -- First value is card number, second value is whether it should be accepted
    -- as valid or not.
    putStrLn "\n==== ASSIGNMENT 7 ===="
    let americanExpressCardsTests = [(0000, False),
                                     (374643176033946, False),
                                     (37464317603394, False),
                                     (343417471228173, True),
                                     (374643176033947, True)]
    let masterCardsTests =  [(0000, False),
                            (343417471228173, False),
                            (374643176033947, False),
                            (5179107796975040, True),
                            (5163973029870953, True)]
    let visaCardsTests = [(0000, False),
                          (5179107796975040, False),
                          (5163973029870953, False),
                          (4230047476400119, True),
                          (4096932195561833, True)]

    if (exc7testAmericanExpressSet americanExpressCardsTests) == True
        then putStrLn "+++ OK, passed own American Express test."
        else putStrLn "--- ERROR, failed own American Express test."

    if (exc7testMasterSet masterCardsTests) == True
        then putStrLn "+++ OK, passed own Master test."
        else putStrLn "--- ERROR, failed own Master test."

    if (exc7testVisaSet visaCardsTests) == True
        then putStrLn "+++ OK, passed own Visa test."
        else putStrLn "--- ERROR, failed own Visa test."


    -- Assignment 8
    putStrLn "\n==== ASSIGNMENT 8 ===="
    putStrLn "Guilty:"
    print (head guilty)
    putStrLn "\nHonest:"
    print honest
