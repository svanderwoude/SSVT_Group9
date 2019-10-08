module Lab5 where
 
import Data.List
import System.Random
import Lecture5
import Test.QuickCheck
import System.IO.Unsafe
import Data.Bits

{-
    A function to return the first element from a list which satisfies 
    monadic predicate. The function is copied from Control.Monad.Loops
    as I do not have the library locally.

    http://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html
-}
firstM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = do
        q <- p x
        if q
                then return (Just x)
                else firstM p xs


genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (>0)


-- Excercise 1 Time spent: 2h
{-
    The 2 functions exM' and exM are the implementation of the algorithm described in
    https://en.wikipedia.org/wiki/Modular_exponentiation#Pseudocode. We initially check
    whether the modulus is equal to 1 and if that is the case then the result is always 0.
    If the modulus is not equal to 1 then we call the exM' which is a recursive function
    that terminates when the exponent becomes 0. The algorithm is described in details at
    the provided link. 
-}
exM' :: Integer -> Integer -> Integer -> Integer -> Integer
exM' _ 0 _ result = result 
exM' base exponent modulus result = let 
    newExponent = shiftR exponent 1
    newBase = (base ^ 2) `mod` modulus
    newResult = if exponent `mod` 2 == 1 then (result * base) `mod` modulus else result
    in exM' newBase newExponent modulus newResult


exM :: Integer -> Integer -> Integer -> Integer
exM base exponent 1 = 0
exM base exponent modulus = exM' (base `mod` modulus) exponent modulus 1

{-
    We check our implementation with the implementation of expM which is the "naive" way
    of doing modular exponentiation. The results yielded should be the same but the performance
    of our implementation is significantly improved compared to expM.

    For example:
    base = 122293321
    exponent = 21312332
    modulus = 32133

    providing these values to expM gives as a result for 5.42sec while exM gives us the same
    result in 0.00sec (GHCi cannot measure such a low value). The result of both functions is 4723.
-}
exMProp :: Integer -> Integer -> Integer -> Bool
exMProp base exponent modulus = Lecture5.exM base exponent modulus == expM base exponent modulus

genPositiveIntegers2 :: Gen Integer
genPositiveIntegers2 = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

-- The test does not work because the generator above applies the check only to the first
-- generated value
quickCheckExM = quickCheckResult $ forAll genPositiveIntegers2 exMProp

-- Excercise 2 Time spent: 40mins

{-
    A composite number is a number that is not prime (meaning it has divisors
    different from 1 and itself).
-}
composite :: [Integer]
composite = [x | x <- [2..], not (prime x)]


{-
    If a number is composite, then it should have divisors different from 1
    and itself.
-}
compositeProp :: Integer -> Bool
compositeProp x = not $ null [d | d <- [2..(x-1)], x `mod` d == 0]


{-
    We can use QuickCheck to test the composite function by using a randomly
    generated integer from QuickCheck and using it as the element we want to
    retrieve from the output of composite. Meaning that if QuickCheck gives
    us the number 5, then we take the 5th element of the output of composite
    and then check if compositeProp holds for it. Since QuickCheck generates
    random numbers as well, we use the absolute value, so we do not have to
    create a custom generator.
-}
compositeTest = quickCheckResult $ forAll genPositiveIntegers (\x -> compositeProp (composite!!x))


-- Excercise 3 Time spent: 1h 15min
{-
    We create a function that allows us to pass the value of k as a parameter
    and creates a list of composite numbers for which the primeTestsF k n
    returns True
-}
findFirstFailF :: Int -> IO (Maybe Integer)
findFirstFailF k = firstM (primeTestsF k) composite

{-
    The functions below return the first element that fools the Fermat's
    primality test. Running them multiple times will highly likely produce
    different values for every run. However, we can see a trend, if we
    increase the value of k also the numbers that fool the test become higher.
    This means that by incrementing the value of k, the test becomes more
    robust, however it is never 100% correct and will always yield misleading
    results, especially with composite numbers.
-}
findFirstFailF1 :: IO (Maybe Integer)
findFirstFailF1 = findFirstFailF 1

findFirstFailF2 :: IO (Maybe Integer)
findFirstFailF2 = findFirstFailF 2

findFirstFailF3 :: IO (Maybe Integer)
findFirstFailF3 = findFirstFailF 3


{-
    We define QuickCheck tests using the same logic as in Excercise 2. Of course,
    all of them eventuall fail.
-}
quickCheckPrimeTestsF1 = quickCheckResult $ forAll genPositiveIntegers (\x -> ioProperty (primeTestsF 1 (composite!!x)))

quickCheckPrimeTestsF2 = quickCheckResult $ forAll genPositiveIntegers (\x -> ioProperty (primeTestsF 2 (composite!!x)))

quickCheckPrimeTestsF3 = quickCheckResult $ forAll genPositiveIntegers (\x -> ioProperty (primeTestsF 3 (composite!!x)))


--exM' :: Integer -> Integer -> Integer -> Integer -> Integer
-- exM' base 0     modulus result = result
-- exM' base expon modulus result | expon `mod` 2 == 1 = exM' newBase newExpon modulus ((result * base) `mod` modulus)
--                                | otherwise = exM' newBase newExpon modulus result
--                               where newBase = (base ^ 2) `mod` modulus
--                                     newExpon = shiftR expon 1

-- -- Base on the 'Right-to-left binary method' pseudocode in https://en.wikipedia.org/wiki/Modular_exponentiation

-- exM2 :: Integer -> Integer -> Integer -> Integer
-- exM2 base expon 1 = 0
-- exM2 base expon modulus = exM' (base `mod` modulus) expon modulus 1




-- base := base mod modulus
-- while exponent > 0
--     if (exponent mod 2 == 1):
--         result := (result * base) mod modulus
--     exponent := exponent >> 1
--     base := (base * base) mod modulus

