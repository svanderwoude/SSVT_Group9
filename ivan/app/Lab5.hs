module Lab5 where
 
import Data.List
import System.Random
import Lecture5
import Test.QuickCheck
import System.IO.Unsafe

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
powersOf2 = [2^x | x <- [1..]]

maxPowerOf2 :: Integer -> Integer
maxPowerOf2 x = maximum $ takeWhile (<= x) powersOf2

repeatedlySquare :: Integer -> Integer -> Integer
repeatedlySquare x 1 = x
repeatedlySquare x y = repeatedlySquare (x^2) (y `div` 2) 

exM :: Integer -> Integer -> Integer -> Integer
exM x y m = let 
    y' = maxPowerOf2 y
    mod' = x `rem` m 
    in (repeatedlySquare mod' y') * (mod' ^ (y - y'))


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

quickCheckPrimeTestsF2 = quickCheckResult $ forAll genPositiveIntegers (\x -> ioProperty (primeTestsF 2 (composite!!x)))
