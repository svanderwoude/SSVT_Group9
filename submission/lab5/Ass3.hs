module Ass3 where 
import Ass2
import Lecture5
import Test.QuickCheck

-- Excercise 3 Time spent: 1h 15min
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