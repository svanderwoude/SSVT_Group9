module HelperCodeLab1 where
import Data.List
import Test.QuickCheck    

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

-- Positive Integer generator for tests
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Integer) (> 0)

-- Small Positive Integer generator for tests
genSmallPositiveIntegers :: Gen Integer
genSmallPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Integer) (\x -> x > 0 && x < 24)

-- Tiny Positive Integer generator for tests
genTinyPositiveIntegers :: Gen Integer
genTinyPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Integer) (\x -> x > 0 && x < 8)
