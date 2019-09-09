module Ass5 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

exc5primes :: Integer -> [Integer]
exc5primes x = take 101 $ filter prime [x..]

-- create a of sums of 101 consecutive primes starting from a different prime and filter only the results
-- where the sum is also prime, then take the head
exc5sumFirst101Primes :: Integer
exc5sumFirst101Primes = head ( filter prime $ map (sum . exc5primes) primes )
