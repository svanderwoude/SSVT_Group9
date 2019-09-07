module Lab1 where
import Data.List
import Test.QuickCheck 

-- Common
genIntegersLargerThan1 :: Gen Integer
genIntegersLargerThan1 = abs <$> (arbitrary :: Gen Integer) `suchThat` (>1)

fac :: (Integral a) => a -> a
fac n = product [1..n]

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]


-- Excercise 1
exc1n1 :: Integer -> Integer
exc1n1 n = sum [ x ^ 2 | x <- [1..n] ]

exc1n2 :: Integer -> Integer
exc1n2 n = (n * (n + 1) * (2*n + 1)) `div` 6

exc1testN :: Integer -> Bool
exc1testN n = exc1n1 n == exc1n2 n

exc1quickCheckTestN = quickCheckResult $ forAll genIntegersLargerThan1 exc1testN

exc1n3 :: Integer -> Integer
exc1n3 n = sum [ x^3 | x <- [1..n] ]

exc1n4 :: Integer -> Integer
exc1n4 n = (n * (n + 1) `div` 2) ^ 2

exc1testN2 :: Integer -> Bool
exc1testN2 n = exc1n3 n == exc1n4 n

exc1quickCheckTestN2 = quickCheckResult $ forAll genIntegersLargerThan1 exc1testN2


-- Excercise 2
exc2PA :: Integer -> [[Integer]]
exc2PA n = subsequences [1..n]

exc2testPA :: Integer -> Bool
exc2testPA n = length (exc2PA n) == 2 ^ n

exc2quickCheckTestPA = quickCheckResult $ forAll genIntegersLargerThan1 exc2testPA


-- Excercise 3
exc3perm :: Integer -> [[Integer]]
exc3perm n = permutations [1..n]

exc3testPerm :: Integer -> Bool
exc3testPerm n = toInteger (length $ exc3perm n) == fac n

exc3quickCheckTestPerm = quickCheckResult $ forAll genIntegersLargerThan1 exc3testPerm


-- Excercise 4
reversal :: Integer -> Integer
reversal = read . reverse . show

exc4PrimeReversal :: [Integer]
exc4PrimeReversal = [ x | x <- [1..10000], prime x, prime $ reversal x ]

-- TODO: figure out the testing part


-- Excercise 5
exc5sumFirst101Primes :: Integer
exc5sumFirst101Primes = sum $ take 101 primes

-- TODO: testing


-- Excercise 6
exc6toRefute :: Integer -> Integer
exc6toRefute n = product (take (fromIntegral n) primes) + 1

-- generate counter examples for the above statement
exc6counterExamples :: [Integer]
exc6counterExamples = filter (not . prime) (map exc6toRefute [1..])

-- find the minimum prime for which the above statement is false
exc6minPrimeToRefute = head exc6counterExamples 

-- proveWrong = quickCheckResult $ forAll genIntegersLargerThan1 


-- Excercise 7
digits :: Integer -> [Int]
digits = map (read . (:[])) . show

-- applies a function to every second element of a list
applyToEverySecondElement :: (a -> a) -> [a] -> [a]
applyToEverySecondElement f (x:s:xs) = x: f s : applyToEverySecondElement f xs
applyToEverySecondElement f xs = xs

-- Read the digits of a number, reverse them and then multiply every other digit by 2. If the resulting number is bigger than 9, the subtract 9 from it
exc7modifyEveryOtherElement :: Integer -> [Int]
exc7modifyEveryOtherElement n = map (\x -> if x > 9 then x - 9 else x) $ applyToEverySecondElement (*2) (reverse $ digits n)

-- Check if the sum of all digits end with 0
exc7luhn :: Integer -> Bool
exc7luhn n =  sum (exc7modifyEveryOtherElement n) `mod` 10 == 0

exc7checkNumDigits :: Integer -> Int -> Bool
exc7checkNumDigits n a = length (digits n) == a

exc7startsWith :: Integer -> Integer -> Bool
exc7startsWith n a = digits a `isPrefixOf` digits n

-- Checks if Luhn is True and whether the number is 15 digits and starts with either 37 or 34
exc7isAmericanExpress :: Integer -> Bool
exc7isAmericanExpress n = exc7luhn n && exc7checkNumDigits n 15 && (exc7startsWith n 37 || exc7startsWith n 34) 

-- Checks if Luhn is True and whether the number is 16 digits and starts with 5
exc7isMaster :: Integer -> Bool
exc7isMaster n = exc7luhn n && exc7checkNumDigits n 16 && exc7startsWith n 5

-- Checks if Luhn is True and whether the number is either 16 or 13 digits and starts with 4
exc7isVisa :: Integer -> Bool
exc7isVisa n = exc7luhn n && (exc7checkNumDigits n 16 || exc7checkNumDigits n 13) && exc7startsWith n 4

-- Excercise 8
data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
 
boys = [Matthew, Peter, Jack, Arnold, Carl]
