module Ass7 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

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

-- Tests
exc7testAmericanExpress :: (Integer, Bool) -> Bool
exc7testAmericanExpress (n, e) = (exc7isAmericanExpress n) == e

exc7testAmericanExpressSet :: [(Integer, Bool)] -> Bool
exc7testAmericanExpressSet xs = all exc7testAmericanExpress xs

exc7testMaster :: (Integer, Bool) -> Bool
exc7testMaster (n, e) = (exc7isMaster n) == e

exc7testMasterSet :: [(Integer, Bool)] -> Bool
exc7testMasterSet xs = all exc7testMaster xs

exc7testVisa :: (Integer, Bool) -> Bool
exc7testVisa (n, e) = (exc7isVisa n) == e

exc7testVisaSet :: [(Integer, Bool)] -> Bool
exc7testVisaSet xs = all exc7testVisa xs
