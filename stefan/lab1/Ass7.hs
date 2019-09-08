-- LAB 1
-- Assignment 7
-- Time: 30 minutes +

module Ass7 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Implements the Luhn algorithm
luhn :: Integer -> Integer
luhn n = sum (calculateDigitScores (doubleAllValues (filterEvenNumbers (doubleAllValues (convertToDigits (reversal n))))))

-- Calculates the digit scores
calculateDigitScores :: [Integer] -> [Integer]
calculateDigitScores list = [x - 9 | x <- list, x > 9]

-- Conerts a large integer to a list of digits
convertToDigits :: Integer -> [Integer]
convertToDigits 0 = []
convertToDigits x = convertToDigits (div x 10) ++ [mod x 10]

-- Doubles all values in a list
doubleAllValues :: [Integer] -> [Integer]
doubleAllValues [] = []
doubleAllValues list = [x*2 | x <- list]

-- Converts a list to a list using only the even indices of the original list
-- counting from 1 (so every second element)
filterEvenNumbers :: [Integer] -> [Integer]
filterEvenNumbers [] = []
filterEvenNumbers [x] = []
filterEvenNumbers (x:y:xs) = y : filterEvenNumbers xs

-- Check if the provided credit card number is a valid American Express number
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = False


-- Check if the provided credit card number is a valid MasterCard number
isMaster :: Integer -> Bool
isMaster n = False


-- Check if the provided credit card number is a valid Visa number
isVisa :: Integer -> Bool
isVisa n = False
