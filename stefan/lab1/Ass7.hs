-- LAB 1
-- Assignment 7
-- Time: 1 hour (TODO update with test)

module Ass7 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Implements the Luhn algorithm, returns if the provided card number is valid
luhn :: Integer -> Bool
luhn n = (mod (sum (luhnCalculateValues n)) 10) == 0

-- Calculates the sum of all digits according to the Luhn algorithm steps
luhnCalculateValues :: Integer -> [Integer]
luhnCalculateValues n = calculateDigitScores (doubleEvenNumbers (reverse (convertToDigits n)))

-- Calculates the digit scores
calculateDigitScores :: [Integer] -> [Integer]
calculateDigitScores list = [if x > 9 then x - 9 else x | x <- list]

-- Conerts a large integer to a list of digits
convertToDigits :: Integer -> [Integer]
convertToDigits 0 = []
convertToDigits x = convertToDigits (div x 10) ++ [mod x 10]

-- Converts a list to a list using only the even indices of the original list
-- counting from 1 (so every second element)
doubleEvenNumbers :: [Integer] -> [Integer]
doubleEvenNumbers [] = []
doubleEvenNumbers [x] = [x]
doubleEvenNumbers (x:y:xs) = x : (y*2) : doubleEvenNumbers xs

-- Check if the provided credit card number is a valid American Express number
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = do
    let digits = convertToDigits n
    let validCard = luhn n
    validCard && (head digits == 3) && (length digits == 15)


-- Check if the provided credit card number is a valid MasterCard number
isMaster :: Integer -> Bool
isMaster n = do
    let digits = convertToDigits n
    let validCard = luhn n
    validCard && (head digits == 5) && (length digits == 16)


-- Check if the provided credit card number is a valid Visa number
isVisa :: Integer -> Bool
isVisa n = do
    let digits = convertToDigits n
    let validCard = luhn n
    validCard && (head digits == 4) && (length digits == 16)

-- Test
-- TODO
