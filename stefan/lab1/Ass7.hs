-- LAB 1
-- Assignment 7
-- Time: 1 hour and 15 minutes (TODO update with test)
-- Source used for specific cards: https://en.wikipedia.org/wiki/Payment_card_number#Issuer_identification_number_(IIN)

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
-- American Express card numbers are 15 numbers long and start with 34 or 37
isAmericanExpress :: Integer -> Bool
isAmericanExpress n = do
    let digits = convertToDigits n
    let validCard = luhn n
    validCard && (length digits == 15) && (isAmericanExpressStart digits)

isAmericanExpressStart :: [Integer] -> Bool
isAmericanExpressStart (3:y:xs) = elem y [4, 7]
isAmericanExpressStart xs = False

-- Check if the provided credit card number is a valid MasterCard number
-- Master card numbers are 16 numbers long and start with 51, 52, 53, 54 or 55
isMaster :: Integer -> Bool
isMaster n = do
    let digits = convertToDigits n
    let validCard = luhn n
    validCard && (length digits == 16) && (isMasterStart digits)

isMasterStart :: [Integer] -> Bool
isMasterStart (5:y:xs) = elem y [1, 2, 3, 4, 5]
isMasterStart (2:y:xs) = False  -- TODO add check
isMasterStart xs = False

-- Check if the provided credit card number is a valid Visa number
-- Visa card numbers are 16 digits long and start with 4
isVisa :: Integer -> Bool
isVisa n = do
    let digits = convertToDigits n
    let validCard = luhn n
    validCard && (length digits == 16) && (head digits == 4)

-- Test if the Luhn implementation returns the expected result
testLuhnCorrectness :: Integer -> Bool -> Bool
testLuhnCorrectness n v = v == luhn n
