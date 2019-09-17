-- LAB 2
-- Assignment 7
-- Time: 1h 30 minutes

module Ass7 where
import Data.Char
import Data.List
import HelperCodeLab2
import Test.QuickCheck

ibanLength :: String -> Integer
ibanLength "AL" = 28
ibanLength "AD" = 24
ibanLength "AT" = 20
ibanLength "AZ" = 28
ibanLength "BH" = 22
ibanLength "BY" = 28
ibanLength "BE" = 16
ibanLength "BA" = 20
ibanLength "BR" = 29
ibanLength "BG" = 22
ibanLength "CR" = 22
ibanLength "HR" = 21
ibanLength "CY" = 28
ibanLength "CZ" = 24
ibanLength "DK" = 18
ibanLength "DO" = 28
ibanLength "TL" = 23
ibanLength "EE" = 20
ibanLength "FO" = 18
ibanLength "FI" = 18
ibanLength "FR" = 27
ibanLength "GE" = 22
ibanLength "DE" = 22
ibanLength "GI" = 23
ibanLength "GR" = 27
ibanLength "GL" = 18
ibanLength "GT" = 28
ibanLength "HU" = 28
ibanLength "IS" = 26
ibanLength "IQ" = 23
ibanLength "IE" = 22
ibanLength "IL" = 23
ibanLength "IT" = 27
ibanLength "JO" = 30
ibanLength "KZ" = 20
ibanLength "XK" = 20
ibanLength "KW" = 30
ibanLength "LV" = 21
ibanLength "LB" = 28
ibanLength "LI" = 21
ibanLength "LT" = 20
ibanLength "LU" = 20
ibanLength "MK" = 19
ibanLength "MT" = 31
ibanLength "MR" = 27
ibanLength "MU" = 30
ibanLength "MC" = 27
ibanLength "MD" = 24
ibanLength "ME" = 22
ibanLength "NL" = 18
ibanLength "NO" = 15
ibanLength "PK" = 24
ibanLength "PS" = 29
ibanLength "PL" = 28
ibanLength "PT" = 25
ibanLength "QA" = 29
ibanLength "RO" = 24
ibanLength "LC" = 32
ibanLength "SM" = 27
ibanLength "SA" = 24
ibanLength "RS" = 22
ibanLength "SC" = 31
ibanLength "SK" = 24
ibanLength "SI" = 19
ibanLength "ES" = 24
ibanLength "SE" = 24
ibanLength "CH" = 21
ibanLength "TN" = 24
ibanLength "TR" = 26
ibanLength "AE" = 23
ibanLength "GB" = 22
ibanLength "VA" = 22
ibanLength "VG" = 24
ibanLength _ = 0

checkIbanLength :: String -> Bool
checkIbanLength x = genericLength x == ibanLength ([x!!0, x!!1])

checkRemainder :: [Integer] -> Integer
checkRemainder x = mod (combine x) 97

combine :: [Integer] -> Integer
combine = read . concatMap show

rotateCharacters :: String -> String
rotateCharacters (a:b:c:d:x) = x ++ [a] ++ [b] ++ [c] ++ [d]

replaceLetters :: String -> [Integer]
replaceLetters x = [replaceLetters' y | y <- x]

replaceLetters' :: Char -> Integer
replaceLetters' c
    | c >= 'A' && c <= 'Z' = toInteger ((ord c) - (ord 'A') + 10)
    | otherwise = toInteger (digitToInt c)

iban :: String -> Bool
iban x = checkIbanLength x && checkRemainder (replaceLetters (rotateCharacters x)) == 1

-- Tests
ibanCheckerResult :: [String] -> Bool -> Bool
ibanCheckerResult [] _ = True
ibanCheckerResult xs e = all (==e) [iban x | x <- xs]

-- Test list of valid IBAN numbers
ibanCheckerValidList = do
    content <- readFile "valid_iban.txt"
    let asExpected = ibanCheckerResult (lines content) True

    if asExpected == True then
        putStrLn ("+++ OK, passed own tests")
    else
        putStrLn ("--- ERROR, failed own tests")

-- Test list of invalid IBAN numbers
ibanCheckerInvalidList = do
    content <- readFile "invalid_iban.txt"
    let asExpected = ibanCheckerResult (lines content) False

    if asExpected == True then
        putStrLn ("+++ OK, passed own tests")
    else
        putStrLn ("--- ERROR, failed own tests")
