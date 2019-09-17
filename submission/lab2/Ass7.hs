module Ass7 where
import HelperCodeLab2
import Data.Char
import Data.List
import Debug.Trace
import System.Random
import Test.QuickCheck 
import Data.Maybe

-- Question 7 Implementing and testing IBAN validation

ibanValidation :: String -> Bool
ibanValidation iban = validateIBANCountry iban && ibanValidateDigits iban

ibanValidateDigits :: String -> Bool
ibanValidateDigits  xs = 
    case xs of 
    [] -> False
    xs -> checkDigits
    where
        -- remove blanks and make all letters uppercase
        clean = map toUpper $ concat $ words xs
        -- Allowed Digits
        digits = ['0'..'9']
        -- Allowed Letters
        letters = ['A'..'Z']
        -- Replace letters with corresponding number
        replDigits = zip letters $ map show [10..35]
        -- All the digits and letters allowed in the IBAN number
        validDigits = digits ++ letters
        -- see if all digits and letters in the IBAN number are allowed
        allowed = all (`elem` validDigits) clean
        -- take the first 4 digits from the number and put them at its end
        (p1, p2) = splitAt 4 clean
        p3 = p2 ++ p1
        -- convert the letters to numbers and
        -- convert the result to an integer
        p4 :: Integer
        p4 = read $ concat $ map convertLetter p3
        convertLetter x
            | x `elem` digits = [x]
            | otherwise = fromJust $ lookup x replDigits
        -- check if the number is valid
        checkDigits = if allowed
                    then if p4 `mod` 97 == 1
                            then True
                            else False -- "Validation failed."
                    else False -- "Number contains illegal digits."

validateIBANCountry :: String -> Bool
validateIBANCountry [] = False
validateIBANCountry xs =
    case searchCountry of
        Nothing -> False
        Just l  -> if length clean /= l
                        then False
                        else True
    where
        -- remove blanks and make all letters uppercase
        clean = map toUpper $ concat $ words xs
        -- get the country code
        country = take 2 clean
        -- search number length
        searchCountry = lookup country countries
        countries :: [(String, Int)]
        countries = zip (words "AL AT BE BA BG HR CZ DO FO FR DE GR GT \
            \IS IL KZ LV LI LU MT MU MD NL PK PL RO SA SK ES CH TR GB \
            \AD AZ BH BR CR CY DK EE FI GE GI GL HU IE IT KW LB LT MK \
            \MR MC ME NO PS PT SM RS SI SE TN AE VG")
            [28,20,16,20,22,21,24,28,18,27,22,27,28,26,23,20,21,21,20,
            31,30,24,18,24,28,24,24,24,24,21,26,22,24,28,22,29,21,28,18,
            20,18,22,23,18,28,22,27,30,28,20,19,27,27,22,15,29,25,27,22,
            19,24,24,23,24]


printvalidateIBAN :: String -> IO()
printvalidateIBAN iban = putStrLn (show iban ++ " " ++ (show $ ibanValidation iban))

-- Tests
ibanCheckerResult :: [String] -> Bool -> Bool
ibanCheckerResult [] _ = True
ibanCheckerResult xs e = all (==e) [ibanValidation x | x <- xs]

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