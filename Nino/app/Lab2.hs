module Lab2 where

import Data.Char
import Data.List
import Debug.Trace
import System.Random
import Test.QuickCheck 
import Data.Maybe
--import HelperCode

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- Lab 2 
-- Question 1 Counting the numbers in the quartiles
-- time: 3 h 21 m

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
                p <- getStdRandom random
                ps <- probs (n-1) 
                return (p:ps)

data Quartiles = Qt Int Int Int Int Int deriving (Show)

data QuartilesIncrement = IncQ1 | IncQ2 | IncQ3 | IncQ4 | IncQ5 

quartileAction :: Quartiles -> QuartilesIncrement -> Quartiles
quartileAction (Qt q1 q2 q3 q4 q5) IncQ1 = Qt (q1+1) q2 q3 q4 q5
quartileAction (Qt q1 q2 q3 q4 q5) IncQ2 = Qt q1 (q2+1) q3 q4 q5
quartileAction (Qt q1 q2 q3 q4 q5) IncQ3 = Qt q1 q2 (q3+1) q4 q5
quartileAction (Qt q1 q2 q3 q4 q5) IncQ4 = Qt q1 q2 q3 (q4+1) q5
quartileAction (Qt q1 q2 q3 q4 q5) IncQ5 = Qt q1 q2 q3 q4 (q5 + 1)

countQ :: Quartiles -> [Float] -> Quartiles
countQ qt [] = qt 
countQ qt (x:xs)
    | x > 0 && x < 0.25 = countQ (quartileAction qt IncQ1) xs
    | x >= 0.25 && x < 0.5 = countQ (quartileAction qt IncQ2) xs
    | x >= 0.5 && x < 0.75 = countQ (quartileAction qt IncQ3) xs
    | x >= 0.75 && x < 1 = countQ (quartileAction qt IncQ4) xs
    | otherwise = countQ (quartileAction qt IncQ5) xs --checks for another scenario


-- Question 2 Recognizing triangles
-- time: 1 h 44 m

data Shape = NotaTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a < 1 || b < 1 || c < 1 || (a + b) < c || (a + c) < b || (b + c) < a = NotaTriangle
    | a == b && a == c && b == c = Equilateral
    | a == b || a == c || b == c = Isosceles
    | (a^2 + b^2) == c^2 || (a^2 + c^2) == b^2 || (b^2 + c^2) == a^2 = Rectangular
    | otherwise = Other

istriangle :: Integer -> Integer -> Integer -> Shape -> Bool
istriangle a b c s = triangle a b c == s

printTriangleTypeIO :: Integer -> Integer -> Integer -> Shape -> IO()
printTriangleTypeIO a b c s = putStrLn (show a ++ " | " ++ show b ++ " | " ++ show c ++ " | " ++ show s ++ " : " ++ (show $ istriangle a b c s))


-- Question 3 Testing properties strength
-- 2 h 31 m
forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker xs p q = stronger xs q p

-- a) Implement all properties from the Exercise 3 from Workshop 2 as Haskell functions of type Int -> Bool. Consider a small domain like [(−10)..10].
e1, e2, e3, e4 :: Int -> Bool
e1 x =  even x && x > 3
e2 x = even x || x > 3
e3 x = (even x && x > 3) || even x
e4 x = (even x && x > 3) || even x

-- b) Provide a descending strength list of all the implemented properties.
-- compareProp (_,p) (_,q)
--     | stronger [(-10)..10] p q = LT
--     | weaker [(-10)..10] p q = GT
--     | otherwise = EQ


-- Question 4 Recognizing Permutations

-- Question 5 Recognizing and generating derangements

-- Question 6 Implementing and testing ROT13 encoding
-- time: 33 min

let2int :: Char -> Char -> Int
let2int c firstLetter = ord c - ord firstLetter

int2let :: Int -> Char -> Char
int2let n firstLetter = chr (ord firstLetter + n)

shift :: Int -> Char -> Char
shift n char 
    | isLower char = int2let (((let2int char 'a') + n) `mod` 26) 'a'
    | isUpper char = int2let (((let2int char 'A') + n) `mod` 26) 'A'
    | otherwise = char

encodeRot13 :: Int -> String -> String
encodeRot13 n xs = [shift n x | x <- xs]

printRot13 :: Int -> String -> IO()
printRot13 number secret = putStrLn (show $ encodeRot13 number secret)


-- Question 7 Implementing and testing IBAN validation
-- 4 h 13 min

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


