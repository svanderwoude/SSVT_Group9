-- LAB 2
-- Assignment 3
-- Time: 45 minutes

module Ass3 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

-- Exercise 3 properties
exc3EvenOnly :: Integer -> Bool
exc3EvenOnly x = even x

exc3FirstLeft :: Integer -> Bool
exc3FirstLeft x = even x && x > 3

exc3SecondLeft :: Integer -> Bool
exc3SecondLeft x = even x || x > 3

exc3ThirdLeft :: Integer -> Bool
exc3ThirdLeft x = (even x && x > 3) || even x

exc3FourthRight :: Integer -> Bool
exc3FourthRight x = (even x && x > 3) || even x

-- Create a list of properties from strong to weak
data DisplayFunction = DisplayFunction String (Integer -> Bool)

instance Show DisplayFunction where
    show (DisplayFunction name _) = name

compareProperties :: DisplayFunction -> DisplayFunction -> Ordering
compareProperties (DisplayFunction _ p) (DisplayFunction _ q)
    | stronger [(-10)..10] p q = GT
    | weaker [(-10)..10] p q = LT
    | otherwise = EQ

exc3_1 = DisplayFunction "even x && x > 3" exc3FirstLeft
exc3_2 = DisplayFunction "even x || x > 3" exc3SecondLeft
exc3_3 = DisplayFunction "(even x && x > 3) || even x" exc3ThirdLeft
exc3_4 = DisplayFunction "(even x && x > 3) || even x" exc3FourthRight
exc3_5 = DisplayFunction "even x" exc3EvenOnly
