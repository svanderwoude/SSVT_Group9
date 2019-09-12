-- LAB 2
-- Assignment 3
-- Time: 15 minutes (+ TODO part b)

module Ass3 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

-- Exercise 3 properties
exc3EvenOnly :: Integer -> Bool
exc3EvenOnly x = even x

exc3FirstLeft :: Integer -> Bool
exc3FirstLeft x = even x || x > 3

exc3SecondLeft :: Integer -> Bool
exc3SecondLeft x = even x || x > 3

exc3ThirdLeft :: Integer -> Bool
exc3ThirdLeft x = (even x && x > 3) || even x

exc3FourthRight :: Integer -> Bool
exc3FourthRight x = (even x && x > 3) || even x

-- Create a list of properties from strong to weak
