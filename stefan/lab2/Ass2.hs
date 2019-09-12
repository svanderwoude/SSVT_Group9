-- LAB 2
-- Assignment 2
-- Time: 30 minutes (+ TODO testing)

module Ass2 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a == 0 || b == 0 || c == 0 = NoTriangle
    | a == b && b == c = Equilateral
    | a == b || b == c || a == c = Isosceles
    | a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a = Rectangular
    | otherwise = Other

