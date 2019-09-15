-- LAB 2
-- Assignment 2
-- Time: 45 minutes

module Ass2 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

-- Returns the type of triangle (if any) of the provided sides a b c
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a < 1 || b < 1 || c < 1 = NoTriangle
    | a + b < c || a + c < b || b + c < a = NoTriangle
    | a == b && b == c = Equilateral
    | a == b || b == c || a == c = Isosceles
    | a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a = Rectangular
    | otherwise = Other

-- Test whether the provided sides a b c return the expected triangle type
testTriangleType :: Integer -> Integer -> Integer -> Shape -> Bool
testTriangleType a b c s = triangle a b c == s

-- IO version for testing results
testTriangleTypeIO :: Integer -> Integer -> Integer -> Shape -> IO()
testTriangleTypeIO a b c s = putStrLn (show a ++ ", " ++ show b ++ ", " ++ show c ++ " is " ++ show s ++ ": " ++ (show $ testTriangleType a b c s))
