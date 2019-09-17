module Ass2 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

data Shape = NoTriangle | Equilateral
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | a+b < c || a+c < b || b+c < a = NoTriangle
    | a == b && b == c = Equilateral
    | (a == b) `xor` (b == c) `xor` (a == c) = Isosceles
    | a^2 + b^2 == c^2 || a^2 + c^2 == b^2 || b^2 + c^2 == a^2 = Rectangular
    | otherwise = Other

checkTriangle :: Integer -> Integer -> Integer -> Shape -> Bool
checkTriangle a b c shape = (triangle a b c) == shape

triangleTests :: Integer -> Integer -> Integer -> Shape -> IO ()
triangleTests a b c shape = putStrLn ("Test whether " ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ " is " ++ show shape ++ ": " ++ (show $ checkTriangle a b c shape))

testTriangle :: IO ()
testTriangle = do
    triangleTests 3 4 5 Rectangular
    triangleTests 1 1 1 Equilateral
    triangleTests 2 2 3 Isosceles
    triangleTests 10 1 1 NoTriangle
    triangleTests 2 4 3 Other
