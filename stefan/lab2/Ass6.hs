-- LAB 2
-- Assignment 6
-- Time: 30 minutes (+ TODO testing)

module Ass6 where
import Data.List
import Data.Maybe
import HelperCodeLab2
import Test.QuickCheck

-- Implements the ROT13 algorithm
rot13 :: String -> String
rot13 x = concatMap (\y -> [rot13CharConverter y]) x

-- Individual character converter according to ROT13 for lower and uppercase
-- characters.
rot13CharConverter :: Char -> Char
rot13CharConverter x
    | elem x ['a'..'z'] = do
        let chars = ['a'..'z']
        let i = fromMaybe 0 (elemIndex x chars)
        if i + 13 < 26 then chars!!(i+13) else chars!!(i-13)
    | elem x ['A'..'Z'] = do
        let chars = ['A'..'Z']
        let i = fromMaybe 0 (elemIndex x chars)
        if i + 13 < 26 then chars!!(i+13) else chars!!(i-13)
    | otherwise = x
