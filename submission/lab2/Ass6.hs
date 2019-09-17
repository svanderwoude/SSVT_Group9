-- Specification:
-- (basic) ROT13 takes a string with letters in a..z or A..Z as input and shifts
-- each letter 13 places to the right in the alphabet, whilst keeping its
-- original case and wrapping around if the end (z/Z) is reached.

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

-- Properties
inAlphabetProperty :: String -> Bool
inAlphabetProperty x = all (\y -> elem y ['A'..'Z'] || elem y ['a'..'z']) x

willLoopProperty :: String -> Bool
willLoopProperty x = x == (rot13 (rot13 x))

-- Combined properties
rot13Properties :: String -> Bool
rot13Properties x = inAlphabetProperty x && willLoopProperty x

data RandomString = RandomString String deriving Show
instance Arbitrary RandomString where
    arbitrary = fmap RandomString (listOf $ elements (['a'..'z'] ++ ['A'..'Z']))

-- Generate a random string and verify the properties
quickCheckRot13 = quickCheckResult (\(RandomString x) -> rot13Properties x)
