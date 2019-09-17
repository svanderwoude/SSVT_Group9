module Ass3 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck


-- Excercise 3
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

data NamedFunction = NamedFunction String (Int -> Bool)

instance Show NamedFunction where show (NamedFunction name _) = name

e31 :: Int -> Bool
e31 x = even x && x > 3

e32 :: Int -> Bool
e32 x = even x || x > 3

e33 :: Int -> Bool
e33 x = (even x && x > 3) || even x

e34 :: Int -> Bool
e34 x = (even x && x > 3) || even x

e35 :: Int -> Bool
e35 = even

-- Compare properties by using `weaker` and `stronger` implementations for the [-10..10] domain
compareProps :: NamedFunction -> NamedFunction -> Ordering
compareProps (NamedFunction _ p) (NamedFunction _ q)
    | stronger [-10..10] p q = GT
    | weaker [-10..10] p q = LT
    | stronger [-10..10] p q && weaker [-10..10] p q = EQ

-- Create NamedFunction instances for every property
ex31 = NamedFunction "even x && x > 3" e31
ex32 = NamedFunction "even x || x > 3" e32
ex33 = NamedFunction "(even x && x > 3) || even x" e33
ex34 = NamedFunction "(even x && x > 3) || even x" e34
ex35 = NamedFunction "even" e35
