module Lab2 where
 
import Data.List
import Data.Char
import Data.Maybe
import System.Random
import Test.QuickCheck  
    

infix 1 --> 
 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Excercise 1
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n-1) 
            return (p:ps)

data Quartiles = Qt Int Int Int Int deriving Show

data QuartilesUpdate = IncrementQ1 | IncrementQ2 | IncrementQ3 | IncrementQ4 

quartileAction :: Quartiles -> QuartilesUpdate -> Quartiles
quartileAction (Qt q1 q2 q3 q4) IncrementQ1 = Qt (q1+1) q2 q3 q4
quartileAction (Qt q1 q2 q3 q4) IncrementQ2 = Qt q1 (q2+1) q3 q4
quartileAction (Qt q1 q2 q3 q4) IncrementQ3 = Qt q1 q2 (q3+1) q4
quartileAction (Qt q1 q2 q3 q4) IncrementQ4 = Qt q1 q2 q3 (q4+1)

countQuartiles :: Quartiles -> [Float] -> Quartiles
countQuartiles qt [] = qt 
countQuartiles qt (x:xs)
    | x > 0 && x < 0.25 = countQuartiles (quartileAction qt IncrementQ1) xs
    | x >= 0.25 && x < 0.5 = countQuartiles (quartileAction qt IncrementQ2) xs
    | x >= 0.5 && x < 0.75 = countQuartiles (quartileAction qt IncrementQ3) xs
    | otherwise = countQuartiles (quartileAction qt IncrementQ4) xs

getResults = do
             x <- fmap (countQuartiles (Qt 0 0 0 0)) (probs 10000)
             print x
-- TODO: how to implement "roughly equal"


-- Excercise 2
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

-- Order the properties starting from the strongest one and going to the weakest one
-- Output: [even x && x > 3,
--          (even x && x > 3) || even x,
--          (even x && x > 3) || even x,
--          even,
--          even x || x > 3]
orderProps = sortBy (flip compareProps) [ex31, ex32, ex33, ex34, ex35]


-- Excercise 4
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (`elem` xs) ys && all (`elem` ys) xs

-- Having the pre-condition of no duplicates in the input, means we test less in the
-- implementation of the function
testIsPermutation :: [Int] -> [Int] -> IO ()
testIsPermutation xs ys = putStrLn ("Testing whether " ++ show xs ++ " and " ++ show ys ++ " are permutations of each other: " ++ show (isPermutation xs ys))

testRel :: (a -> a -> Bool) -> (a -> a) -> [a] -> Bool
testRel spec f = all (\x -> spec x (f x))

--permP1 = testRel (\xs ys -> length xs == length ys) isPermutation

isPermutationTests = do
    testIsPermutation [1,2,3] [3,2,1]
    testIsPermutation [3,1,1] [3,2,1] -- check whether duplicates in the first list are detected
    testIsPermutation [1,2,3] [3,1,1] -- check whether duplicates in the second list are detected
    testIsPermutation [1,2,3] [4,3,2,1] -- check differring lengths
    testIsPermutation [1,2,3,4] [3,2,1] -- check dffering lengths

-- TODO: properties

-- Excercise 5
isDerangement :: [Int] -> [Int] -> Bool
isDerangement xs ys = isPermutation xs ys && all (\x -> elemIndex x xs /= elemIndex x ys) xs

deran :: Int -> [[Int]]
deran n = [x | x <- permutations [1..n-1], isDerangement x [1..n-1]]

-- Excercise 6
-- ROT13(ROT13(x)) = x for any letter in Latin alphabet

az = ['a'..'z']

aZ = ['A'..'Z']

rot13map :: Char -> Char
rot13map x
    | x `elem` az = let i = fromJust $ x `elemIndex` az in if i+13<26 then az!!(i+13) else az!!(i-13) 
    | x `elem` aZ = let i = fromJust $ x `elemIndex` aZ in if i+13<26 then aZ!!(i+13) else aZ!!(i-13)
    | otherwise = x

rot13 :: String -> String
rot13 = map rot13map

-- Excercise 7
charMap :: Char -> Int
charMap x = fromJust (x `elemIndex` aZ) + 10

headToEnd :: [a] -> [a]
headToEnd []     = []
headToEnd (x:xs) = xs ++ [x]

moveToBack :: Int -> String -> String
moveToBack 0 xs = xs
moveToBack n xs = moveToBack (n-1) (headToEnd xs)

-- Moves the block of first 4 characters to the back of the list
ibanPushToBack :: String -> String
ibanPushToBack = moveToBack 4

-- Replaces every letter with the corresponding number (A -> 10, B -> 11 etc.)
ibanReplaceLetters :: String -> String
ibanReplaceLetters = concatMap (\x -> if x `elem` aZ then show (charMap x) else [x])

-- Check if the IBAN is valid. It does not check for IBAN length since it is different per country code.
-- The maximum length that is allowed is 34, therefore we check only this.
ibanValidate :: String -> Bool
ibanValidate iban = read (ibanReplaceLetters (ibanPushToBack iban)) `mod` 97 && length iban < 35 == 1