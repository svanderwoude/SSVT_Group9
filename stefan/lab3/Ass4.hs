-- LAB 3
-- Assignment 4
-- Time: 2 hours (so far)

module Ass4 where
import Data.List
import HelperCodeLab3
import System.Random
import Test.QuickCheck

-- Helper function to generate a random number
getRandNum :: IO Int
getRandNum = randomRIO (1, 6)

-- Since we have to deal with a lot of IO stuff, and we also want to be able to
-- generate both visual representations and parsed forms we will have to create
-- a "logic generator" which generates the input strings. We can then also create
-- the actual "form generator" by feeding the generated logic string into the
-- provided parse function

-- Logic generator
-- This method manages the logic generation by randomly selecting a operator to
-- generate in each call. Since we do not want to create endless formulas, a
-- limit parameter is also added to logicGenerator', which is defined in the
-- call from this function. We keep track of the current recursion depth using
-- the i parameter given in this function.
logicGenerator :: Int -> IO String
logicGenerator i = do
    n <- getRandNum
    logic <- logicGenerator' n i 10  -- Maximum recursion depth set to 10
    return logic

-- Generating the structure of the logic is done in this function. This is also
-- managed by a limiter k, with the value of i being the current recursion depth.
-- This value is also used for all properties in the current recursion depth.
logicGenerator' :: Int -> Int -> Int -> IO [Char]
logicGenerator' n i k
    | i <= k && n == 1 = return (show i)  -- Prop
    | i <= k && n == 2 = do
        logic <- logicGenerator (i + 1)
        return ("-" ++ logic)  -- Neg
    | i <= k && n == 3 = do
        logic1 <- logicGenerator (i + 1)
        logic2 <- logicGenerator (i + 1)
        return ("*(" ++ logic1 ++ " " ++ logic2 ++ ")")  -- Cnj
    | i <= k && n == 4 = do
        logic1 <- logicGenerator (i + 1)
        logic2 <- logicGenerator (i + 1)
        return ("+(" ++ logic1 ++ " " ++ logic2 ++ ")")  -- Dsj
    | i <= k && n == 5 = do
        logic1 <- logicGenerator (i + 1)
        logic2 <- logicGenerator (i + 1)
        return ("(" ++ logic1 ++ "==>" ++ logic2 ++ ")")  -- Impl
    | i == 0 = do
        logic1 <- logicGenerator (i + 1)
        logic2 <- logicGenerator (i + 1)
        return ("(" ++ logic1 ++ "<=>" ++ logic2 ++ ")")  -- Equiv (only once in first level)
    | otherwise = return (show i)

-- Form generator using logicGenerator
formGenerator :: IO Form
formGenerator = do
    logic <- logicGenerator 0
    let form = head (parse logic)
    return (form)
