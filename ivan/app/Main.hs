module Main where

import Lib
import Lab1
import Test.QuickCheck

main :: IO ()
main = do
    putStrLn "bla"
    quickCheckResult testN
