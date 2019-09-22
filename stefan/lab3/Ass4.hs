-- LAB 3
-- Assignment 4
-- Time: 1 hour (so far)

module Ass4 where
import Control.Monad
import Data.List
import HelperCodeLab3
import System.Random
import Test.QuickCheck

-- Form generator
formGenerator :: [Form]
formGenerator = [f | (f,_) <- parseForm formGenerator']

formGenerator' :: [Token]
formGenerator' = formGeneratorSection

formGenerator'' :: String -> [Token]
formGenerator'' "prop" = [TokenInt 1]
formGenerator'' "neg" = [TokenNeg] ++ (formGeneratorSection)
formGenerator'' "cnj" = [TokenCnj, TokenOP] ++ (formGeneratorSection) ++ [TokenCP]
formGenerator'' "dsj" = [TokenDsj, TokenOP] ++ (formGeneratorSection) ++ [TokenCP]
formGenerator'' "impl" = [TokenOP] ++ (formGeneratorSection) ++ [TokenImpl] ++ (formGeneratorSection) ++ [TokenCP]
formGenerator'' "equiv" = [TokenOP] ++ (formGeneratorSection) ++ [TokenEquiv] ++ (formGeneratorSection) ++ [TokenCP]

formGeneratorSection :: [Token]
formGeneratorSection = do
    let l = ["prop", "neg"]
    i <- (l !!) <$> randomRIO (0, length l - 1)
    formGenerator' i

-- formGeneratorSection' :: Int -> [Token]
-- formGeneratorSection' n
--     | n == 1 = formGenerator'' "cnj"
--     | n == 2 = formGenerator'' "neg"
--     | otherwise = formGenerator'' "prop"
