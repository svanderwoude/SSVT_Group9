-- LAB 2
-- Assignment 1
-- Time: 30 minutes

module Ass1 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck

data Quartile = Qt Int Int Int Int deriving Show
data QuartileCount = Q1 | Q2 | Q3 | Q4 

updateQuartileCount :: Quartile -> QuartileCount -> Quartile
updateQuartileCount (Qt q1 q2 q3 q4) Q1 = Qt (q1+1) q2 q3 q4
updateQuartileCount (Qt q1 q2 q3 q4) Q2 = Qt q1 (q2+1) q3 q4
updateQuartileCount (Qt q1 q2 q3 q4) Q3 = Qt q1 q2 (q3+1) q4
updateQuartileCount (Qt q1 q2 q3 q4) Q4 = Qt q1 q2 q3 (q4+1)

quartileCounter :: Quartile -> [Float] -> Quartile
quartileCounter qt [] = qt
quartileCounter qt (x:xs)
    | x < 0.25 = quartileCounter (updateQuartileCount qt Q1) xs
    | x < 0.5 = quartileCounter (updateQuartileCount qt Q2) xs
    | x < 0.75 = quartileCounter (updateQuartileCount qt Q3) xs
    | otherwise = quartileCounter (updateQuartileCount qt Q4) xs

quartileCounterResultIO n = do
    qt <- fmap (quartileCounter (Qt 0 0 0 0)) (probs n)
    putStrLn ("Quartiles: " ++ show qt)