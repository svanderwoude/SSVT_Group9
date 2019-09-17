module Ass1 where
import Data.List
import HelperCodeLab2
import Test.QuickCheck
import System.IO.Unsafe



ranges :: [Float] -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)
ranges [] (a, b, c, d) = (a, b, c, d)
ranges probabilities (a, b, c, d)
  | head probabilities < 0.25 = ranges (tail probabilities) (a+1,b,c,d)
  | head probabilities < 0.5  = ranges (tail probabilities) (a,b+1,c,d)
  | head probabilities < 0.75 = ranges (tail probabilities) (a,b,c+1,d)
  | head probabilities < 1.0  = ranges (tail probabilities) (a,b,c,d+1)

countQuartiles amountOfValues = do
                                randomizedValues <- probs amountOfValues
                                let distributionOfRanges = ranges randomizedValues (0,0,0,0)
                                return distributionOfRanges


getPrecentage part total = 100 * part `div` total

checkWithinThreshold percentage thesholdPercentage
 | (percentage >= 25 - thesholdPercentage)  && (percentage <= 25 + thesholdPercentage) = True
 | otherwise = False

checkDifferenceRanges :: Int -> (Int,Int,Int,Int) -> Bool
checkDifferenceRanges threshold (a, b, c, d) = precentageA && precentageB && precentageC && precentageD
    where
    sumRanges = a + b + c + d
    precentageA = checkWithinThreshold (getPrecentage a sumRanges) threshold
    precentageB = checkWithinThreshold (getPrecentage b sumRanges) threshold
    precentageC = checkWithinThreshold (getPrecentage c sumRanges) threshold
    precentageD = checkWithinThreshold (getPrecentage d sumRanges) threshold


-- Positive Integer generator for tests
genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> suchThat (arbitrary :: Gen Int) (> 1000)

-- 5 as threshold, the higher the amount of integers that are generated the smaller the threshold can be, as
-- little numbers have less precission
testValidityQuartiles amountOfValues = checkDifferenceRanges 5 (unsafePerformIO(countQuartiles amountOfValues))

-- +++ OK, passed 100 tests.
-- Success {numTests = 100, labels = [], output = "+++ OK, passed 100 tests.\n"}
-- quickCheckResult $ forAll genPositiveIntegers testValidityQuartiles
