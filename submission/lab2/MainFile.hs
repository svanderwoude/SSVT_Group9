module MainFile where
import HelperCodeLab2
import Test.QuickCheck
import Ass1
import Ass2
import Ass3
import Ass4
import Ass5
import Ass6
import Ass7



main = do
       putStrLn "==== ASSIGNMENT 1 ===="
       quickCheckResult $ forAll genPositiveIntegers testValidityQuartiles


       putStrLn "==== ASSIGNMENT 2 ===="
       triangleTests 3 4 5 Rectangular
       triangleTests 1 1 1 Equilateral
       triangleTests 2 2 3 Isosceles
       triangleTests 10 1 1 NoTriangle
       triangleTests 2 4 3 Other

       putStrLn  "==== ASSIGNMENT 3 ===="
       print orderProps

       putStrLn  "==== ASSIGNMENT 4 ===="
       putStrLn  "For the properties see assigment 4 file"
       -- quickCheckPermutations = quickCheckResult (\(RandomIntListSmall xs) -> let list = nub xs in all (permutationProperties xs) (permutations xs))
       quickCheckPermutations

       putStrLn  "==== ASSIGNMENT 5 ===="
       quickCheckIsDerangement

       putStrLn  "==== ASSIGNMENT 6 ===="


       putStrLn  "==== ASSIGNMENT 7 ===="
