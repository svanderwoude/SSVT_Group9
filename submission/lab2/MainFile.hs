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

       putStrLn "\n==== ASSIGNMENT 2 ===="
       triangleTests 3 4 5 Rectangular
       triangleTests 1 1 1 Equilateral
       triangleTests 2 2 3 Isosceles
       triangleTests 10 1 1 NoTriangle
       triangleTests 2 4 3 Other

       putStrLn "\n==== ASSIGNMENT 3 ===="
       print orderProps
       quickCheckPermutations

       putStrLn "\n==== ASSIGNMENT 4 ===="
       quickCheckPermutations

       putStrLn "\n==== ASSIGNMENT 5 ===="
       quickCheckIsDerangement

       putStrLn "\n==== ASSIGNMENT 6 ===="
       quickCheckRot13

       putStrLn "\n==== ASSIGNMENT 7 ===="
       printvalidateIBAN "GB82 WEST 1234 5698 7654 32"
       printvalidateIBAN "GB82 WEST 1234 5698 7654 3"

       putStrLn "\nTesting valid IBAN numbers"
       ibanCheckerValidList

       putStrLn "\nTesting invalid IBAN numbers"
       ibanCheckerInvalidList
