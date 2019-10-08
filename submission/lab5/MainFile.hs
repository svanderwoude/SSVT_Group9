module MainFile where
import Ass1
import Ass2
import Ass3
-- import Ass4
-- import Ass5
import Ass6
import Data.List
import Test.QuickCheck

main = do
    putStrLn "\n==== ASSIGNMENT 1 ===="


    putStrLn "\n==== ASSIGNMENT 2 ===="
    putStrLn "Testing composite implementation:"
    compositeTest

    putStrLn "\n==== ASSIGNMENT 3 ===="
    putStrLn "Find first composite number to fail primeTestsF for k=1:"
    f1Fail <- findFirstFailF1
    print f1Fail
    putStrLn "\nFind first composite number to fail primeTestsF for k=2:"
    f2Fail <- findFirstFailF2
    print f2Fail
    putStrLn "\nFind first composite number to fail primeTestsF for k=3:"
    f3Fail <- findFirstFailF3
    print f3Fail
    

    putStrLn "\n==== ASSIGNMENT 6 ===="
    putStrLn "Testing tree1:"
    quickCheckTree1
    putStrLn "\nTesting tree2:"
    quickCheckTree2
