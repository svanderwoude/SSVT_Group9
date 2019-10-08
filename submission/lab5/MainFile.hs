module MainFile where
import Ass1
import Ass2
import Ass3
import Ass4
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


    putStrLn "\n==== ASSIGNMENT 4 ===="
    putStrLn "Testing Fermat's primality check using list generated by carmichael function:"
    putStrLn "With k = 1"
    k1 <- testFermatWithCarMichael 0 1
    print k1

    putStrLn "\nWith k = 2"
    k2 <- testFermatWithCarMichael 0 2
    print k2

    putStrLn "\nWith k = 3"
    k3 <- testFermatWithCarMichael 0 1
    print k3

    putStrLn "\nPlease see Ass4.hs for 4.2"


    putStrLn "\nPlease see Ass4.hs to see what we found about the Miller-Rabin primality check"


    putStrLn "\n==== ASSIGNMENT 6 ===="
    putStrLn "Testing tree1 for all coprimes:"
    quickCheckTree1
    putStrLn "\nTesting tree1 for expected list of pairs:"
    quickCheckTree1List
    putStrLn "\nTesting tree2 for all coprimes:"
    quickCheckTree2
    putStrLn "\nTesting tree2 for expected list of pairs:"
    quickCheckTree2List
