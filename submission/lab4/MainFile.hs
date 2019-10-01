module MainFile where
import Ass1
import Ass2
import Ass3
import Ass4
import Ass5
import Ass6
import Ass7
import Data.List
import HelperCodeLab4
import SetOrd
import Test.QuickCheck

main = do
    putStrLn "==== ASSIGNMENT 1 ===="
    putStrLn "Manual random Set Int:"
    putStrLn "Run in 'ghci Ass1.hs':"
    putStrLn "genSetsFromScratch"
    randset <- genSetsFromScratch
    print randset

    putStrLn "\nQuickCheck random Set Int:"
    putStrLn "Run in 'ghci Ass1.hs':"
    putStrLn "generate arbitrary :: IO (Set Int)"


    putStrLn "\n==== ASSIGNMENT 2 ===="
    putStrLn "Testing intersection tests:"
    intersectionSetsTest

    putStrLn "\nTesting union tests:"
    unionSetsTest

    putStrLn "\n Testing difference tests:"
    differenceSetsTest


    putStrLn "\n==== ASSIGNMENT 3 ===="
    putStrLn "See assignment 6"


    putStrLn "\n==== ASSIGNMENT 4 ===="
    putStrLn "Copy ass4 test"


    putStrLn "\n==== ASSIGNMENT 5 ===="
    putStrLn "See assignment 6"


    putStrLn "\n==== ASSIGNMENT 6 ===="
    putStrLn "Testing symmetric closure:"
    testSymClos
    quickCheckSymClos

    putStrLn "\nTesting transitive closure:"
    testTrClos
    quickCheckTrClos


    putStrLn "\n==== ASSIGNMENT 7 ===="
    putStrLn "Testing if testTrClosSymClos holds. QuickCheck should generate "
    putStrLn "a counter example to fail the test, which we use to verify that "
    putStrLn "there is a difference between the two:\n"
    quickCheckTrClosSymClos
    