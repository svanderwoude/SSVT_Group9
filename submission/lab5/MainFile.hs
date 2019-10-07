module MainFile where
-- import Ass1
-- import Ass2
-- import Ass3
-- import Ass4
-- import Ass5
import Ass6
import Data.List
import Test.QuickCheck

main = do
    putStrLn "\n==== ASSIGNMENT 6 ===="
    putStrLn "Testing tree1:"
    quickCheckTree1
    putStrLn "\nTesting tree2:"
    quickCheckTree2
