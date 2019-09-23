module MainFile where
import Ass1
import Ass2
import Data.List
import HelperCodeLab3
import Test.QuickCheck

main = do
    putStrLn "==== ASSIGNMENT 1 ===="
    putStrLn ("Test contradiction: " ++ show testContradiction)
    putStrLn ("Test tautology: " ++ show testTautology)
    putStrLn ("Test entails: " ++ show testEntails)
    putStrLn ("Test equivalence: " ++ show testEquiv)

    putStrLn "\n==== ASSIGNMENT 2 ===="
    -- TODO automated tests
    putStrLn ("Test manual prop: " ++ show (testManual "1" [Prop 1]))
    putStrLn ("Test manual neg: " ++ show (testManual "-1" [Neg (Prop 1)]))
    putStrLn ("Test manual cnj: " ++ show (testManual "*(1 2)" [Cnj [Prop 1, Prop 2]]))
    putStrLn ("Test manual dsj: " ++ show (testManual "+(1 2)" [Dsj [Prop 1, Prop 2]]))
    putStrLn ("Test manual impl: " ++ show (testManual "(1==>2)" [Impl (Prop 1) (Prop 2)]))
    putStrLn ("Test manual equiv: " ++ show (testManual "(1<=>2)" [Equiv (Prop 1) (Prop 2)]))
    putStrLn ("Test manual invalid: " ++ show (testManual "+-1" []))

    putStrLn "\n==== ASSIGNMENT 3 ===="
    -- TODO complete(?) & test using ass4

    putStrLn "\n==== ASSIGNMENT 4 ===="
    -- TODO test ass3

    putStrLn "\n==== ASSIGNMENT 5 ===="
