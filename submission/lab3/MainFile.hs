module MainFile where
import Ass1
import Ass2
import Ass3
import Ass4
import Ass5
import Data.List
import HelperCodeLab3
import Test.QuickCheck

main = do
    putStrLn "==== ASSIGNMENT 1 ===="
    -- In order to test our definitions of the contradiction, tautology, entails
    -- and equivalence methods we can use a number of methods.
    -- First of all, we can create manual tests in order to test whether the
    -- expectations we set for our own formulas are indeed correctly given by
    -- the corresponding method.
    --
    -- Secondly, we could add a set of automatic tests. The best way in which we
    -- can do this is to generate a set of random forms using the random form
    -- generator we created in assignment 4. This way we know for sure that the
    -- tested method works for any random form as well. However, this won't work
    -- for our methods. For example, we won't be able to control whether
    -- the form generator will return a formula that is a contradiction. Because
    -- of this lack of control in the form generator, which is purely random, we
    -- are unable to apply automatic testing without creating a set of manual
    -- formulas, which removes the added value of automated tests.
    --
    -- One way in which we could apply automated tests is to remake a form
    -- generator with specific limited boundaries and rules that have to be
    -- applied. For example we could say that it should contain a sub-formula
    -- Dsj [p, (Neg p)] in the case of a tautology. However, if we would do this
    -- we would again remove the added value of automated testing, since all
    -- generated forms will have the same structure. This in turn means that a
    -- set of manual tests would be sufficient since it will always follow the
    -- same rules. The same goes for creating very complex formulas: since we
    -- know (or at least can assume) that the provided genVals method returns
    -- the correct results for each formula, no matter the complexity, it will
    -- have no added value to create overly complex formulas since they will act
    -- the same as more simple formulas.

    -- Manually test contradiction
    testContradictionIO (Cnj [p, (Neg p)]) True
    testContradictionIO (p) False

    -- Manually test tautology
    testTautologyIO (Dsj [p, (Neg p)]) True
    testTautologyIO (p) False

    -- Manually test entails
    testEntailsIO (p) (Dsj [p, q]) True
    testEntailsIO (p) (Neg p) False

    -- Manually test equivalence
    testEquivIO (Dsj [p, (Neg p)]) (Dsj [(Neg p), p]) True
    testEquivIO (p) (Neg p) False


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
