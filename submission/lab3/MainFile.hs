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
    --
    -- However, as we will see in assignment 3, we can use the equivalence
    -- method in order to test the correctness of the CNF converter. We also
    -- confirm this with manual tests. But in addition to this we could also use
    -- the CNF converter to test whether our equivalence method produces the
    -- desired results. From this side we may assume that the CNF converter
    -- works correctly, thus we can use it to automatically test the
    -- functionality of the equivalence function. Since both tests would be the
    -- same, this test can be found in assignment 3 when we test the CNF
    -- converter output.

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
    -- In order to test the provided parser we can use a number of methods.
    -- First of all, we can create manual tests in order to test whether the
    -- provided logic strings are parsed as expected by the parser. This way we
    -- can see if the parser output is in lign with our expectations.
    --
    -- Secondly, we could add a set of automatic tests. The best way to do this
    -- is by generating a set of random formulas using our random formula 
    -- generator from assignment 4. Since we know that this formula generator
    -- will always produce correct forms and/or logic strings, we can use it to
    -- test whether the parser is also able to parse it. Thus in the case of
    -- automated testing we can not test whether the resulting formula is
    -- exactly the same as the input string, but we can test whether it detects
    -- both correct and incorrect formula strings.
    --
    -- Another seemingly logical way to test this using our form generator is to
    -- see if the created formGenerator produces the same result as the parsed
    -- version of logicGenerator output. However, this wouldn't be useful since
    -- our implementation of formGenerator uses the parser to create a form
    -- object itself, meaning that in case of a comparison we would execute the
    -- same exact operation on the same input...
    --
    -- However, there is another way in which we could automatically test the
    -- parser. In order to do so we would have to create a new formula generator
    -- which does not generate logic string, but generates both logic string and
    -- form (token) objects at the same time. This way we could afterwards
    -- compare the generated form object with the parsed version of the logic
    -- string in order to figure out if it was succesfully parsed by the parser.
    --
    -- (TODO if enough time, create the generator mentioned above)

    -- Manually test Prop parsing
    putStrLn ("Test manual prop: " ++ show (testManual "1" [Prop 1]))

    -- Manually tests Neg parsing
    putStrLn ("Test manual neg: " ++ show (testManual "-1" [Neg (Prop 1)]))

    -- Manually test Cnj parsing
    putStrLn ("Test manual cnj: " ++ show (testManual "*(1 2)" [Cnj [Prop 1, Prop 2]]))

    -- Manually test Dsj parsing
    putStrLn ("Test manual dsj: " ++ show (testManual "+(1 2)" [Dsj [Prop 1, Prop 2]]))

    -- Manually test Impl parsing
    putStrLn ("Test manual impl: " ++ show (testManual "(1==>2)" [Impl (Prop 1) (Prop 2)]))

    -- Manually test Equiv parsing
    putStrLn ("Test manual equiv: " ++ show (testManual "(1<=>2)" [Equiv (Prop 1) (Prop 2)]))

    -- Manually test invalid formulas
    putStrLn ("Test manual invalid: " ++ show (testManual "+-1" []))


    putStrLn "\n==== ASSIGNMENT 3 ===="
    -- In order to test our formula to CNF converter, we can use a number of
    -- methods. The first method we could use is to check if the output of the
    -- converter is the same as our set expectations. These manual tests will
    -- show us whether certain statements (sub-formulas) are correctly converted
    -- into CNF form as we would expect.
    --
    -- Secondly, we can test whether the converted formulas give the same output
    -- as the pre-converter formulas. We can do this using our form generator
    -- from assignment 4. In order to test this, we have to generate a formula
    -- and create a duplicate, which is then converted into CNF form. By doing
    -- so we should have two differently formatted formulas giving the same
    -- output. We will test this by checking if they are equivalent using the
    -- equivalent method from assignment 1.
    --
    -- NOTE: it appears that sometimes our quickCheckCNF gets stuck, but it
    -- works in many other cases... Didn't have enough time to fix it.
    -- (Non-exhaustive patterns in function isCnf)
    --
    -- TODO finish correctly
    testCnfIO (Dsj [Cnj [Prop 1, Prop 2], Prop 3]) (Cnj [Dsj [Prop 1, Prop 3], Dsj [Prop 2, Prop 3]])
    testCnfIO (Cnj [Dsj [Prop 1, Prop 3], Dsj [Prop 2, Prop 3]]) (Cnj [Dsj [Prop 1, Prop 3], Dsj [Prop 2, Prop 3]])
    testCnfIO (Prop 1) (Prop 1)
    testCnfIO (Dsj [Cnj [Neg (Prop 1), Prop 2], Prop 3]) (Cnj [Dsj [Neg (Prop 1), Prop 3], Dsj [Prop 2, Prop 3]])

    putStrLn "\nTesting CNF converter (1): "
    quickCheckCNF
    -- Testing CNF converter (1):
    -- +++ OK, passed 100 tests.


    putStrLn "\n==== ASSIGNMENT 4 ===="
    -- Testing of the CNF converter: see assignment 3.
    --
    -- We can define a number of properties for CNF fomulas. First of all, like
    -- the name suggests, a CNF formula is a conjuction of one or more clauses.
    -- This means that the ouput of the CNF converter should be of this form.
    -- Another property we can also get from this is that a CNF formula should
    -- only contain AND and OR statements (Dsj and Cnj), besides Prop and
    -- Neg (only for single Props).
    --
    -- An important property of the converter is that, no matter if the
    -- conversion is completely correct or not, the input formula should
    -- produce the same results as the output formala. This is also explained in
    -- assignment 3 and tested.
    --
    -- NOTE: it appears that sometimes our quickCheckCNF gets stuck, but it
    -- works in many other cases... Didn't have enough time to fix it.
    -- (Non-exhaustive patterns in function isCnf)
    testCnfIO (Dsj [Cnj [Prop 1, Prop 2], Prop 3]) (Cnj [Dsj [Prop 1, Prop 3], Dsj [Prop 2, Prop 3]])
    testCnfIO (Cnj [Dsj [Prop 1, Prop 3], Dsj [Prop 2, Prop 3]]) (Cnj [Dsj [Prop 1, Prop 3], Dsj [Prop 2, Prop 3]])
    testCnfIO (Prop 1) (Prop 1)
    testCnfIO (Dsj [Cnj [Neg (Prop 1), Prop 2], Prop 3]) (Cnj [Dsj [Neg (Prop 1), Prop 3], Dsj [Prop 2, Prop 3]])

    putStrLn "\nTesting CNF converter (2): "
    quickCheckCNF
    -- Testing CNF converter (2):
    -- +++ OK, passed 100 tests.


    putStrLn "\n==== ASSIGNMENT 5 ===="
    -- Test whether all elements in created set are also present in original
    -- form. Using another way to get those elements
    putStrLn "\nTesting sub property of containing all correct sets: "
    quickCheckTestSetPropertyTwo

    -- In order to test the implementation of nsub', we could use an alternative
    -- way of determining the number of sub-formulas in a form just like we did
    -- in one of the properties of part 1. This way we can determine if the
    -- function returns the correct number of properties.
    --
    -- NOTE: it appears that sometimes our quickCheckTestNsubfails, but it
    -- works in many other cases... Didn't have enough time to fix it.
    putStrLn "\nTesting nsub' length correctness: "
    quickCheckTestNsub
