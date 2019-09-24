-- LAB 3
-- Assignment 5
-- Time:

module Ass5 where
import Data.List
import HelperCodeLab3
import System.Random
import System.IO.Unsafe
import Ass4
import Ass1
import Test.QuickCheck
import SetOrd


-- How can you prove that the sub implementation is correct?
-- We can prove the sub implementation is correct by formulating properties of subsets
-- and testing if these properties are True.
-- From wikipedia we can obtain the following properties∷

-- A set X is a subset of Y if and only if their intersection is equal to X.
-- A set X is a subset of Y if and only if their union is equal to Y.
-- A finite set X is a subset of Y if and only if the cardinality of their intersection is equal to the cardinality of X.


-- Test the implementation with two QuickCheck properties.



sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

form_one = Impl (Cnj [Neg (Dsj [Prop 1, Prop 2]), Prop 3]) (Prop 4)


-- A set X is a subset of Y if and only if their union is equal to Y.
-- testSetProportyOne f = compare (unionSet (sub f) f) (list2set f)


-- A finite set X is a subset of Y if and only if the cardinality of their intersection is equal to the cardinality of X.



-- Test
-- Custom datatype used to generate and store random forms
-- data RandomForm = RandomForm Form deriving (Show)
-- instance Arbitrary RandomForm where
--     arbitrary = do
        -- It's best not to use unsafePerformIO, but no other solution was found
        -- let f = unsafePerformIO formGenerator
        -- return (RandomForm f)



-- quickCheckTestSetProportyOne  = quickCheckResult (\(RandomForm f) -> testSetProportyOne f)
