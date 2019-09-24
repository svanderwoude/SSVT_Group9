

module Lab3 where
import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
    

-- Excercise 1
-- Time spent: 45min

-- A contradiction is a proposition that is always false, e.g. p && not p,
-- therefore in the context of Form and Valuation, we have to check that for a
-- given Form, all its Valuations evaluate to False
contradiction :: Form -> Bool
contradiction f = all (\v -> not (evl v f)) (allVals f)

testContradiction = do
    putStrLn ("Test contradiction with p && not p: " ++ show (contradiction (Cnj [p, Neg p])))
    putStrLn ("Test contradiction with p || not p: " ++ show (not $ contradiction (Dsj [p, Neg p])))


-- A tautology is a proposition that is always true, e.g. p || not p,
-- therefore in the context of Form and Valuation, we have to check that for a
-- given Form, all its Valuations evaluate to True
tautology :: Form -> Bool
tautology f = all (`evl` f) (allVals f)

testTautology = do
    putStrLn ("Test tautology with p || not p: " ++ show (tautology (Dsj [p, Neg p])))
    putStrLn ("Test tautology with x && not x: " ++ show (not $ tautology (Cnj [p, Neg p])))


-- | logical entailment 
-- f1 logically entails f2 if an only if all valuations which evaluate f1 to True,
-- also evaluete f2 to True
entails :: Form -> Form -> Bool
entails f1 f2 = all (`evl` f2) (filter (`evl` f1) (allVals f1))

testEntails = do
    putStrLn ("Test entails with p |= p || q: " ++ show (entails p (Dsj [p, q])))
    putStrLn ("Test entails with p |= p || not q: " ++ show (entails p (Dsj [p, Neg q])))
    putStrLn ("Test entails with not p |= p: " ++ show (entails (Neg p) p))

-- | logical equivalence
-- f1 is logically equivalent to f2 if an only if they have the same truth tables.
-- In the context of Form and Valuation, we have to check that all valuations that evaluate
-- f1 to True must evaluate f2 to True and vice versa. This can be checked also by
-- f1 <-> f2 is a tautology.
equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology (Equiv f1 f2)


-- Excercise 2

-- nnf :: Form -> Form 
-- nnf (Prop x) = Prop x
-- nnf (Neg (Prop x)) = Neg (Prop x)
-- nnf (Neg (Neg f)) = nnf f
-- nnf (Cnj fs) = Cnj (map nnf fs)
-- nnf (Dsj fs) = Dsj (map nnf fs)
-- nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
-- nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

-- cnf (Prop x) = Prop x
-- cnf (Neg f) = Neg (cnf f)
-- cnf (Dsj [Cnj [x,y],z]) = Cnj [cnf (Dsj [z,x]), cnf (Dsj [z,y])]
-- cnf (Cnj [z, Cnj [x, y]]) = Cnj [cnf (Dsj [z,x]), cnf (Dsj [z,y])]
-- cnf (Cnj fs) = Dsj (map cnf fs)
-- cnf (Dsj fs) = Dsj (map cnf fs)


-- Excercise 3
-- Time spent: 1d 

-- First, we clean up --> and <-> and we push negations inwards such that only atoms are negated.
cnf :: Form -> Form
cnf f = cnf' ((nnf.arrowfree) f)

cnf' :: Form -> Form
-- A property results in itself
cnf' (Prop x) = Prop x
-- A negated property results in itself. It is not possible to have a negated formula (Cnj|Dsj), since we call
-- this function with NNF propisition already 
cnf' (Neg p) = Neg p
-- If there is Cnj over Cnj, this can be replaced with a single Cnj. For example, P && (Q && R) is equal to
-- P && Q && R. The pattern match does not seem to work
cnf' (Cnj ((Cnj xs):fs)) = Cnj (map cnf' (xs ++ fs))
cnf' (Cnj fs) = Dsj (map cnf fs)
-- If there is Dsj over Cnj, we have to convert it by applying distributive law. For example, P || (Q && R), should
-- result in (P || Q) && (P || R). This pattern would only match a case where the Cnj is the first element, however
-- if it is in any place of the list of the disjunction, the same rule should be applied (Not sure how to do that).
cnf' (Dsj ((Cnj xs):fs)) = Cnj (map (\(x, f) -> Dsj [cnf' x, cnf' f]) [(x, f) | x <- xs, f <- fs])
-- Any other Dsj is just itself with converting its members to cnf.
cnf' (Dsj fs) = Dsj (map cnf' fs)

-- isCnf is used in converting to cnf as a form must be checked over and over
-- untill it is cnf, if not it has to be rerun (as this runs inward it can take
-- a long time for long forms). DOESN'T CHECK CORRECTLY
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Prop x)) = True
isCnf (Neg _) = False
isCnf (Dsj xs) = not (any containsCnj xs) && all containsCnj xs
isCnf (Cnj xs) = all containsCnj xs


-- Simple function that checks if an element is a conjucntion.
containsCnj :: Form -> Bool
containsCnj (Cnj xs) = True
containsCnj _ = False

toCnf :: Form -> Form
toCnf f = while (not . isCnf) cnf f

--Dsj [Cnj [1,2], Prop 3, 4]
--Dsj [Cnj [Prop 1,Dsj [Cnj [Prop 1, Prop 2], Prop 3]], Prop 3]


--(1 ^ ((1 ^ 2) v 3)) v 3