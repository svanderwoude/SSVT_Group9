

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

-- Excercise 3
-- Time spent: 1h
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Impl f1 f2) = arrowfree (Impl f1 f2) 
cnf (Equiv f1 f2) = arrowfree (Equiv f1 f2)
cnf (Neg fs) = (nnf.arrowfree) (Neg fs)
cnf (Cnj fs) = Cnj (map cnf fs)
-- cnf (Dsj ((Cnj ys):(Prop x):fs)) = Cnj (((map cnf fs) ++ [Dsj ((map cnf ys) ++ [Prop x])]))
--cnf (Dsj ((Cnj ys):(Prop x):fs)) = Cnj ((map cnf fs) ++ (map (\y -> Dsj [Prop x, cnf y]) (map cnf ys)))
--cnf (Dsj ((Prop x):(Cnj ys):fs)) = Cnj ((map cnf fs) ++ (map (\y -> Dsj [Prop x, cnf y]) (map cnf ys)))
cnf (Dsj ((Cnj xs):fs)) = Cnj (map (\(x, f) -> Dsj [cnf x, cnf f]) (zip xs fs))
cnf (Dsj fs) = Dsj (map cnf fs)

--Dsj [Cnj [1,2], Prop 3, 4]
--Dsj [Cnj [Prop 1,Dsj [Cnj [Prop 1, Prop 2], Prop 3]], Prop 3]


--(1 ^ ((1 ^ 2) v 3)) v 3