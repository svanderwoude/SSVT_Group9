module Ass4 where
import Data.List
import HelperCodeLab4
import System.Random
import Test.QuickCheck


isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain relations = all (\x_first -> any (\(x_second,y) -> x_first == x_second && y `elem` domain && x_second /= y) relations) domain



infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q


-- Property: If the number of relations is smaller then the number of elements in the domain then that means
-- that not for any x 'elem' A in xRy does not hold ->
-- ToDo: Elaborate more and use quickcheck
propertyOne domain relations = length relations < length domain --> not (isSerial ds rs)



-- Consider the relation R = {(x,y) | x = y (mod n)} where (mod n) is the modulo
-- function in modular arithmetic and n > 0.

-- Discuss whether (and when) R is serial:
-- x = y (mod n) is a relation that tells us that x and y will have the same
-- remainder when divided by a value n. This means that we can rewrite the
-- provided relation R into a formula: x = k*n + y. This relation is considered
-- to be a congruence relation. Because of this we can say that:
-- x = y (mod n) in modular arithmetic is considered to be a congruence relation.
-- This in turn means that it will satisfy all conditions of an equivalence
-- relation. Which means that the relation R holds the following properties:
--
-- - R is reflexive
-- - R is symmetric
-- - R is transitive
--
-- Since we a reflexive relation is always a serial relation, we can consider the
-- relation R to be serial in all cases where it can be considered reflexive.
-- Since a symmetric and transitive relation can be proven to be a reflexive
-- relation as well, it would suffice to proof that the relation is reflexive.
-- In the case of this relation the properties defined above can be written as
-- follows:
--
-- - Reflexivity: x = x (mod n)
-- - Symmetry: x = y (mod n) if y = x (mod n)
-- - Transivity: if x = y (mod n) and y = z (mod n), then x = z (mod n)
--
-- Since in our case any value of x would satisfy the reflexive property we can
-- say that our relation R -> x = y (mod n) is always serial with n > 0. We can
-- proof this by looking at the formula x = k * n + y. In order to prove this we
-- can rewrite the formula to x - y = k * n. Since we want x to be equal to y,
-- we can rewrite this to be x - x = k * n. Now, since we we know that the left
-- side of this formula will always produce 0 as a result, we can rewrite the
-- equation to be 0 = k * n. Since we want to support any value of n > 0, we have
-- to figure out a way to make the right side 0 as well. Since we perform a
-- multiplication k * n, where k can be anything we want, we can simply say that
-- this value should be 0. In that case we would get the following result:
-- 0 = 0 * n, which becomes 0 = 0, proving that the relation is reflexive.
--
-- An example of how this would turn out with any value of n > 0 and any value
-- for x (and y, since they are equal):
--
-- x - y = k * n
-- 5 - 5 = k * 3
-- 0 = k * 3
-- 0 = 0 * 3
-- 0 = 0 -> multiples of 3
--
-- We can conclude that 0 is a multiple of any number n > 0, meaning that no
-- matter what value we assign to x and y, we will always end up with a
-- reflexive relation since 0 is always the result for both sides, as long as
-- x == y holds.
--
-- Thus, to conclude: the relation R is serial when it can be considered
-- reflexive either directly or indirectly through symmetry and transivity.
-- Since we can say that every possible (positive) number for y (and n) in this
-- relation can form a reflexive relation, it should work for all numbers if the
-- correct multiple is used for the value of x.
--
--
-- As mentioned above, we can test and prove that the relation R is serial when
-- the reflexive property can be proven. This is the case when x == y, and when
-- both x and y are in domain A.
--
-- Consider domain [1,2,3], we can the define the relation to be:
-- [(1,1), (2,2), (3,3)] by applying the rules defined above. This relation is
-- reflexive and thus serial, and it is valid since the remainder of x - y is
-- 0 in all cases, and 0 is a multiple of any value n > 0 that we set.
