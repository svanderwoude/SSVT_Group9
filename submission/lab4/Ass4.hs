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
