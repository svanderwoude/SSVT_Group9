module Ass8 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Identify which boy accuses which boy(s)
accuses :: Boy -> Boy -> Bool
-- Matthew
accuses Matthew x = not (elem x [Carl, Matthew])
-- Peter
accuses Peter Matthew = True
accuses Peter Jack = True
-- Jack
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
-- Arnold
accuses Arnold x = not (accuses Matthew x == accuses Peter x)
-- Carl
accuses Carl x = not (accuses Arnold x)
-- Other combinations not listed are always False
accuses x y = False

-- List of occusers for each boy
accusers :: Boy -> [Boy]
accusers x = [y | y <- boys, accuses y x]

-- List of guilty boys
-- Find a majority result, if at least 3 kids say something it must be True
-- since there are only two that lie. If 3 kids give the same answer it must be
-- the truth because of this.
guilty :: [Boy]
guilty = [x | x <- boys, length (accusers x) == 3]

-- List of honest boys
honest :: [Boy]
honest = [x | x <- boys, accuses x (head guilty)]
