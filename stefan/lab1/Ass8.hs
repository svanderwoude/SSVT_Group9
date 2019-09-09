-- LAB 1
-- Assignment 8
-- Time: 

module Ass8 where
import Data.List
import HelperCodeLab1
import Test.QuickCheck

-- Identify which boy accuses which boy(s)
accuses :: Boy -> Boy -> Bool
-- Matthew
accuses Matthew Carl = False
accuses Matthew Matthew = False
accuses Matthew x = True
-- Peter
accuses Peter Matthew = True
accuses Peter Jack = True
accuses Peter x = True
-- Jack
accuses Jack x = not (accuses Matthew x) && not (accuses Peter x)
-- Arnold
accuses Arnold x = not (accuses Matthew x == accuses Peter x)
-- Carl
accuses Carl x = not (accuses Arnold x)

-- List of occusers for each boy
accusers :: Boy -> [Boy]
accusers x = [y | y <- boys, accuses y x]

-- List of guilty boys
guilty :: [Boy]
guilty = []

-- List of honest boys
honest :: [Boy]
honest = []
