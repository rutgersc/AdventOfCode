module Day1 where

import Control.Lens
import Control.Lens.Combinators
import Numeric.Lens

solve1 :: String -> Int
solve1 = sumOf (_Modules . to fuelFromMass)

solve1_2 ::  String -> Int
solve1_2 = sumOf (_Modules . unfolded fuelMass)

_Modules :: Integral a => IndexedTraversal' Int String a
_Modules = lined . decimal

-- Part 1

fuelFromMass :: Int -> Int
fuelFromMass mass = max 0 ((mass `div` 3) - 2)

-- Part 2

fuelMass :: Int -> Maybe (Int, Int)
fuelMass mass = if mass == 0 
  then Nothing 
  else Just (mass, fuelFromMass mass)

    