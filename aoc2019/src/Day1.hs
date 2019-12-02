module Day1 where

import Control.Lens
import Control.Lens.Combinators
import Control.Lens.Internal
import Data.Functor
import Data.Monoid
import Data.Semigroup
import Helpers
import Numeric.Lens as Numeric.Lens
import Test.Hspec
import Text.Pretty.Simple (pPrint)
import Prelude as P

type Mass = Int
type Module = Int
newtype Fuel = Fuel {unFuel :: Int}
  deriving (Show, Eq, Ord)
  deriving (Semigroup) via (Sum Int)
  deriving (Monoid) via (Sum Int)

solve1 :: Traversable f => f Module -> Fuel
solve1 = foldOf _RequiredFuelFold

solve1_2 ::  String -> Int
solve1_2 = getSum . fuelUsage'

examples =
  [ (12, Fuel 2),
    (14, Fuel 2),
    (1969, Fuel 654),
    (33583, Fuel 100756)
  ]

lines :: FilePath -> IO [Module]
lines f = readLines @Module f

-- Part 1

fuelFromMass :: Mass -> Int
fuelFromMass mass = max 0 ((mass `div` 3) - 2)

_RequiredFuelFold :: Traversable f => Fold (f Module) Fuel
_RequiredFuelFold = traverse . to (Fuel . fuelFromMass)

-- Part 2

_FuelUsageUnfold :: Fold Mass Int 
_FuelUsageUnfold  = unfolded (\mass -> 
  let f = fuelFromMass mass
  in if mass == 0 then Nothing else Just (f, f))

_FuelUsages :: Fold String Int
_FuelUsages =
  lined                     -- IndexedLensLike' Int f String String
    . Numeric.Lens.decimal  -- IndexedTraversal' Int String Int
    . _FuelUsageUnfold      -- Fold String Int

fuelUsage' :: String -> Sum Int
fuelUsage' = sumOf $ _FuelUsages . to Sum
    