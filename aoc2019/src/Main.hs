module Main
(
  module Helpers,
  module Day1,
  module Day2,
  module Day3,
  module Day4,
  main
) where

import Helpers
import Data.Functor
import Data.Semigroup
import Data.Monoid
-- import PyF
import Control.Lens
import Control.Lens.Combinators
import Text.Pretty.Simple (pPrint)
import Test.Hspec
import Data.Text

import qualified Day1 as Day1
import qualified Day2 as Day2
import Day3
import Day4

main :: IO ()
main = do
  putStrLn "hello aoc world"


solution2  = Day2.solve1 <$> txt "2"
solution22 = Day2.solve1_2 <$> txt "2_2"
