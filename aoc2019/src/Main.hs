module Main
(
  module Helpers,
  module Day1,
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

import Day1 as Day1

main :: IO ()
main = do
  putStrLn "hello world"


q ::  SpecWith ()
q = return ()
------------------------------------
------------------------- Solutions

-- printSolve1   = solve1 <$> read1Lines "1"
-- printSolve1_2 = solve1_2 <$> read1Lines "1_2"
-- printSolve1_2_Examples = solve1_2 <$> [[1969], [100756]]
