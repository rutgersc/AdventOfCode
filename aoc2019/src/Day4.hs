module Day4 where

import Control.Lens
import Control.Lens.Combinators
import Data.Map
import Data.Semigroup
import qualified Data.Set as S
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Lens
import Debug.Trace
import Helpers
import PyF
import Test.Hspec
import Prelude as P
import Data.List as L
import Numeric.Lens (decimal)

day4_examples_1 =
  [ (111111, True)
  , (223450, False)
  , (123789, False)
  ]

day4input :: [Int]
day4input = [235741..706948]

digits :: Int -> [Int]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

digit x = (x `div` 10 , x `mod` 10)

twoAdjacent = any (>=2) . L.map length . group

hasOneExactlyDoubledDigit = elem 2 . L.map length . group

notDescending x = sort x == x

solveday4_1 :: [Int] ->  Int
solveday4_1 = length . L.filter (\x -> notDescending x && twoAdjacent x) . fmap digits

solveday4_2 :: [Int] -> Int
solveday4_2 = length . L.filter (\x -> notDescending x && hasOneExactlyDoubledDigit x) . fmap digits
