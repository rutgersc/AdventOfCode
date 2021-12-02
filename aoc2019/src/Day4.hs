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
=======
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
=======
type Point = (Int, Int)

addPoint (x, y) (x', y') = (x + x', y + y')

distanceTo (x, y) (x', y') = abs (x - x') + abs (y - y')

data Wire
    = Wire
        { _pos :: Point,
        _steps :: Int,
        _path :: Map Point Int
        }

makeLenses ''Wire

day4input = "R8,U5,L5,D3\nU7,R6,D4,L4" :: Text

day4input2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" :: Text

day4input3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" :: Text

day4_examples_1 =
    [ (day4input, 6),
    (day4input2, 159),
    (day4input3, 135)
    ]

-- day4_examples_2 =
--     [ (day3input, 30),
--     (day3input2, 610),
--     (day3input3, 410)
--     ]

testFor f (input, expected) = it "" $ expected `shouldBe` f input

-- day3spec :: Spec
-- day3spec = do
--     describe "day3" $ do
--     describe "1" $ do
--         P.foldr1 (>>) $ testFor solveday3_1 <$> day3_examples_1

--     describe "2" $ do
--         P.foldr1 (>>) $ testFor solveday3_2 <$> day3_examples_2

{-
Facts:
    It is a six-digit number.
    The value is within the range given in your puzzle input.
    Two adjacent digits are the same (like 22 in 122345).
    Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

    It is between the given range

    111111 meets these criteria (double 11, never decreases).
    223450 does not meet these criteria (decreasing pair of digits 50).
    123789 does not meet these criteria (no double).
-}
    
solveday3_1 :: Text -> Int
solveday3_1 input = undefined

solveday3_2 :: Text -> Int
solveday3_2 input = undefined
=======