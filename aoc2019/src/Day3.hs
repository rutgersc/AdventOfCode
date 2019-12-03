module Day3 where

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

day3input = "R8,U5,L5,D3\nU7,R6,D4,L4" :: Text

day3input2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" :: Text

day3input3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" :: Text

day3_examples_1 =
  [ (day3input, 6),
    (day3input2, 159),
    (day3input3, 135)
  ]

day3_examples_2 =
  [ (day3input, 30),
    (day3input2, 610),
    (day3input3, 410)
  ]

testFor f (input, expected) = it "" $ expected `shouldBe` f input

day3spec :: Spec
day3spec = do
  describe "day3" $ do
    describe "1" $ do
      P.foldr1 (>>) $ testFor solveday3_1 <$> day3_examples_1

    describe "2" $ do
      P.foldr1 (>>) $ testFor solveday3_2 <$> day3_examples_2
  
solveday3_1 :: Text -> Int
solveday3_1 input =
  let (wire1 : wire2 : _) = followWires <$> splitOn "\n" input
  in minimum $ distanceTo (0, 0) <$> keys (intersection wire1 wire2)

solveday3_2 :: Text -> Int
solveday3_2 input =
  let (wire1 : wire2 : _) = followWires <$> splitOn "\n" input
  in minimum $ elems $ intersectionWithKey (const (+)) wire1 wire2

followWires :: Text -> Map Point Int
followWires = view path . foldlOf points follow (Wire {_pos = (0, 0), _path = empty, _steps = 0})
  where
    points = folding (splitOn ",") . withDirection

    follow :: Wire -> Point -> Wire
    follow prev point =
      let _pos = (prev ^. pos) `addPoint` point
          _steps = prev ^. steps + 1
          _path = prev ^. path
          fixedPath =
            if member _pos _path
              then _path
              else insert _pos _steps _path
       in Wire {_pos, _steps, _path = fixedPath}

withDirection :: Fold Text (Int, Int)
withDirection = folding $ \direction ->
  let (c : v) = unpack direction
      s = case c of
        'U' -> (0, 1)
        'D' -> (0, -1)
        'L' -> (-1, 0)
        'R' -> (1, 0)
  in s ^.. replicated (read v)