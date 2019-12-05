module Day5 where

import Control.Lens
import Control.Lens.Combinators
import Data.List as L
import Data.Map
import Data.Semigroup
import qualified Data.Set as S
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Lens
import Data.Void
import Day2
import Debug.Trace
import Helpers
import Numeric.Lens (decimal)
import PyF
import Test.Hspec
import Prelude as P
import Day4 as Day4
import Data.Maybe

day5_examples_1 =
  [ (111111, True),
    (223450, False),
    (123789, False)
  ]

day5input :: [Int]
day5input = undefined

solveday5_1 = undefined -- head $ view _1  $ fixComputer $ restore (instructions input) (12, 2)

solveday5_2 = undefined

-- Part 1

fixComputer' :: Int -> FullProgram -> FullProgram
fixComputer' input = _1 %~ fst . go 0
  where
    go pos instructions =
      let (opAndMode : params) = P.drop pos instructions
          (op, m1, m2) = getModeFrom opAndMode
          binaryOp o =
            let (p1 : p2 : address : _) = params
                (v1, v2) = over both ((!!) instructions) (p1, p2)
            in go (pos + 4) $ instructions & ix address .~ v1 `o` v2
       in case op of
            1 -> binaryOp (+)
            2 -> binaryOp (*)
            3 -> go (pos + 2) $ instructions & ix (params !! 0) .~ input
            4 -> go (pos + 2) $ instructions & trace [fmt|output = {params !! 0}|]
            99 -> (instructions, pos)
            _ -> error "undefined"

getModeFrom :: Int -> (Int, Int, Int)
getModeFrom opcode = (p 0, p 1, p 2)
  where 
    digits = reverse $ Day4.digits opcode
    p i = fromMaybe 0 $ digits ^? ix i

{-
opcode =
  2-digit number
  rightmost 2-digits of the 1st value in an instruction.

parameterMode =
  single digits (0-9),
  one per parameter, read right-to-left from the opcode
  1st pMode = 100s digit, 2nd = 1000s digit, the 3rd = 10.000s digit, Any missing modes are 0.


  PROGRAM: 1002,4,3,4,33.
             ^^--- Multiply (Rightmost 2 digits)
            ^-- param1Mode 0 (100s digit)
           ^--- param2Mode 1 (1000s digit)
          ^---- param3Mode 0 (10000s digit, not present)

          -** Parameters that an instruction writes to will never be in immediate mode.

  ** op -> (param, mode) ->
-}
