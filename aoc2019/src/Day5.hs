module Day5 where

import Control.Lens
import Control.Lens.Combinators
import Data.List as L
import Data.Map
import Data.Semigroup
import qualified Data.Set as S
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Lens
=======
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
  [ ((200,[3,0,4,0,99]), 200)

  ]

day5input :: [Int]
day5input = [3,0,4,0,99]

day5input2 :: [Int]
day5input2 = [1002,4,3,4,33]

solveday5_1 = undefined -- head $ view _1  $ fixComputer $ restore (instructions input) (12, 2)

solveday5_2 = undefined

-- Part 1

fixComputer' :: Int -> Program -> Program
fixComputer' input = fst . go 0
  where
    go pos instructions =
      let (op, p0, p1, address, m1, m2) = readInstruction pos (traceIns instructions)
          valueAtAddr = (!!) instructions
          binaryOp o =
            let (v1, v2) = over both valueAtAddr (p0, p1)
            in go (pos + 4) $ instructions & ix address .~ v1 `o` v2
       in case trace [fmt|op: {op}|] $ op of
            1 -> binaryOp (+)
            2 -> binaryOp (*)
            3 -> go (pos + 2) $ instructions & ix p0 .~ input
            4 -> go (pos + 2) $ instructions & trace [fmt|output = {valueAtAddr p0}|]
            99 -> trace "halting" (instructions, pos)
            _ -> error "undefined"

readInstruction pos instructions = 
  let (opAndModes : params) =  P.drop pos instructions
      digits = Day4.digits $ opAndModes
      opModes = tracse $ reverse $ digits
      op = tracde $ P.reverse $ P.concat $ show <$> P.take 2 opModes
      modes = default0 $ P.drop 2 opModes
      param = default0 params
  in (read @Int op, param 0, param 1, param 2, modes 0, modes 1)

default0 list i = fromMaybe 0 $ list ^? (ix i)

tracse v = trace ("opstr ==  " <> show v) v
tracde v = trace ("op ==  " <> show v) v

traceIns :: Show a => a -> a
traceIns v = trace ("instructions " <> show v) v

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
=======
import Debug.Trace
import Helpers
import PyF
import Test.Hspec
import Prelude as P
import Day2
=======