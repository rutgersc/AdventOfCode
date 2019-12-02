module Day2 where

import Control.Lens
import Control.Lens.Combinators
import Control.Lens.Internal
import Data.Functor
import Data.Monoid
import Data.Semigroup
import Data.Text (Text, pack, splitOn, unpack)
import Data.Text.Lens
import Debug.Trace
import Helpers
import Numeric.Lens as Numeric.Lens
import Numeric.Lens (decimal)
import PyF
import Test.Hspec
import Text.Pretty.Simple (pPrint)
import Prelude as P

type Program = [Int]
type Noun = Int
type Verb = Int
type FullProgram = (Program, (Noun, Verb))

instructions :: String -> Program
instructions input = splitOn "," (pack input) ^.. traverse . unpacked . decimal

solve1 :: String -> Int
solve1 input = head $ view _1  $ fixComputer $ restore (instructions input) (12, 2)

solve1_2 :: String -> (Program, Int)
solve1_2 input = result & _2 %~ combineNounVerb
  where 
    restoredProg = restore (instructions input) <$> allNounVerbs
    expected = (==)19690720 . head . fst
    result = head $ filter expected $ fixComputer <$> restoredProg
    combineNounVerb (n, v) = 100 * n + v
    allNounVerbs = (,) <$> [0..99] <*> [0..99]
    

-- Part 1

restore :: Program -> (Noun, Verb) -> FullProgram
restore p nv@(noun, verb) = (p', nv)
  where p' = p & ix 1 .~ noun 
               & ix 2 .~ verb

fixComputer :: FullProgram -> FullProgram
fixComputer = _1 %~ fst . go 0 
  where
    go pos instructions =
      let (op : params) = drop pos instructions
          (p1 : p2 : at : _) = params
          (v1, v2) = over both ((!!) instructions) (p1, p2)
          execute o = go (pos + 4) $ set (ix at) (v1 `o` v2) instructions
       in case op of
            1 -> execute (+)
            2 -> execute (*)
            _ -> (instructions, pos)

    