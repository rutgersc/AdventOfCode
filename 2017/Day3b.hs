import Data.Maybe
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Data.Bifunctor

data Cell = Cell { _v :: Int,  _x :: Int,  _y :: Int } deriving (Eq, Show)
data Memory = Memory { _cells :: [Cell], _counter :: Int, _depth :: Int, _pos :: (Int, Int) }  deriving (Eq, Show)

initMemory = Memory [Cell 1 0 0] 1 0 (0,0)
shift1 (Cell v x y) = Cell v (x+1) (y+1)

main :: IO ()
main = defaultMain $ testCase "Day3 tests" $ do
    assertEqual "" ((Memory [Cell 1 1 1, Cell 1 2 1] 2 1 (2,1)), 2) $ nextSpiralStep initMemory
    assertEqual "" 349975 $ solve 347991

solve input = solve' initMemory input

solve' m input = 
    let (m', cellSum) = nextSpiralStep m
    in if input < cellSum then cellSum  else solve' m' input

nextSpiralStep :: Memory -> (Memory, Int) 
nextSpiralStep m@(Memory cs count depth (x,y)) = 
    let memMax = depth * 2
    in if x == memMax && y == memMax
        then 
            let xy@(x', y') = (x+1+1, y+1); 
                cell = Cell (sumAdjacent x' y' cs) x' y'
            in (Memory (cell:(shift1 <$> cs)) (count+1) (depth+1) xy, _v cell)
        else 
            let xy@(x', y') = allocate x y memMax
                cell = Cell (sumAdjacent x' y' cs) x' y'
            in (Memory (cell:cs) (count+1) depth xy, _v cell)

allocate :: Int -> Int -> Int -> (Int, Int)
allocate x y max
    | x == max && 0 < y = (0,-1)
    | y == 0 && 0 < x = (-1,0)
    | x == 0 && y < max = (0,1)
    | y == max && x < max = (1,0) 

sumAdjacent :: Int -> Int -> [Cell] -> Int
sumAdjacent cellX cellY cells = sum $ catMaybes $ do
    i <- [-1, 0, 1]
    j <- [-1, 0, 1] 
    return $ _v <$> find (\c -> _x c == (cellX+i) && _y c == (cellY+j)) cells


            

