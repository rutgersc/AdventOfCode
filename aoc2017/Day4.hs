import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Data.Set

day4Input :: IO [String]  
day4Input = lines <$> readFile "Input/Day4.txt"

main :: IO ()
main = defaultMain $ testCase "Day3 tests" $ do
    assertEqual "1" True  $ isValidPassphrase "aa bb cc dd ee"
    assertEqual "2" False $ isValidPassphrase "aa bb cc dd aa"
    assertEqual "3" True  $ isValidPassphrase "aa bb cc dd aaa"
    assertEqual "6" 1 $ validLines ["aa bb cc", "cc dd dd"]
    input <- day4Input 
    assertEqual "7" 451 $ validLines input  

validLines :: [String] -> Int 
validLines input = sum $ fmap fromEnum $ fmap isValidPassphrase  input 

isValidPassphrase :: String -> Bool
isValidPassphrase input = length (words input) == (size $ fromList $ words input)

-- validLinesDebug ["aa bb cc", "cc dd dd"]

validLinesDebug :: [String] -> [Bool]
validLinesDebug = fmap isValidPassphrase 