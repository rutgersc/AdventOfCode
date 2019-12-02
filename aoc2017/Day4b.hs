import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad
import Data.Maybe

day4Input :: IO [String]  
day4Input = lines <$> readFile "Input/Day4.txt"

main :: IO ()
main = defaultMain $ testCase "Day3 tests" $ do
    assertEqual "" False $ isAnagram ["abcde", "fghij"]
    assertEqual "" True  $ isAnagram ["abcde", "ecdab"]
    assertEqual "" True  $ isAnagram ["oiii", "ioii"]
    assertEqual "" True  $ isValidPassphrase "abcde fghij"
    assertEqual "" False $ isValidPassphrase "abcde xyz ecdab"
    assertEqual "" True  $ isValidPassphrase "a ab abc abd abf abj" 
    assertEqual "" True  $ isValidPassphrase "iiii oiii ooii oooi oooo" 
    assertEqual "8" False $ isValidPassphrase "oiii ioii iioi iiio" 
    assertEqual "" True $ isValidPassphrase "drczdf bglmf gsx flcf ojpj kzrwrho owbkl dgrnv bggjevc"
    assertEqual "" False $ isValidPassphrase "ndncqdl lncaugj mfa lncaugj skt pkssyen rsb npjzf"
    assertEqual "" 3 $ validLines ["abcde fghij", "abcde xyz ecdab", "a ab abc abd abf abj", "iiii oiii ooii oooi oooo", "oiii ioii iioi iiio"] 
    input <- day4Input 
    assertEqual "" 223 $ validLines input  

validLines :: [String] -> Int 
validLines = sum . fmap (fromEnum . isValidPassphrase) 

isValidPassphrase :: String -> Bool
isValidPassphrase input = isNothing $ find isAnagram $ combinations (words input) 
    where combinations = filter (\d -> 2 == length d) . subsequences

isAnagram (a:b:_) = sort a == sort b