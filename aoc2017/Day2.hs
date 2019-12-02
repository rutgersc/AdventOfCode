import Data.Maybe
import Data.List
import Test.Tasty
import Test.Tasty.HUnit

day2Spreadsheet = 
    "5048 177 5280 5058 4504 3805 5735 220 4362 1809 1521 230 772 1088 178 1794\n\
    \6629 3839 258 4473 5961 6539 6870 4140 4638 387 7464 229 4173 5706 185 271\n\
    \5149 2892 5854 2000 256 3995 5250 249 3916 184 2497 210 4601 3955 1110 5340\n\
    \153 468 550 126 495 142 385 144 165 188 609 182 439 545 608 319\n\
    \1123 104 567 1098 286 665 1261 107 227 942 1222 128 1001 122 69 139\n\
    \111 1998 1148 91 1355 90 202 1522 1496 1362 1728 109 2287 918 2217 1138\n\
    \426 372 489 226 344 431 67 124 120 386 348 153 242 133 112 369\n\
    \1574 265 144 2490 163 749 3409 3086 154 151 133 990 1002 3168 588 2998\n\
    \173 192 2269 760 1630 215 966 2692 3855 3550 468 4098 3071 162 329 3648\n\
    \1984 300 163 5616 4862 586 4884 239 1839 169 5514 4226 5551 3700 216 5912\n\
    \1749 2062 194 1045 2685 156 3257 1319 3199 2775 211 213 1221 198 2864 2982\n\
    \273 977 89 198 85 1025 1157 1125 69 94 919 103 1299 998 809 478\n\
    \1965 6989 230 2025 6290 2901 192 215 4782 6041 6672 7070 7104 207 7451 5071\n\
    \1261 77 1417 1053 2072 641 74 86 91 1878 1944 2292 1446 689 2315 1379\n\
    \296 306 1953 3538 248 1579 4326 2178 5021 2529 794 5391 4712 3734 261 4362\n\
    \2426 192 1764 288 4431 2396 2336 854 2157 216 4392 3972 229 244 4289 1902\n"

main :: IO ()
main = defaultMain $ testCase "Day2 tests" $ do
    assertEqual ""       (9,1) $ maxMin [5,1,9,5]
    assertEqual ""       (7,3) $ maxMin [7,5,3]
    assertEqual "" (5735, 177) $ maxMin $ read <$> words "5048 177 5280 5058 4504 3805 5735 220 4362 1809 1521 230 772 1088 178 1794"
    assertEqual ""           6 $ diff $ [2,4,6,8]
    assertEqual "example"   18 $ solve $ readSpreadsheet "5 1 9 5\n7 5 3\n2 4 6 8"
    assertEqual "puzzle" 58975 $ solve $ readSpreadsheet day2Spreadsheet
    
readSpreadsheet :: String -> [[Int]]
readSpreadsheet sheet = fmap read . words <$> lines sheet

solve :: [[Int]] -> Int
solve = sum . fmap diff 

diff :: [Int] -> Int
diff = uncurry (-) . maxMin 

maxMin :: [Int] -> (Int, Int)
maxMin = foldl maxMinFold (0, maxBound)

maxMinFold :: (Int, Int) -> Int -> (Int, Int)
maxMinFold (currMax, currMin) d = (max currMax d, min currMin d)