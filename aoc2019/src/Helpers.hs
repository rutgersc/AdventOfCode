module Helpers where


import Test.Hspec
import Control.Lens
import Data.Text.Lens

readLines :: Read r => FilePath -> IO [r]
readLines f = do
    f <- txt f
    return (read <$> lines f)

txt' f = readFile (f <> ".txt")

txt :: IsText t => FilePath -> IO t
txt f = view packed <$> readFile (f <> ".txt")