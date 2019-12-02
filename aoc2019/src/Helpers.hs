module Helpers where


import Test.Hspec

readLines :: Read r => FilePath -> IO [r]
readLines f = (fmap . fmap) read (lines <$> file)
    where file = readFile (f <> ".txt")