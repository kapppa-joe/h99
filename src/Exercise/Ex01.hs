module Exercise.Ex01 (strip)  where

import Data.Char

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
