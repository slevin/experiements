module P4 where

import Data.List
import Data.Char
import Data.Maybe

caps :: String -> Bool
caps [] = False
caps (x:_) = x == toUpper x

allCaps :: [String] -> Bool
allCaps msgs = and $ map caps msgs

--dropTrailingWhitespace :: String -> String
