module P4 where

import Data.List
import Data.Char
import Data.Maybe

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

caps :: String -> Bool
caps s = fmap toUpper (safeHead s) == safeHead s && safeHead s /= Nothing

allCaps :: [String] -> Bool
allCaps msgs = and $ map caps msgs



dropTrailingWhitespace :: String -> String
dropTrailingWhitespace str = dropWhileEnd isSpace str

firstLetters :: [String] -> [Char]
firstLetters strs = concat $ fmap (take 1) strs

asList :: [String] -> String
asList strs = "[" ++ (intercalate "," strs) ++ "]"

{-


-}
