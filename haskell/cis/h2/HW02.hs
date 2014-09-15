{-
Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:

formableBy :: String -> Hand -> Bool
formableBy "" _ = True
formableBy (x:xs) chars =
  if elem x chars
  then formableBy xs (delete x chars)
  else False

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (\wrd -> formableBy wrd hand) allWords

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate t chars w =
  if wordFitsTemplate' t w
  then formableBy w (chars ++ (filter (\x -> x /= '?') t))
  else False


wordFitsTemplate' :: Template -> String -> Bool -- just check if word fits template (not hand)
wordFitsTemplate' "" "" = True
wordFitsTemplate' ('?':ts) (w:ws) = wordFitsTemplate' ts ws
wordFitsTemplate' (t:ts) (w:ws) =
  if t == w
  then wordFitsTemplate' ts ws
  else False
wordFitsTemplate' "" (w:ws) = False
wordFitsTemplate' (t:ts) "" = False

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (\wrd -> wordFitsTemplate template hand wrd) allWords

scrabbleValueWord :: String -> Int
scrabbleValueWord str = foldl (\val char -> val + (scrabbleValue char)) 0 str

bestWords :: [String] -> [String]
bestWords words = bestWords' words [] 0

bestWords' :: [String] -> [String] -> Int -> [String]
bestWords' [] found _ = found
bestWords' (w:ws) found score
  | thisScore > score = bestWords' ws [w] thisScore
  | thisScore == score = bestWords' ws (found ++ [w]) score
  | otherwise = bestWords' ws found score
  where
    thisScore = scrabbleValueWord w

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate template word = svt template word 0

svt :: STemplate -> String -> Int -> Int
svt [] _ val = val
svt (t:ts) (w:ws) val
  | t == '2' = 2 * (svt ts ws (val + charScore))
  | t == '3' = 3 * (svt ts ws (val + charScore))
  | t == 'D' = svt ts ws (val + (2 * charScore))
  | t == 'T' = svt ts ws (val + (3 * charScore))
  | otherwise = svt ts ws (val + charScore)
  where
    charScore = scrabbleValue w
