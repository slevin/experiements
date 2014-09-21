{-# OPTIONS_GHC -Wall #-}

module Lan where

import Log
import Data.List
import Data.Char

-- parseMessage "E 2 256 somethign happens here" -> ValidLM (LogMessgae (Error 2) 256 "something...")
-- parseMessage "k wrong" -> InvalidLM
parseMessage :: String -> MaybeLogMessage
parseMessage str = let lst = words str in
                    case lst of
                     ("E":errNumStr:timeStr:rest) -> let errNum = readInt errNumStr;
                                                         timeNum = readInt timeStr in
                                                      case (errNum, timeNum) of
                                                       (ValidInt eNum, ValidInt tNum) -> ValidLM (LogMessage (Error eNum) tNum (unwords rest))
                                                       _ -> InvalidLM str
                     ("I":timeStr:rest) -> let timeNum = readInt timeStr in
                                            case timeNum of
                                             ValidInt tNum -> ValidLM (LogMessage Info tNum (unwords rest))
                                             _ -> InvalidLM str
                     ("W":timeStr:rest) -> let timeNum = readInt timeStr in
                                            case timeNum of
                                             ValidInt tNum -> ValidLM (LogMessage Warning tNum (unwords rest))
                                             _ -> InvalidLM str
                     _ -> InvalidLM str


validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly (ValidLM x@(LogMessage _ _ _):xs) = x:validMessagesOnly xs
validMessagesOnly (_:xs) = validMessagesOnly xs

parse :: String -> [LogMessage]
parse str = let arr = lines str in
             validMessagesOnly $ fmap parseMessage arr

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ ts1 _) (LogMessage _ ts2 _)
  | ts1 > ts2 = GT
  | ts1 == ts2 = EQ
  | otherwise = LT

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages msgs = sortBy compareMsgs msgs

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = fmap (\(LogMessage _ _ s) -> s) $ sortMessages $ filter (\(LogMessage t _ _) -> case t of
                                                                      (Error v) -> v >= 50
                                                                      _ -> False)
                                                              msgs

strupper :: String -> String
strupper s = map toUpper s

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout s msgs = filter (\(LogMessage _ _ m) -> isInfixOf (strupper s) (strupper m)) msgs

fltr :: String -> [LogMessage] -> [LogMessage]
fltr str msgs = filter (\(LogMessage t _ m) -> case (t,m) of
                                                ((Error v), _) -> v >= 50
                                                (_, m) -> isInfixOf (strupper str) (strupper m)) msgs

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced str msgs = fmap (\(LogMessage _ _ s) -> s) $ sortMessages $ fltr str msgs
