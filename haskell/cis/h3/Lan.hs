{-# OPTIONS_GHC -Wall #-}

module Lan where

import Log


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
