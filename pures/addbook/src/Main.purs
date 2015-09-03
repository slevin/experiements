module Data.AddressBook where

import Prelude
import Data.List
import Data.Maybe
import Control.Plus (empty)

type Entry =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , state :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry e = e.lastName ++ ", " ++ e.firstName ++ ": " ++ showAddress e.address

showAddress :: Address -> String
showAddress a = a.street ++ ", " ++ a.city ++ ", " ++ a.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry fName lName book = head $ filter filterEntry book
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == fName && entry.lastName == lName
