module Bst where

import Data.Char
import Data.List


data BST a = Leaf | Node (BST a) a (BST a)
                    deriving Show

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node l v r)
  | f x v == LT = Node (insertBST f x l) v r
  | otherwise = Node l v (insertBST f x r)

ord :: Int -> Int -> Ordering
ord a b
  | a == b = EQ
  | a < b = LT
  | otherwise = GT

allCaps :: [String] -> Bool
