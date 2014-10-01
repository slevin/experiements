-- fun1 :: [Integer] -> Integer
-- fun1 [] = 1
-- fun1 (x:xs)
-- | even x = (x - 2) * fun1 xs
-- | otherwise = fun1 xs

fun1' :: [Integer] -> Integer


-- fun2 :: Integer -> Integer
-- fun2 1 = 0
-- fun2 n | even n = n + fun2 (n ‘div‘ 2)
-- | otherwise = fun2 (3 * n + 1)


data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr foldTree' Leaf

foldTree' :: a -> Tree a -> Tree a
foldTree' x Leaf = Node 0 Leaf x Leaf
foldTree' x (Node num Leaf y Leaf) = Node (num + 1) (foldTree' x Leaf) y Leaf
foldTree' x (Node num Leaf y other) = Node num (foldTree' x Leaf) y other
foldTree' x (Node num other y Leaf) = Node num other y (foldTree' x Leaf)
foldTree' x (Node num left y right) = let
