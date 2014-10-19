{-

need a way to get viable neighbors of square

lets try tests, too hard without writing tests

test swapping 0 and 8 in start
then test swapping 0 and 6

-}

module Slider where

------------
-- Model
import Array

type Square = Int
type Row = Array Square
type Board = Array Row
type Index = (Int, Int)

emptySquare : Square
emptySquare = 0

start : Board
start = fromList [fromList [1,2,3]
                 ,fromList [4,5,6]
                 ,fromList [7,8,emptySquare]
                 ]


-- helper for replacing empty and swapper in row
swapSquare : Square -> Square -> Square
swapSquare swapper actual = if | actual == swapper -> emptySquare
                               | actual == emptySquare -> swapper
                               | otherwise -> actual

-- swap takes board and number and swaps with empty and returns new board
swap : Square -> Board -> Board
swap s b = Array.map (\row -> Array.map (\sq -> swapSquare s sq) row) b

{-
if item else deeper
-}
indexOfInBoard : Square -> Board -> Maybe Index
indexOfInBoard sq b = Array.
{-


-- given board, find neghbors of square
findNeighbors : Square -> Board -> [Square]
findNeighbors sq b = let (x,y) = indexOfInBoard sq b
                     in
-}

-- empty neighbors gets list of Squares next to empty
-- stepShuffle shuffles based on random number
-- shuffle finds empty and uses input to pick direction for moving
--shuffle : Float -> Board -> Board



-----------
-- View




----------
-- Control

