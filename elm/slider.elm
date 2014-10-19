{-

need a way to get viable neighbors of square

lets try tests, too hard without writing tests

test swapping 0 and 8 in start
then test swapping 0 and 6

what about a graph like representation where its
data Tile = Edge | EmptyTile | Tile Int
type Square = { me: Tile, L:Tile, R:Tile, U:tile, D:Tile}
Board = [Square]

so everyone knows their nighbors, no need for indices
 swap means find object and pick a neighbor and replacing their things

lists2Board (too hard to write out graph)
swap s1 s2 Board -> Board

[ { 1, E, 2, E, 4 }, { 2, 1, 3, E, 5}, { 3, 2, E, E, 6}
  { 4, E, 5, 1, 7 }, { 5, 4, 6, 2, 8}, { 6, 5, E, 3, _}
  { 7, E, 8, 4, E }, { 8, 7, _, 5, E}, { _, 8, E, 6, E} ]
(not necessarily in that order)
swap means cycle through every one and update numbers from one to the other
I don' think its productive
better is just a board with pieces on it each square of board has index
squares of baord are just a list of those pieces swapping is replacing two squares with updated numbers just a map step
are things finished means some sort of creating answer and comparing to current state

starting initially means just iterating list

need to know when finished as well

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

what about an r
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

