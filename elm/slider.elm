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

data Tile = EmptyTile | Tile Int
type Square = { x:Int, y:Int, tile:Tile }
type Board = [Square]

columns : Int
columns = 3
rows : Int
rows = 3

-- Given a row/column count, creates an array of arrays with the containing tiles
starterList : Int -> Int ->[[Tile]]
starterList c r = let tileFn num = if | num == c * r -> EmptyTile
                                      | otherwise -> Tile num 
                      squareFn row col = col + (row * c) |> tileFn
                  in 
                    indexedMap (\idx x -> map (squareFn idx) [1..c]) [1..r]

-- Uses starter list to create a board which is an array of tiles and their positions
starterBoard : Int -> Int -> Board
starterBoard c r = let starter = starterList c r
                       rowFn idxRow row = indexedMap (\idxCol t -> Square idxCol idxRow t) row
                   in 
                     indexedMap (\idxRow row -> rowFn idxRow row) starter |> concat
                   

start : Board
start = starterBoard columns rows



neighbors : Tile -> Board -> [Tile]
neighbors t b = let tileSquares = take 1 <| filter (\sq -> sq.tile == t) b
                    tileNeighbors = concat <| map (\ts -> neighbors' ts.x ts.y b) tileSquares
                in
                  map (\sq -> sq.tile) tileNeighbors

neighbors' : Int -> Int -> Board -> [Square]
neighbors' x y b = filter (\sq -> (sq.x == x && sq.y == (y - 1)) ||
                                  (sq.x == x && sq.y == (y + 1)) ||
                                  (sq.x == (x - 1) && sq.y == y) ||
                                  (sq.x == (x + 1) && sq.y == y)) b


nextToEmpty : Tile -> Board -> Bool
nextToEmpty t b = any (\tile -> tile == EmptyTile) <| neighbors t b




swapWithEmpty : Tile -> Board -> Board
swapWithEmpty t b = if | nextToEmpty t b -> swapWithEmpty' t b
                       | otherwise -> b

swapWithEmpty' : Tile -> Board -> Board
swapWithEmpty' t b = map (\sq -> if | sq.tile == EmptyTile -> {sq | tile<-t}
                                    | sq.tile == t -> {sq | tile<-EmptyTile}
                                    | otherwise -> sq) b
{-

draw board

add signals to elements

signal does swap with empty under its selection

detect win condition and show some element that its won

then do some reverse shuffling on start

then try for animations with swapping

-}

-----------
-- View




----------
-- Control

