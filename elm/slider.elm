{-


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
  turn board into list of lists -> group/sort/sort
  elements are squares with number inside them
  and then have a an input signal attached to them which signals a new tile

add signals to elements

signal does swap with empty under its selection
 foldp on input signal does swap with empty
 might be kind of easy

detect win condition and show some element that its won

then do some reverse shuffling on start

then try for animations with swapping


given a list of list can I foldl new element into one of those lists

given [] and xy
 foldl sqs (\
 filter [] where item head sq.y == 
-}

-----------
-- View

-- turn board in to list of lists of tiles to make easy
-- generation of elements

squarePartitionedByY : Square -> [[Square]] -> [[Square]]
squarePartitionedByY sq ls = let found = filter (\l -> head l |> .y |> (==) sq.y) ls
                             in
                               case found of
                                 [] -> [sq] :: ls
                                 otherwise -> [sortBy (\sq -> sq.x) (sq :: (head found))]
                                                        
board2Lists : Board -> [[Square]]
board2Lists b = foldl squarePartitionedByY [] b
                
--boardElements : Board -> [Element]

                         


----------
-- Control

