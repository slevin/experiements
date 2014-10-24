module Slider where

------------
-- Model
import Array
import Graphics.Input (Input, input, clickable)

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

detect win condition and show some element that its won

then do some reverse shuffling on start

how about hovering as a highlighting color

then try for animations with swapping

-}

----------
-- Control

tileClick : Input Tile
tileClick = input EmptyTile

gameState : Signal Board
gameState = foldp swapWithEmpty (starterBoard 3 3) tileClick.signal

-----------
-- View

sqSz : Int
sqSz = 60

sqSp : Int
sqSp = 10

squarePartitionedByY : Square -> [[Square]] -> [[Square]]
squarePartitionedByY sq ls = let found = partition (\l -> head l |> .y |> (==) sq.y) ls
                             in
                               case found of
                                 ([],others) -> others ++ [[sq]]
                                 (good, others) -> ([sortBy (\sq -> sq.x) ((head good) ++ [sq])] |> (++) others) |> sortBy (\l -> head l |> .y)

board2Lists : Board -> [[Square]]
board2Lists b = foldl squarePartitionedByY [] b
                
square2Element : Square -> Element
square2Element sq = case sq.tile of
                      EmptyTile -> spacer sqSz sqSz
                      Tile x -> [show x |> toText |> centered,
                                 spacer sqSz sqSz |> color red] |> 
                                flow inward |>
                                clickable tileClick.handle sq.tile

columnSpacer : Element
columnSpacer = spacer sqSp sqSz

squareRow2Element : [Square] -> Element
squareRow2Element sqs = map square2Element sqs |> 
                        intersperse columnSpacer |> 
                        flow right

rowSpacer : Element
rowSpacer = spacer sqSz sqSp

rows2Element : [Element] -> Element
rows2Element rows = intersperse rowSpacer rows |> 
                    flow down

boardElements : Board -> Element
boardElements b = board2Lists b |>
                  map squareRow2Element |> 
                  rows2Element


main = boardElements <~ gameState
                         


