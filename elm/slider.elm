module Slider where

------------
-- Model
import Array
import Graphics.Input (Input, input, clickable)

data Tile = EmptyTile | Tile Int
type Square = { x:Int, y:Int, tile:Tile }
type Board = [[Square]]

-- Given a row/column count, creates an array of arrays with the squares
starterBoard : Int -> Int -> Board
starterBoard c r = let tileFn num = if | num == c * r -> EmptyTile
                                       | otherwise -> Tile num
                       cr2num col row = ((c * row) + (col + 1))
                       squareFn row col = Square col row (tileFn <| cr2num col row)
                   in 
                     map (\row -> map (squareFn row) [0..(c - 1)]) [0..(r - 1)]


allSquares : Board -> [Square]
allSquares b = concat b

neighbors : Tile -> Board -> [Tile]
neighbors t b = allSquares b |>  
                filter (\sq -> sq.tile == t) |>
                take 1 |>
                map (\ts -> neighbors' ts.x ts.y b) |>
                concat |>
                map (\sq -> sq.tile)

neighbors' : Int -> Int -> Board -> [Square]
neighbors' x y b = allSquares b |> 
                   filter (\sq -> (sq.x == x && sq.y == (y - 1)) ||
                           (sq.x == x && sq.y == (y + 1)) ||
                           (sq.x == (x - 1) && sq.y == y) ||
                           (sq.x == (x + 1) && sq.y == y))


nextToEmpty : Tile -> Board -> Bool
nextToEmpty t b = any (\tile -> tile == EmptyTile) <| neighbors t b


swapWithEmpty : Tile -> Board -> Board
swapWithEmpty t b = if | nextToEmpty t b -> swapWithEmpty' t b
                       | otherwise -> b

swapWithEmpty' : Tile -> Board -> Board
swapWithEmpty' t b = let swapFn sq = if | sq.tile == EmptyTile -> {sq | tile<-t}
                                        | sq.tile == t -> {sq | tile<-EmptyTile}
                                        | otherwise -> sq
                     in
                       map (\row -> map swapFn row) b
{-

detect win condition and show some element that its won

then do some reverse shuffling on start

how about hovering as a highlighting color
 board would have to carry hover state which gets updated
  by signal, and need to figure out foldp based on two signals
  (or pass it all the way in to rendering)

then try for animations with swapping

center it, make it look nice, maybe give it images from real game

could remove partitioning just by keeping board a [[Square]]
 and maping over it to swap double nested map (not hard)
 clean up starter list stuff

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
boardElements b = map squareRow2Element b |>
                  rows2Element


main = boardElements <~ gameState
                         


