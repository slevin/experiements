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

-- searches through board and finds a list of tiles neighboring
-- the given tile
neighbors : Tile -> Board -> [Tile]
neighbors t b = allSquares b |>  
                filter (\sq -> sq.tile == t) |>
                take 1 |>
                map (\ts -> neighbors' ts.x ts.y b) |>
                concat |>
                map (\sq -> sq.tile)

-- helper function that finds neighboring tiles to a position
neighbors' : Int -> Int -> Board -> [Square]
neighbors' x y b = allSquares b |> 
                   filter (\sq -> (sq.x == x && sq.y == (y - 1)) ||
                           (sq.x == x && sq.y == (y + 1)) ||
                           (sq.x == (x - 1) && sq.y == y) ||
                           (sq.x == (x + 1) && sq.y == y))


-- finds neighboring tiles and checks if any of them are empty
nextToEmpty : Tile -> Board -> Bool
nextToEmpty t b = any (\tile -> tile == EmptyTile) <| neighbors t b


-- if given tile is next to empty swap with that
swapWithEmpty : Tile -> Board -> Board
swapWithEmpty t b = if | nextToEmpty t b -> swapWithEmpty' t b
                       | otherwise -> b

-- helper function to do the swapping with the empty tile
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

-- create a ui element for a given square
square2Element : Tile -> Square -> Element
square2Element h sq = case sq.tile of
                        EmptyTile -> spacer sqSz sqSz
                        Tile x -> [show x |> toText |> centered,
                                   spacer sqSz sqSz |> 
                                   if | h == sq.tile -> color yellow
                                      | otherwise -> color red] |> 
                                  flow inward |>
                                  clickable tileClick.handle sq.tile

columnSpacer : Element
columnSpacer = spacer sqSp sqSz

-- make a row of elements for a row of squares
squareRow2Element : Tile -> [Square] -> Element
squareRow2Element h sqs = map (square2Element h) sqs |> 
                          intersperse columnSpacer |> 
                          flow right

rowSpacer : Element
rowSpacer = spacer sqSz sqSp

-- turn a set of element rows into a single element
rows2Element : [Element] -> Element
rows2Element rows = intersperse rowSpacer rows |> 
                    flow down

-- turn a set of rows in a board to a set of elements
boardElements : Board -> Tile -> Element
boardElements b h = map (squareRow2Element h) b |>
                    rows2Element


main = boardElements <~ gameState ~ (constant (Tile 2))
                         


