module Slider where

------------
-- Model
import Array (fromList, get)
import Graphics.Input (Input, input, clickable, hoverable)
import Generator
import Generator.Standard

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

-- tile lists membership test 
inTiles : Tile -> [Tile] -> Bool
inTiles t ts = (filter (\x -> x == t) ts |> length) > 0

-- if event is an off remove the off tile
-- if an event is an on  make sure it contains the tile
updateHovers : TileHoverEvent -> [Tile] -> [Tile]
updateHovers event tiles = let others = filter (\t -> t /= event.tile) tiles
                           in 
                             if | event.hoverState -> event.tile :: others
                                | otherwise -> others

-- shuffle board based on feed of random numbers
shuffleBoard : [Float] -> Board -> Board
shuffleBoard fs b = foldl shuffleOnce b fs

-- pick a neighbor of EmptyTile based on Float
-- and return the board with it swapped
shuffleOnce : Float -> Board -> Board
shuffleOnce f b = let emptyNeighbors = neighbors EmptyTile b
                      idx = length emptyNeighbors |> toFloat |> (*) f |> floor
                      tileNum = fromList emptyNeighbors |> get idx
                  in
                    case tileNum of
                      Just (Tile num) -> swapWithEmpty (Tile num) b
                      Nothing -> b

{-


then see if I can place those in a collage
 instead of flow of elements
 (so I can get the input)

then I can fake a slide then swap when its done



then try for animations with swapping

animation is foldp over whatever fps




after hovering we need to somehow signal
 an update to hover (because it moves and I lose it)
 hover is a mix of hover signal and postclick signal
  clicking updates postclick with an off event for that tile
  but it should only update if its a move

center it, make it look nice, maybe give it images from real game

-}

----------
-- Control

tileClick : Input Tile
tileClick = input EmptyTile

type TileHoverEvent = { tile:Tile, hoverState:Bool }

tileHover : Input TileHoverEvent
tileHover = input <| TileHoverEvent EmptyTile False

floatList : [Float]
floatList = fst <| Generator.listOf Generator.float 10 (Generator.Standard.generator 100)

absoluteStart : Board
absoluteStart = starterBoard 3 3

shuffledBoard : Board
shuffledBoard = absoluteStart |> shuffleBoard floatList

gameState : Signal Board
gameState = foldp swapWithEmpty shuffledBoard tileClick.signal

hoverState : Signal [Tile]
hoverState = foldp updateHovers [] tileHover.signal

-----------
-- View

sqSz : Int
sqSz = 160

gap : Int
gap = 6

sqSp : Int
sqSp = 6

-- create a ui element for a given square

square2Element : [Tile] -> Square -> Element
square2Element hs sq = case sq.tile of
                        EmptyTile -> spacer sqSz sqSz
                        Tile x -> image (sqSz - gap) (sqSz - gap) ("./tiles/" ++ (show x) ++ ".jpg") |>
                                  container sqSz sqSz middle |>
                                  clickable tileClick.handle sq.tile |>
                                  hoverable tileHover.handle (\b -> TileHoverEvent sq.tile b) |>
                                  if | inTiles sq.tile hs -> color yellow
                                     | otherwise -> color red
-- |> 

columnSpacer : Element
columnSpacer = spacer sqSp sqSz

-- make a row of elements for a row of squares
squareRow2Element : [Tile] -> [Square] -> Element
squareRow2Element hs sqs = map (square2Element hs) sqs |> 
                          intersperse columnSpacer |> 
                          flow right

rowSpacer : Element
rowSpacer = spacer sqSz sqSp

-- turn a set of element rows into a single element
rows2Element : [Element] -> Element
rows2Element rows = intersperse rowSpacer rows |> 
                    flow down

-- turn a set of rows in a board to a set of elements
boardElements : Board -> [Tile] -> Element
boardElements b hs = map (squareRow2Element hs) b |>
                    rows2Element


-- board or "you win!"
gameElements : Board -> [Tile] -> Element
gameElements b hs = if | b == absoluteStart -> plainText "You Win!"
                       | otherwise -> boardElements b hs

main = gameElements <~ gameState ~ hoverState
                         


