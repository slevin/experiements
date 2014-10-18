{-

slider puzzle 3 x 3
one missing space

start with solution


-}


------------
-- Model

type Square = Int
type Row = [Square]
type Board = [Row]

empty : Square
empty = 0

start : Board
start = [[1,2,3], [4,5,6], [7,8,empty]]


-- helper for replacing empty and swapper in row
swapSquare : Square -> Square -> Square
swapSquare swapper actual = case actual of
                              swapper -> empty
                              empty -> swapper
                              otherwise -> actual

-- swap takes board and number and swaps with empty and returns new board
swap : Square -> Board -> Board
swap s b = map (\row -> map (\sq -> swapSquare s sq) row) b

-- shuffle finds empty and uses input to pick direction for moving
--shuffle : Float -> Board -> Board



-----------
-- View




----------
-- Control

