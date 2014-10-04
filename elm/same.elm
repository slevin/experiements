{-
Same Game implementation.

5 high by 10 wide squares of 3 colors
starts with random color assignment

click on one of those squares
if there is at least one neighboring square (nsew), disappears those squares

remaining square fall down and to the left (in that order)
until all are gone
or no more squares have neighbors

goal:
- use signals

check list:
- draw random colors
- respond to click
- check neighbors
- remove those guys if necessary
- redraw squares

small steps:
assign random colors over those

limit size to fit area

how do I get random between three colors

now we need to take a random 1 of 4 colors
as I'm building up the positions and create some sort of record
then I can use the color to sort out box

then I need to set up some response to mouse tap

which can somehow build up a list of things that will disappear
then need to remove those from the list of everything and rerender
the state crazy
-}

import Random (range)


width = 500
height = 250
sq = 40
sp = 2
box color = filled color (square sq)
offsetBL (idxX, idxY) = move ((sq / 2) - (width / 2) + (sq * idxX) + (sp * idxX) , 
                           (sq / 2) - (height / 2) + (sq * idxY) + (sp * idxY))

num x = range 1 5 (constant x)
numarray = combine <| map  (\x -> num x) [0..9]

idx2Pos : [Int] -> [[(Int, Int)]]
idx2Pos arr = indexedMap (\idx num -> map (\y -> (idx, y)) [0..num]) arr

allPairs : Signal [(Int, Int)]
allPairs = lift concat (lift idx2Pos numarray)

pair2Square : (Int, Int) -> Form
pair2Square (x, y) = offsetBL (toFloat x, toFloat y) (box red)

squares = lift2 map (constant pair2Square) allPairs
main = lift3 collage (constant width) (constant height) squares
