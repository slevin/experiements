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
- add some randomness to heights
- respond to click
- check neighbors
- remove those guys if necessary
- redraw squares

small steps:
method to generate pairs of x,ys 00, 01, 10, 11 ec
and map offsetbl over those

then assign random colors over those

limit size to fit area
-}

width = 500
height = 250
sq = 40
sp = 2
box color = filled color (square sq)
offsetBL (idxX, idxY) = move ((sq / 2) - (width / 2) + (sq * idxX) + (sp * idxX) , 
                           (sq / 2) - (height / 2) + (sq * idxY) + (sp * idxY))

pairs = map (\x -> map (\y -> (x, y)) [0..4]) [0..9]
allPairs = concat pairs
squares = map (\pair -> offsetBL pair (box red)) allPairs
main = collage width height squares
