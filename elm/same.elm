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
- draw 10 x 5 grid of squares same color with gaps
- draw random colors
- add some randomness to heights
- respond to click
- check neighbors
- remove those guys if necessary
- redraw squares

small steps:
- draw a square
-}

width = 500
height = 250
sq = 40
sp = 2
box color = filled color (square sq)
offsetBL idxX idxY = move ((sq / 2) - (width / 2) + (sq * idxX) + (sp * idxX) , 
                           (sq / 2) - (height / 2) + (sq * idxY) + (sp * idxY))
main = collage width height [ offsetBL 0 0 (box red)
                            , offsetBL 0 1 (box red)
                            , offsetBL 1 0 (box red)]
