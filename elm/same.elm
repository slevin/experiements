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

main = collage 500 250