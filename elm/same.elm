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
- respond to click
- check neighbors
- remove those guys if necessary
- redraw squares

small steps:


limit size to fit area


then I need to set up some response to mouse tap

which can somehow build up a list of things that will disappear
then need to remove those from the list of everything and rerender
the state crazy

my colors are biased towards 1 because I'm not collecting between each one
I should to 0 to 3 and round down instead
-}

import Random (floatList)
import Array

width = 500
height = 250
sq = 40
sp = 2

type BoxModel = { x:Int, y:Int, color:Color }
type RandomList = [Float]
type RandomArray = Array.Array Float

box color = filled color (square sq)
offsetBL (idxX, idxY) = move ((sq / 2) - (width / 2) + (sq * idxX) + (sp * idxX) , 
                           (sq / 2) - (height / 2) + (sq * idxY) + (sp * idxY))

numarray : RandomList -> [Int]
numarray fs = zipWith (\x y -> floor <| x * 5) fs [0..9]

pos2Color : RandomArray -> (Int, Int) -> Color
pos2Color arr (x, y) = let n = Array.getOrElse 0 (x*9 + y) arr in
                           num2Color n
                           
pos2Box : RandomArray -> Int -> Int -> BoxModel
pos2Box ars x y = BoxModel x y (pos2Color ars (x, y))

columns2Boxes : RandomList -> [Int] -> [[BoxModel]]
columns2Boxes rs arr = let ars = Array.fromList rs in
                           indexedMap (\idx num -> map (\y -> (pos2Box ars idx y)) [0..num]) arr

allBoxes : RandomList -> [BoxModel]
allBoxes rs = concat <| columns2Boxes rs <| numarray rs

num2Color : Float -> Color
num2Color f = let n = floor <| f * 3 in
              if | n == 0 -> red
                 | n == 1 -> green
                 | otherwise -> blue

box2Square : BoxModel -> Form
box2Square b = offsetBL (toFloat <| b.x, toFloat <| b.y) (box b.color)

allSquares : RandomList -> [Form]
allSquares rs = map box2Square <| allBoxes rs

randomSignal : Signal [Float]
randomSignal = floatList (constant 100)

main = let 
           all = lift allSquares randomSignal 
       in
           lift3 collage (constant width) (constant height) all

