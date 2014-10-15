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

done
* starting point set
* draw boxes
* given one box find matching set to remove
* can find box from mouse

----
todo

* click on point
 find box in state (state is list of boxes)
 find matches
 update state to remove matches if matches size > 1
 render state
 

* how to highlight matches, hmmm tricky
 since each item would have a signal for a color, but maybe that's ok
 not sure how yet (skip maybe)

* adjust points so that they "fall down" and "fall left"
 only remove if group is larger than 1
  (put into lists and back and I get it forfree)

* animate highlighting of matching points

* animate falling down

* how do I get interaction with mouse click
 do I add a listener for each item or something?
 what about hover


-}

import Random (floatList)
import Array
import Mouse
import Generator
import Generator.Standard

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

mousePos2Pos : (Int, Int) -> (Int, Int)
mousePos2Pos (x, y) = let posX = toFloat x |> (flip (/) (sq + sp)) |> floor
                          posY = toFloat y |> ((-) height) |> (flip (/) (sq + sp)) |> floor
                      in
                        (posX, posY)


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

posSignal : Signal (Int, Int)
posSignal = lift mousePos2Pos Mouse.position

posElement : Signal Element
posElement = lift show posSignal |> lift toText |> lift leftAligned


boxEquals : BoxModel -> BoxModel -> Bool
boxEquals b1 b2 = b1.x == b2.x && b1.y == b2.y

boxInBoxList : BoxModel -> [BoxModel] -> Bool
boxInBoxList box list = case list of
                          [] -> False
                          (b::bs) -> if | boxEquals box b -> True
                                        | otherwise -> boxInBoxList box bs

-- provide remaining list of first list minus second list
boxListDiff : [BoxModel] -> [BoxModel] -> [BoxModel]
boxListDiff group rest = filter (\x -> (boxInBoxList x rest) == False) group

-- possibleNieghbors
isMatchingNeighbor : BoxModel -> BoxModel -> Bool
isMatchingNeighbor me her = me.color == her.color &&
                            ((me.x == her.x && (me.y == her.y + 1 || me.y == her.y - 1)) ||
                             (me.y == her.y && (me.x == her.x + 1 || me.x == her.x - 1)))


-- find matching neighbors
-- me, all items, set of boxes that match including me
findMatchingNeighbors : BoxModel -> [BoxModel] -> [BoxModel]
findMatchingNeighbors me all = filter (\x -> isMatchingNeighbor me x) all

type CheckGraph = { checked:[BoxModel], possible:[BoxModel] }

-- find all that match and are in a chain
-- uses a graph to traverse neighbors and
-- mark viewed as viewed
findMatchingGroup : BoxModel -> [BoxModel] -> [BoxModel]
findMatchingGroup me all = findMatchingGroup' all (CheckGraph [] [me])

findMatchingGroup' : [BoxModel] -> CheckGraph -> [BoxModel]
findMatchingGroup' all graph = case graph.possible of
                                 [] -> graph.checked
                                 (b::bs) -> let matches = findMatchingNeighbors b all
                                                remaining = boxListDiff matches (graph.checked ++ graph.possible)
                                            in
                                              findMatchingGroup' all (CheckGraph (b::graph.checked) (bs ++ remaining))



-- given position and all boxmodels find matching
posFindBox : (Int, Int) -> [BoxModel] -> [BoxModel]
posFindBox (x, y) all = filter (\b -> b.x == x && b.y == y) all

{-
  lift pos2Box signal int,int
-}

--findBox : Signal [BoxModel]
--findBox sbs = (sampleOn Mouse.clicks Mouse.position)

main = let 
           floats = fst <| Generator.listOf Generator.float 100 (Generator.Standard.generator 100)
           all = allSquares floats
           boxes = collage width height all
           combined = combine [(constant boxes), posElement]
       in
         lift2 flow (constant down) combined

