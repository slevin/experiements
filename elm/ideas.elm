{-
done
* starting point set
* draw boxes
* given one box find matching set to remove


----
todo

* I could figure out how to draw the matches by
 turning yellow set matching one hovered over


* adjust points so that they "fall down" and "fall left"
 only remove if group is larger than 1
  (put into lists and back and I get it forfree)


* animate highlighting of matching points

* animate falling down

* how do I get interaction with mouse click
 do I add a listener for each item or something?
 what about hover


iterate for matching points do graph thing I think
 take current, get potential neighbors
 add to list of checked
 check each potential neighbor if same color, and not already checked
    add it to list of to check
    

-}

type BoxModel = { x:Int, y:Int, color:Color }

b1 : BoxModel
b1 = BoxModel 0 0 red
b2 = BoxModel 1 0 red
b3 = BoxModel 2 0 red
b4 = BoxModel 0 1 red
b5 = BoxModel 0 2 green

s1 = [b1, b2, b3]
s2 = [b1, b3]
s3 = [b1, b2, b3, b4, b5]

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

main = leftAligned <| toText <| show <| findMatchingGroup b5 s3
