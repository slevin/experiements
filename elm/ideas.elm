{-

need to figure out how to transform a set of points/colors
and find if there are any neighboring points of same color
and flag them for delete and then remove them

working back I have a set of positions, I need a set of positions to delete
I need to remove all the set from the total set

1 given list of box and given second list of box create new box - old box

for each item in first list 
if its in second list don't include it

so box in boxlist
-}

import Set (diff, fromList, toList)

type BoxModel = { x:Int, y:Int, color:Color }

b1 : BoxModel
b1 = BoxModel 0 0 red
b2 = BoxModel 1 0 red
b3 = BoxModel 2 0 red
b4 = BoxModel 0 1 red

s1 = [b1, b2, b3]
s2 = [b1, b3]

boxEquals : BoxModel -> BoxModel -> Bool
boxEquals b1 b2 = b1.x == b2.x && b1.y == b2.y

boxInBoxList : BoxModel -> [BoxModel] -> Bool
boxInBoxList box list = case list of
                          [] -> False
                          (b::bs) -> if | boxEquals box b -> True
                                        | otherwise -> boxInBoxList box bs

boxListDiff : [BoxModel] -> [BoxModel] -> [BoxModel]
boxListDiff group rest = filter (\x -> (boxInBoxList x rest) == False) group

s3 = boxListDiff s1 s2

main = leftAligned <| toText <| show s3

--main = toText "hello"
