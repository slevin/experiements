{-

need to figure out how to transform a set of points/colors
and find if there are any neighboring points of same color
and flag them for delete and then remove them

working back I have a set of positions, I need a set of positions to delete
I need to remove all the set from the total set

1 given list of box and given second list of box create new box - old box
-}

import Set (diff, fromList, toList)

type BoxModel = { x:Int, y:Int, color:Color }

b1 : BoxModel
b1 = BoxModel 0 0 red
b2 = BoxModel 1 0 red
b3 = BoxModel 2 0 red
b4 = BoxModel 0 1 red

s1 = [b1, b2, b3]
s2 = [b2, b3]

s3 : [BoxModel] -> [BoxModel] -> [BoxModel]
s3 total minus = (fromList total) `diff` (fromList minus)

--main = toText "hello"
