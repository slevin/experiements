--module Main where

import Slider (..)
import ElmTest.Assertion (..)
import ElmTest.Runner.Element (..)
import ElmTest.Test (..)

{-
assertEqualList : [comparable] -> [comparable] -> Assertion
assertEqualList l1 l2 = assertEqual (sort l1) (sort l2)

swapHorizontalTest = let 
                         after = swap 8 start
                     in
                       equals after [[1,2,3],[4,5,6],[7,emptySquare,8]]

swapVerticalTest = let 
                       after = swap 6 start
                   in
                     equals after [[1,2,3],[4,5,emptySquare],[7,8,6]]

findNeighborsTest = let found = findNeighbors emptySquare start
                    in defaultTest <| assertEqualList found [8,6]

-}

starterListTest = let after = starterList 2 2
                  in equals after [[Tile 1, Tile 2],[Tile 3, EmptyTile]]

starterBoardTest = let after = starterBoard 2 2
                   in equals [{x=0,y=0,tile=Tile 1}
                             ,{x=1,y=0,tile=Tile 2}
                             ,{x=0,y=1,tile=Tile 3}
                             ,{x=1,y=1,tile=EmptyTile}
                             ] after

s = suite "Board Functions"
    [
     starterListTest
    ,starterBoardTest
    ]

main = runDisplay s
