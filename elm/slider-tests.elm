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


nextToEmptyTest1 = defaultTest <| assert <| nextToEmpty (Tile 8) start
nextToEmptyTest2 = defaultTest <| assert <| nextToEmpty (Tile 6) start
nextToEmptyTest3 = defaultTest <| assert <| (nextToEmpty (Tile 1) start) == False

neighborsTest1 = let n = neighbors (Tile 1) start
                 in equals n [Tile 2, Tile 4]
neighborsTest2 = let n = neighbors (Tile 5) start
                 in equals n [Tile 2, Tile 4, Tile 6, Tile 8]
neighborsTest3 = let n = neighbors (Tile 8) start
                 in equals n [Tile 5, Tile 7, EmptyTile]
neighborsTest4 = let n = neighbors (Tile 20) start
                 in equals n []
                    

swapWithEmptyTest = let after = swapWithEmpty (Tile 2) <| starterBoard 2 2
                    in
                      equals [{x=0,y=0,tile=Tile 1}
                             ,{x=1,y=0,tile=EmptyTile}
                             ,{x=0,y=1,tile=Tile 3}
                             ,{x=1,y=1,tile=Tile 2}
                             ] after

board2ListsTest = let after = starterBoard 2 2 |> board2Lists
                  in
                    equals [[{x=0,y=0,tile=Tile 1},{x=1,y=0,tile=Tile 2}],
                            [{x=0,y=1,tile=Tile 3},{x=1,y=1,tile=EmptyTile}]] after
                                          
s = suite "Board Functions"
    [
     starterListTest
    ,starterBoardTest
    ,neighborsTest1
    ,neighborsTest2
    ,neighborsTest3
    ,neighborsTest4
    ,nextToEmptyTest1
    ,nextToEmptyTest2
    ,nextToEmptyTest3
    ,swapWithEmptyTest
    ,board2ListsTest
    ]

main = runDisplay s
