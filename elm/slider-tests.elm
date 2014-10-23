--module Main where

import Slider (..)
import ElmTest.Assertion (..)
import ElmTest.Runner.Element (..)
import ElmTest.Test (..)

sq1 = {x=0,y=0,tile=Tile 1}
sq2 = {x=1,y=0,tile=Tile 2}
sq3 = {x=0,y=1,tile=Tile 3}            
sq4 = {x=1,y=1,tile=EmptyTile}
      
starterListTest = let after = starterList 2 2
                  in equals after [[Tile 1, Tile 2],[Tile 3, EmptyTile]]

starterBoardTest = let after = starterBoard 2 2
                   in equals [sq1, sq2, sq3, sq4] after

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
                      equals [sq1
                             ,{x=1,y=0,tile=EmptyTile}
                             ,sq3
                             ,{x=1,y=1,tile=Tile 2}
                             ] after

spbTest1 = let after = squarePartitionedByY sq1 []
           in
             equals [[sq1]] after

spbTest2 = let after = squarePartitionedByY sq2 [[sq1]]
           in
             equals [[sq1, sq2]] after

spbTest3 = let after = squarePartitionedByY sq3 [[sq1]]                    
           in
             equals [[sq1],[sq3]] after

spbTest4 = let after = squarePartitionedByY sq2 [[sq1], [sq3]]
           in
             equals [[sq1, sq2], [sq3]] after

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
    ,spbTest1
    ,spbTest2
    ,spbTest3
    ,spbTest4
    ,board2ListsTest
    ]

main = runDisplay s
