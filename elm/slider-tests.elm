--module Main where

import Slider (..)
import ElmTest.Assertion (..)
import ElmTest.Runner.Element (..)
import ElmTest.Test (..)

sq1 = {x=0,y=0,tile=Tile 1}
sq2 = {x=1,y=0,tile=Tile 2}
sq3 = {x=0,y=1,tile=Tile 3}            
sq4 = {x=1,y=1,tile=EmptyTile}
      
starterBoardTest = let after = starterBoard 2 2
                   in
                     equals [[sq1, sq2], [sq3, sq4]] after

testStart = starterBoard 3 3
nextToEmptyTest1 = defaultTest <| assert <| nextToEmpty (Tile 8) testStart
nextToEmptyTest2 = defaultTest <| assert <| nextToEmpty (Tile 6) testStart
nextToEmptyTest3 = defaultTest <| assert <| (nextToEmpty (Tile 1) testStart) == False

neighborsTest1 = let n = neighbors (Tile 1) testStart
                 in equals n [Tile 2, Tile 4]
neighborsTest2 = let n = neighbors (Tile 5) testStart
                 in equals n [Tile 2, Tile 4, Tile 6, Tile 8]
neighborsTest3 = let n = neighbors (Tile 8) testStart
                 in equals n [Tile 5, Tile 7, EmptyTile]
neighborsTest4 = let n = neighbors (Tile 20) testStart
                 in equals n []
                    

swapWithEmptyTest = let after = swapWithEmpty (Tile 2) <| starterBoard 2 2
                    in
                      equals [[sq1,{x=1,y=0,tile=EmptyTile}],
                              [sq3,{x=1,y=1,tile=Tile 2}]
                             ] after

s = suite "Board Functions"
    [
     starterBoardTest
    ,neighborsTest1
    ,neighborsTest2
    ,neighborsTest3
    ,neighborsTest4
    ,nextToEmptyTest1
    ,nextToEmptyTest2
    ,nextToEmptyTest3
    ,swapWithEmptyTest
    ]

main = runDisplay s
