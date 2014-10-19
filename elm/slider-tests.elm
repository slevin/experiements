--module Main where

import Slider (..)
import ElmTest.Assertion (..)
import ElmTest.Runner.Element (..)
import ElmTest.Test (..)

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

s = suite "Board Functions"
    [
     swapHorizontalTest
    ,swapVerticalTest
    ]

main = runDisplay s
