module Main where

import Prelude
import Data.Maybe
import Graphics.Canvas
import DOM.Timer
import Control.Monad.Eff
import Control.Monad.ST

type GameState =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  }

initialState :: GameState
initialState = { x: (areaSize / 2.0) - (bitSize / 2.0)
                , y: (areaSize / 2.0) - (bitSize / 2.0)
                , w: bitSize
                , h: bitSize
                }

areaSize = 100.0
sqSize = areaSize / 5.0
bitSize = sqSize - 2.0


main :: forall h. Eff (canvas :: Canvas, st :: ST h, timer :: Timer) Unit
main = do
  Just canvas <- getCanvasElementById "canvas"
  dim <- getCanvasDimensions canvas
  ctx <- getContext2D canvas


  st <- newSTRef initialState

  inter <- interval 500 $ do
    st2 <- readSTRef st
    setFillStyle "#000000" ctx
    fillPath ctx $ rect ctx { x:0.0, y:0.0, w:dim.width, h:dim.height }
    setFillStyle "#0000FF" ctx
    fillPath ctx $ rect ctx st2
    modifySTRef st \st1 -> { x: st1.x + sqSize
                           , y: st1.y
                           , w: st1.w
                           , h: st1.h
                           }
    return unit
  return unit
