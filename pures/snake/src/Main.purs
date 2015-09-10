module Main where

import Prelude
import Data.Maybe

import Graphics.Canvas
import DOM.Timer
import Control.Monad.Eff
import Control.Monad.ST
import Control.Monad.Eff.JQuery hiding (append)
import Control.Monad.Eff.Console
import DOM

foreign import jqwhich :: forall eff. JQueryEvent -> Eff (dom :: DOM | eff) String

data Direction = Left | Up | Right | Down

type Move =
  { x :: Number
  , y :: Number
  }

type GameState =
  { x :: Number
  , y :: Number
  , w :: Number
  , h :: Number
  , dir :: Direction
  }


directionToMove :: Direction -> Move
directionToMove Left =  { x: -1.0, y:  0.0 }
directionToMove Up =    { x:  0.0, y: -1.0 }
directionToMove Right = { x:  1.0, y:  0.0 }
directionToMove Down  = { x:  0.0, y:  1.0 }


initialState :: GameState
initialState = { x: (areaSize / 2.0) - (bitSize / 2.0)
               , y: (areaSize / 2.0) - (bitSize / 2.0)
               , w: bitSize
               , h: bitSize
               , dir: Down
               }

areaSize = 100.0
sqSize = areaSize / 5.0
bitSize = sqSize - 2.0

moveSnake :: GameState -> GameState
moveSnake state = { x: state.x + (mv.x * sqSize)
                  , y: state.y + (mv.y * sqSize)
                  , w: state.w
                  , h: state.h
                  , dir: state.dir
                  }
  where
    mv = directionToMove state.dir

main :: forall h. Eff (canvas :: Canvas, st :: ST h, timer :: Timer, dom :: DOM, console :: CONSOLE) Unit
main = do
  Just canvas <- getCanvasElementById "canvas"
  dim <- getCanvasDimensions canvas
  ctx <- getContext2D canvas


  st <- newSTRef initialState

  body <- body
  on "keydown" onKey body

  inter <- interval 500 $ do
    st2 <- readSTRef st
    setFillStyle "#FF0000" ctx
    fillPath ctx $ rect ctx { x:0.0, y:0.0, w:dim.width, h:dim.height }
    setFillStyle "#0000FF" ctx
    fillPath ctx $ rect ctx { x:st2.x, y:st2.y, w:st2.w, h:st2.h }
    modifySTRef st \st1 -> moveSnake st1
    return unit
  return unit

onKey :: forall eff a h. JQueryEvent -> JQuery -> Eff (dom :: DOM, console :: CONSOLE | eff) Unit
onKey evt _ = do
  --37 8 9 40 left up right down
  code <- jqwhich evt
  log code


-- pass in state ugh
-- now code to direction
  -- and put direction in the state
-- how do I want to modify?
-- need a way to compile it and open in a browser
-- compile means pulp browserify > and then open something
  -- would be nice to have some sort of project thing I can think about that
