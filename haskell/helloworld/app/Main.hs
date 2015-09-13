{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import Control.Concurrent (threadDelay)
import Foreign.C.Types
import Foreign.C.String
import Linear
import qualified Graphics.UI.SDL as SDL

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.init SDL.SDL_INIT_VIDEO
  title <- newCString "SDL Example"
  window <- SDL.createWindow title 100 100 screenWidth screenHeight SDL.SDL_WINDOW_SHOWN
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window
  screenSurfaceFormat <- SDL.surfaceFormat screenSurface
  white <- SDL.mapRGB screenSurfaceFormat 255 255 255
  SDL.fillRect screenSurface (SDL.Rect { SDL.rectX = 100, SDL.rectY = 100, SDL.rectW = 100, SDL.rectH = 100 }) white
  SDL.updateWindowSurface window

  threadDelay 2000000
  SDL.destroyWindow window
  SDL.quit
