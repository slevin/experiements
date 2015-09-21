{-# LANGUAGE OverloadedStrings #-}
module Main where

import Linear
import Linear.Affine ( Point(P) )
import SDL (($=))
import qualified SDL

main :: IO ()
main = do
  SDL.initialize [ SDL.InitEverything ]

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowInitialSize = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello world" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 255 0 0 255
  SDL.fillRect renderer $ Just $ SDL.Rectangle (P (V2 100 100)) (V2 50 50)
  SDL.present renderer

  SDL.delay 2000

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
