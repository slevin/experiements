{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Linear
import Linear.Affine ( Point(P) )
import SDL (($=))
import qualified SDL
import Data.IORef
import Data.Word

-- data GameState = GameState {
--                            }
main :: IO ()
main = do
  SDL.initialize [ SDL.InitEverything ]

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowInitialSize = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello world" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig

  lstRef <- newIORef (0 :: Word32)

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        lst <- readIORef lstRef
        ts <- SDL.ticks
        if ts > lst + 800
          then do writeIORef lstRef ts
                  putStrLn "writing"
          else do return ()

        SDL.rendererDrawColor renderer $= V4 0 0 0 255
        SDL.clear renderer

        SDL.rendererDrawColor renderer $= V4 248 231 28 255
        SDL.fillRect renderer $ Just $ SDL.Rectangle (P (V2 100 100)) (V2 50 50)
        SDL.present renderer

        unless quit loop
  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
