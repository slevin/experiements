{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Linear
import Linear.Affine ( Point(P) )
import SDL (($=))
import qualified SDL
import Data.IORef
import Data.Word
import Data.Maybe


data Direction = L | U | R | D
               deriving(Show)

-- turn an event into a possible direction if its an arrow key
--key2Code :: SDL.EventPayload -> Maybe Int32
key2Code (SDL.KeyboardEvent ked) = Just $ SDL.unwrapScancode $ SDL.keysymScancode $ SDL.keyboardEventKeysym ked
key2Code _ = Nothing

key2Direction :: SDL.EventPayload -> Maybe Direction
key2Direction (SDL.KeyboardEvent ked)
  | sym == 80 = Just L
  | sym == 82 = Just U
  | sym == 79 = Just R
  | sym == 81 = Just D
  where sym = SDL.unwrapScancode $ SDL.keysymScancode $ SDL.keyboardEventKeysym ked
key2Direction _ = Nothing


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
  offRef <- newIORef (0, 0)
  dirRef <- newIORef R

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        let dirs = mapMaybe key2Direction $ map SDL.eventPayload events

        forM_ dirs $ writeIORef dirRef
        currentDir <- readIORef dirRef

        -- putStrLn $ show dirs -- $ map key2Code $ map SDL.eventPayload events

        lst <- readIORef lstRef
        ts <- SDL.ticks
        if ts > lst + 400
          then do writeIORef lstRef ts
                  modifyIORef offRef (\x -> case currentDir of
                                        L -> (fst x - 50, snd x)
                                        R -> (fst x + 50, snd x)
                                        U -> (fst x, snd x - 50)
                                        D -> (fst x, snd x + 50))
          else do return ()

        SDL.rendererDrawColor renderer $= V4 0 0 0 255
        SDL.clear renderer

        SDL.rendererDrawColor renderer $= V4 248 231 28 255
        off <- readIORef offRef
        SDL.fillRect renderer $ Just $ SDL.Rectangle (P (V2 (100 + fst off) (100 + snd off))) (V2 50 50)
        SDL.present renderer

        unless quit loop
  loop

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
