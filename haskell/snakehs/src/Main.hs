{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad
import Linear
import Linear.Affine ( Point(P) )
import SDL (($=))
import qualified SDL
import Data.IORef
import Data.Word
import Data.Maybe
import Data.List (uncons)
import System.Random
import Foreign.C.Types
import Control.Lens

gridSize = 25 :: Int


data Direction = L | U | R | D
               deriving (Eq, Show)

type Snake = [V2 Int]
type Food = Maybe (V2 Int)
type MovingDirection = Maybe Direction
type Randoms = [Int]
data GameState = GameState { _snake :: Snake
                           , _food :: Food
                           , _direction :: MovingDirection
                           , _ticks :: Word32
                           , _randomInts :: Randoms
                           } deriving (Eq, Show)

makeClassy ''GameState

printableState :: GameState -> (Snake, Food, MovingDirection, Word32)
printableState gs = (_snake gs, _food gs, _direction gs, _ticks gs)

key2Direction :: SDL.EventPayload -> Maybe Direction
key2Direction (SDL.KeyboardEvent ked)
  | sym == 80 = Just L
  | sym == 82 = Just U
  | sym == 79 = Just R
  | sym == 81 = Just D
  where sym = SDL.unwrapScancode $ SDL.keysymScancode $ SDL.keyboardEventKeysym ked
key2Direction _ = Nothing

moveSnakeInDirection :: MovingDirection -> Snake -> Snake
moveSnakeInDirection (Just d) (s:ss) = (case d of
                                          L -> s + (V2 (-1) 0)
                                          R -> s + (V2 1 0)
                                          U -> s + (V2 0 (-1))
                                          D -> s + (V2 0 1)):s:ss
moveSnakeInDirection Nothing s = s


-- functional "update" that says given a snake (already moved forward one)
-- if it moved onto a food, remove the food otherwise remove the last item
-- to make it seem like it moved
nextSnakeAndFood :: (Food, Snake) -> (Food, Snake)
nextSnakeAndFood (Just f, sn@(s:ss)) = if f == s
                                       then (Nothing, sn)
                                       else (Just f, init sn)
nextSnakeAndFood (Nothing, sn) = (Nothing, init sn)
nextSnakeAndFood (fd, s) = (fd, s)



updateFood :: GameState -> GameState
updateFood gs = let (fd, rs) = updateFood' (_food gs, _randomInts gs) in
  gs { _food=fd, _randomInts=rs }

updateFood' :: (Food, Randoms) -> (Food, Randoms)
-- if no food then make it somewhere, and return rest of randoms
updateFood' (Nothing, r1:r2:rs) = (Just (V2 r1 r2), rs)
-- if food exists then just keep going the same
updateFood' (f, rs) = (f, rs)

updateDirection :: MovingDirection -> GameState -> GameState
updateDirection Nothing st = st
updateDirection newDirection st = st { _direction=newDirection }

updateSnakeAndFood :: Word32 -> GameState -> GameState
updateSnakeAndFood newTicks currentState =
  if newTicks > (_ticks currentState) + 400
  then let (newFood, newSnake) = nextSnakeAndFood (_food currentState, moveSnakeInDirection (_direction currentState) (_snake currentState)) in
  currentState { _snake=newSnake, _food=newFood, _ticks=newTicks }
  else currentState

updateState :: MovingDirection -> Word32 -> GameState -> GameState
updateState newDirection newTicks currentState =
  updateFood $
  updateSnakeAndFood newTicks $
  updateDirection newDirection currentState

main :: IO ()
main = do
  SDL.initialize [ SDL.InitEverything ]

  let winConfig = SDL.defaultWindow { SDL.windowPosition = SDL.Absolute (P (V2 100 100))
                                    , SDL.windowInitialSize = V2 640 480 }

      rdrConfig = SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedVSyncRenderer
                                     , SDL.rendererTargetTexture = True }

  window <- SDL.createWindow "Hello world" winConfig
  renderer <- SDL.createRenderer window (-1) rdrConfig


  let loop currentState = do
        -- get events
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        let newDir =  listToMaybe $ reverse $ mapMaybe key2Direction $ map SDL.eventPayload events

        -- update state
        newTicks <- SDL.ticks
        let newState = updateState newDir newTicks currentState
        --print $ printableState newState

        -- clear the drawing
        SDL.rendererDrawColor renderer $= V4 0 0 0 255
        SDL.clear renderer

        -- draw a food
        case _food newState of
          Just f -> do
            SDL.rendererDrawColor renderer $= V4 184 233 134 255
            SDL.fillRect renderer $ Just $ SDL.Rectangle (P (fmap (CInt . fromIntegral) ((* gridSize) <$> f))) (fmap (CInt . fromIntegral) (V2 gridSize gridSize))
          Nothing -> return ()


        -- draw the snake
        SDL.rendererDrawColor renderer $= V4 248 231 28 255
        forM_ (_snake newState) $ (\s ->
          SDL.fillRect renderer $ Just $ SDL.Rectangle (P (fmap (CInt . fromIntegral) ((* gridSize) <$> s))) (fmap (CInt . fromIntegral) (V2 gridSize gridSize)))
        SDL.present renderer

        unless quit $ loop newState

  -- start loop with initial state
  rds <- fmap (randomRs (0, 10)) getStdGen
  loop GameState { _snake=[V2 0 0]
                 , _food=Nothing
                 , _direction=Just R
                 , _ticks=0
                 , _randomInts=rds
                 }


  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
