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
import Data.List (uncons)
import System.Random
import Foreign.C.Types (CInt)

gridSize = 25


data Direction = L | U | R | D
               deriving(Show)
type Snake = [(CInt, CInt)]
type Food = Maybe (CInt, CInt)
type MovingDirection = Maybe Direction
type Randoms = [CInt]
data GameState = GameState { snake :: Snake
                           , food :: Food
                           , direction :: MovingDirection
                           , ticks :: Word32
                           , randoms :: Randoms
                           } deriving (Show)

-- turn an event into a possible direction if its an arrow key
--key2Code :: SDL.EventPayload -> Maybe Int32
--key2Code (SDL.KeyboardEvent ked) = Just $ SDL.unwrapScancode $ SDL.keysymScancode $ SDL.keyboardEventKeysym ked
--key2Code _ = Nothing

key2Direction :: SDL.EventPayload -> Maybe Direction
key2Direction (SDL.KeyboardEvent ked)
  | sym == 80 = Just L
  | sym == 82 = Just U
  | sym == 79 = Just R
  | sym == 81 = Just D
  where sym = SDL.unwrapScancode $ SDL.keysymScancode $ SDL.keyboardEventKeysym ked
key2Direction _ = Nothing

moveSnakeInDirection :: Direction -> Snake -> Snake
moveSnakeInDirection dir (s:ss) = (case dir of
                                     L -> (fst s - gridSize, snd s)
                                     R -> (fst s + gridSize, snd s)
                                     U -> (fst s, snd s - gridSize)
                                     D -> (fst s, snd s + gridSize)):s:ss
moveSnakeInDirection _ _ = []


-- functional "update" that says given a snake (already moved forward one)
-- if it moved onto a food, remove the food otherwise remove the last item
-- to make it seem like it moved
nextSnakeAndFood :: (Food, Snake) -> (Food, Snake)
nextSnakeAndFood (fd, sn) = case uncons sn of
  Just (s, _) -> (case fd of
                     Just (fx, fy) -> (if fst s == fx && snd s == fy
                                       then (Nothing, sn)
                                       else (fd, init sn))
                     Nothing -> (fd, init sn))
  Nothing -> (fd, sn)


updateFood :: GameState -> GameState
updateFood currentState
-- if no food then make it somewhere, and return rest of randoms
updateFood (Nothing, r1:r2:rs) = (Just (r1 * gridSize, r2 * gridSize), rs)
-- if food exists then just keep going the same
updateFood (f, rs) = (f, rs)

updateDirection :: MovingDirection -> GameState -> GameState
updateDirection Nothing st = st
updateDirection newDirection st = st { direction=newDirection }

updateSnakeAndFood :: Word32 -> GameState -> GameState
updateSnakeAndFood newTicks currentState =
  if newTicks > (ticks currentState) + 400
  then let (newSnake, newFood) = nextSnakeAndFood (food currentState, moveSnakeInDirection (direction currentState) (snake currentState)) in
  currentState { snake=newSnake, food=newFood }
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
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents

        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
        let newDir =  listToMaybe $ reverse $ mapMaybe key2Direction $ map SDL.eventPayload events

        newTicks <- SDL.ticks
        let newState = updateState newDir newTicks currentState


        -- clear the drawing
        SDL.rendererDrawColor renderer $= V4 0 0 0 255
        SDL.clear renderer

        -- draw a food
        (food, snake) <- readIORef stateRef
        case food of
          Just ((x, y)) -> do
            SDL.rendererDrawColor renderer $= V4 184 233 134 255
            SDL.fillRect renderer $ Just $ SDL.Rectangle (P (V2 x y)) (V2 gridSize gridSize)
          Nothing -> return ()


        -- draw the snake
        SDL.rendererDrawColor renderer $= V4 248 231 28 255
        forM_ snake $ (\s ->
          SDL.fillRect renderer $ Just $ SDL.Rectangle (P (V2 (fst s) (snd s))) (V2 gridSize gridSize))
        SDL.present renderer

        unless quit loop

  -- start loop with initial state
  rds <- fmap (randomRs (0, 10)) getStdGen
  loop GameState { snake=[(0,0)]
                 , food=Nothing
                 , direction=Nothing
                 , ticks=0
                 , randoms=rds
                 }


  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
