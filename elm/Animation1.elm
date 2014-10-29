{-

experiement with abstracting an animation

-}

import Graphics.Input (Input, input, clickable)

data Pos = Left | Right
data Moving = Stopped Pos | Moving

type GameState = { moving:Moving }

startState : GameState
startState = GameState (Stopped Left)

clickSignal : Input ()
clickSignal = input ()

data Crazy = Click | FpsUpdate

crazySignal : Signal Crazy
crazySignal = let c = lift (\_ -> Click) clickSignal.signal
                  f = lift (\_ -> FpsUpdate) <| fps 10
              in
                merge c f
                
currentState : Signal GameState
currentState = foldp (\sig st -> st) startState crazySignal

updateState : Crazy -> GameState -> GameState
updateState c s = case c of
                    Click -> { s | moving <- Moving }
                    FpsUpdate -> if | s.moving == Stopped _ -> s
                                    | otherwise -> 
main = color green <| collage 400 400 [moveX -150 <|
                                       toForm <| 
                                       clickable clickSignal.handle () <| 
                                       color red <| 
                                       spacer 100 100]