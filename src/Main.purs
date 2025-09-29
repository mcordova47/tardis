module Main where

import Prelude

import Data.DateTime (DateTime)
import Data.DateTime as DT
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Elmish (Dispatch, ReactElement, Transition, fork, forks, subscribe)
import Elmish.Boot (defaultMain)
import Elmish.HTML as H
import Elmish.Subscription (Subscription(..))
import Elmish.TimeMachine (withTimeMachine')
import Elmish.TimeMachine as TM
import Space as S
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener) as E
import Web.HTML (window) as W
import Web.HTML.Window (cancelAnimationFrame, requestAnimationFrame)
import Web.HTML.Window (toEventTarget) as W
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup) as E

main :: Effect Unit
main = defaultMain
  { def: withTimeMachine' TM.defaults { playbackDelay = 1.0 } { init, update, view }
  , elementId: "app"
  }

data Message
  = Load DateTime
  | Tick DateTime
  | Keydown KeyboardEvent
  | Keyup KeyboardEvent

data State
  = Loading
  | Loaded
      { position :: S.Position
      , velocity :: S.Velocity
      , acceleration :: S.Acceleration
      , direction :: S.Direction
      , spin :: Maybe S.Spin
      , timestamp :: DateTime
      }

init :: Transition Message State
init = do
  subscribe Keydown $ keyboardEventSub E.keydown
  subscribe Keyup $ keyboardEventSub E.keyup
  fork $
    Load <$> liftEffect nowDateTime
  pure Loading
  where
    keyboardEventSub event = Subscription \dispatch -> liftEffect do
      listener <- E.eventListener \e -> case KeyboardEvent.fromEvent e of
        Just ke -> dispatch ke
        _ -> pure unit

      W.window <#> W.toEventTarget >>= E.addEventListener event listener false

      pure $
         liftEffect $ W.window <#> W.toEventTarget >>= E.removeEventListener E.keydown listener false

update :: State -> Message -> Transition Message State
update Loading = case _ of
  Load timestamp -> do
    fork $
      Tick <$> liftEffect nowDateTime
    pure $ Loaded
      { position: S.origin
      , velocity: S.resting
      , acceleration: S.Stationary
      , direction: S.up
      , spin: Nothing
      , timestamp
      }
  _ ->
    pure Loading
update (Loaded state) = map Loaded <<< case _ of
  Load _ ->
    pure state
  Tick timestamp -> do
    let
      elapsed = DT.diff timestamp state.timestamp
      position /\ velocity = S.positionAndVelocity state elapsed
      direction = S.direction state elapsed
    forks \{ dispatch, onStop } -> liftEffect do
      id <-
        W.window >>= requestAnimationFrame do
          nextTimeStamp <- nowDateTime
          dispatch $ Tick nextTimeStamp
      onStop $
        liftEffect $ W.window >>= cancelAnimationFrame id
    pure state { position = position, velocity = velocity, direction = direction, timestamp = timestamp }
  Keydown e | KE.code e == "ArrowUp" ->
    pure state { acceleration = S.Accelerating }
  Keydown e | KE.code e == "ArrowDown" ->
    pure state { acceleration = S.Decelerating }
  Keydown e | KE.code e == "ArrowRight" ->
    pure state { spin = Just S.Clockwise }
  Keydown e | KE.code e == "ArrowLeft" ->
    pure $ state { spin = Just S.CounterClockwise }
  Keydown _ ->
    pure state
  Keyup e | KE.code e == "ArrowUp" ->
    pure state { acceleration = S.Stationary }
  Keyup e | KE.code e == "ArrowDown" ->
    pure state { acceleration = S.Stationary }
  Keyup e | KE.code e == "ArrowRight" ->
    pure state { spin = Nothing }
  Keyup e | KE.code e == "ArrowLeft" ->
    pure state { spin = Nothing }
  Keyup _ ->
    pure state

view :: State -> Dispatch Message -> ReactElement
view Loading _ =
  H.empty
view (Loaded state) _ =
  H.div
    { style: H.css
        { position: "fixed"
        , top: 0
        , bottom: 0
        , left: 0
        , right: 0
        , backgroundColor: "#1a1a1a"
        }
    } $
    H.img
      { src: "/assets/tardis.svg"
      , style: H.css
          { position: "fixed"
          , height: "100px"
          , left: "calc(50% + " <> show state.position.x <> "px)"
          , top: "calc(50% + " <> show (-state.position.y) <> "px)"
          , transform: "translate(-50%, -50%) " <> S.rotateCss state.direction
          }
      }
