module Main where

import Prelude

import Data.Array ((!!))
import Data.DateTime (DateTime)
import Data.DateTime as DT
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
import Web.HTML.Window (innerHeight, innerWidth, toEventTarget) as W
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
  = Load DateTime S.Coordinates
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
      , pageSize :: S.Coordinates
      , image :: { row :: Int, col :: Int }
      }

init :: Transition Message State
init = do
  subscribe Keydown $ keyboardEventSub E.keydown
  subscribe Keyup $ keyboardEventSub E.keyup
  fork $ liftEffect do
    window <- W.window
    x <- W.innerWidth window <#> Int.toNumber
    y <- W.innerHeight window <#> Int.toNumber
    timestamp <- nowDateTime
    pure $ Load timestamp { x, y }
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
  Load timestamp pageSize -> do
    fork $
      Tick <$> liftEffect nowDateTime
    pure $ Loaded
      { position: S.origin
      , velocity: S.resting
      , acceleration: S.Stationary
      , direction: S.up
      , spin: Nothing
      , timestamp
      , pageSize
      , image: { row: 0, col: 0 }
      }
  _ ->
    pure Loading
update (Loaded state) = map Loaded <<< case _ of
  Load _ _ ->
    pure state
  Tick timestamp -> do
    let
      elapsed = DT.diff timestamp state.timestamp
      direction = S.direction state elapsed
      position /\ velocity = S.positionAndVelocity state elapsed
      pos
        | position.x > state.pageSize.x / 2.0 =
            position { x = position.x - state.pageSize.x } /\ state.image { col = state.image.col + 1 }
        | position.x < -state.pageSize.x / 2.0 =
            position { x = position.x + state.pageSize.x } /\ state.image { col = state.image.col - 1 }
        | position.y > state.pageSize.y / 2.0 =
            position { y = position.y - state.pageSize.y } /\ state.image { row = state.image.row + 1 }
        | position.y < -state.pageSize.y / 2.0 =
            position { y = position.y + state.pageSize.y } /\ state.image { row = state.image.row - 1 }
        | otherwise =
            position /\ state.image
      position' /\ image = pos
    forks \{ dispatch, onStop } -> liftEffect do
      id <-
        W.window >>= requestAnimationFrame do
          nextTimeStamp <- nowDateTime
          dispatch $ Tick nextTimeStamp
      onStop $
        liftEffect $ W.window >>= cancelAnimationFrame id
    pure state { position = position', velocity = velocity, direction = direction, timestamp = timestamp, image = image }
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
        , backgroundColor: "#101010ff"
        , backgroundImage: "url(/assets/" <> image <> ")"
        , backgroundSize: "cover"
        , backgroundRepeat: "no-repeat"
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
  where
    image =
      images !! (mod state.image.row 3) # maybe defaultImage \row ->
        row !! (mod state.image.col 3) # fromMaybe defaultImage
    images =
      [ ["space1.jpeg", "space2.webp", "space3.webp"]
      , ["space4.webp", "space5.webp", "space6.webp"]
      , ["space7.webp", "space8.webp", "space9.webp"]
      ]
    defaultImage = "space1.jpeg"
