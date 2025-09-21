module Main where

import Prelude

import Data.Array (length, null, uncons, unsnoc)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Elmish (Dispatch, ReactElement, Transition, fork, (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Styled as H

main :: Effect Unit
main = defaultMain
  { def:
      { init
      , update
      , view
      }
  , elementId: "app"
  }

data Message
  = Dec
  | Inc
  | TimeTravelMsg TimeTravelMessage

data TimeTravelMessage
  = Track
  | Undo
  | Redo

type State = State' ( timeTravel :: { history :: Array (State' ()), future :: Array (State' ()) } )
type State' r =
  { count :: Int
  | r
  }

init :: Transition Message State
init = pure { count: 0, timeTravel: initTimeTravel }

initTimeTravel :: { history :: Array (State' ()), future :: Array (State' ()) }
initTimeTravel = { history: [{ count: 0 }], future: [] }

update :: State -> Message -> Transition Message State
update state = case _ of
  Dec -> do
    fork $ pure $ TimeTravelMsg Track
    pure state { count = state.count - 1 }
  Inc -> do
    fork $ pure $ TimeTravelMsg Track
    pure state { count = state.count + 1 }
  TimeTravelMsg msg -> updateTimeTravel state msg # lmap TimeTravelMsg

updateTimeTravel :: State -> TimeTravelMessage -> Transition TimeTravelMessage State
updateTimeTravel state = case _ of
  Track -> pure state { timeTravel = { history: state.timeTravel.history <> [{ count: state.count }], future: [] } }
  Undo -> case unsnoc state.timeTravel.history of
    Just { init, last } -> case unsnoc init of
      Just { last: current } -> pure state { count = current.count, timeTravel = { history: init, future: [last] <> state.timeTravel.future } }
      Nothing -> pure state
    Nothing -> pure state
  Redo -> case uncons state.timeTravel.future of
    Just { head, tail } -> pure state { count = head.count, timeTravel = { history: state.timeTravel.history <> [head], future: tail } }
    Nothing -> pure state

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div ""
  [ H.h1 "" "Time Travel"
  , H.h2 "" "Counter"
  , H.button_ "btn"
      { onClick: dispatch <| Dec }
      "-"
  , H.span "" $ "Count: " <> show state.count
  , H.button_ "btn"
      { onClick: dispatch <| Inc }
      "+"
  , H.h2 "" "Time Machine"
  , H.button_ "btn"
      { onClick: dispatch <| TimeTravelMsg Undo, disabled: length state.timeTravel.history <= 1 }
      "↩️"
  , H.button_ "btn"
      { onClick: dispatch <| TimeTravelMsg Redo, disabled: null state.timeTravel.future }
      "↪️"
  ]
