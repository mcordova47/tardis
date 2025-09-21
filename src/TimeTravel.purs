module TimeTravel
  ( Message
  , withTimeTravel
  )
  where

import Prelude

import Debug as Debug
import Elmish (ComponentDef', bimap, lmap, (<|))
import Elmish.HTML.Styled as H
import TimeTravel.History (History)
import TimeTravel.History as History

data Message msg
  = Message msg
  -- UI Visibility
  | Show
  | Hide
  -- Controls
  | Undo
  | Redo

type State s =
  { state :: s
  , history :: History s
  }

withTimeTravel ::
  forall m msg state
  . Debug.DebugWarning
  => Functor m
  => ComponentDef' m msg state
  -> ComponentDef' m (Message msg) (State state)
withTimeTravel def =
  { init: init'
  , update: update'
  , view: view'
  }
  where
    { init, update, view } = def

    init' = do
      state <- init # lmap Message
      pure
        { state
        , history: History.empty state
        }

    update' state = case _ of
      Message msg -> do
        state' <- update state.state msg # bimap Message state { state = _ }
        pure state' { history = History.track state.history state'.state }
      Show ->
        pure state -- TODO
      Hide ->
        pure state -- TODO
      Undo ->
        pure $ timeTravel History.undo state
      Redo ->
        pure $ timeTravel History.redo state

    view' { state, history } dispatch =
      H.fragment
      [ view state $ dispatch <<< Message
      , H.div ""
        [ H.h6 "" "Time Machine"
        , H.button_ "btn"
            { onClick: dispatch <| Undo
            , disabled: not History.hasPast history
            }
            "↩️"
        , H.button_ "btn"
            { onClick: dispatch <| Redo
            , disabled: not History.hasFuture history
            }
            "↪️"
        ]
      ]

timeTravel :: forall s. (History s -> History s) -> State s -> State s
timeTravel upd state =
  { state: History.present history
  , history
  }
  where
    history = upd state.history
