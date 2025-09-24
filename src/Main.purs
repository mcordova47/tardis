module Main where

import Prelude

import Effect (Effect)
import Elmish (Dispatch, ReactElement, Transition, (<|))
import Elmish.Boot (defaultMain)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.TimeMachine (withTimeMachine)

main :: Effect Unit
main = defaultMain
  { def: withTimeMachine { init, update, view }
  , elementId: "app"
  }

data Message
  = ChangeInput String

type State =
  { input :: String
  }

init :: Transition Message State
init = pure { input: "" }

update :: State -> Message -> Transition Message State
update _ = case _ of
  ChangeInput input ->
    pure { input }

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div "container"
  [ H.h1 "" "Time Travel"
  , H.h2 "" "Input"
  , H.input_ "form-control"
      { value: state.input
      , onChange: dispatch <| ChangeInput <<< E.inputText
      }
  ]
