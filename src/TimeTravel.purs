module TimeTravel
  ( withTimeTravel
  )
  where

import Debug as Debug
import Elmish (ComponentDef')

withTimeTravel ::
  forall m msg state
  . Debug.DebugWarning
  => ComponentDef' m msg state
  -> ComponentDef' m msg state
withTimeTravel def =
  { init: init'
  , update: update'
  , view: view'
  }
  where
    { init, update, view } = def

    init' = init

    update' = update

    view' = view
