module TimeTravel
  ( Keybindings
  , Message
  , withTimeTravel
  , withTimeTravel'
  )
  where

import Prelude

import Data.Foldable (for_)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Maybe (Maybe(..))
import Debug as Debug
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Elmish (ComponentDef', ReactElement, forks, lmap, subscribe, (<|))
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Elmish.HTML as H
import Elmish.React (class ReactChildren)
import Elmish.Subscription (Subscription(..))
import TimeTravel.History (History)
import TimeTravel.History as History
import Web.DOM (Element)
import Web.DOM.Document (createElement) as DOM
import Web.DOM.Element as Element
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.NonElementParentNode (getElementById) as W
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window) as W
import Web.HTML.HTMLDocument (body, toDocument, toNonElementParentNode) as W
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document) as W
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.KeyboardEvent.EventTypes (keydown)

data Message msg
  = Message msg
  -- Controls
  | Undo
  | Redo
  -- UI
  | ToggleExpanded
  -- Keydown
  | Keydown KeyboardEvent

type State msg s =
  { history :: History msg s
  , visible :: Boolean
  , expanded :: Boolean
  , keybindings :: Keybindings
  }

type Keybindings =
  { toggle :: KeyboardEvent -> Boolean
  }

withTimeTravel ::
  forall m msg state
  . Debug.DebugWarning
  => MonadEffect m
  => Functor m
  => ComponentDef' m msg state
  -> ComponentDef' m (Message msg) (State msg state)
withTimeTravel =
  withTimeTravel'
    { toggle: \e ->
        (KeyboardEvent.ctrlKey e || KeyboardEvent.metaKey e) &&
        KeyboardEvent.altKey e &&
        KeyboardEvent.code e == "KeyZ"
    }

withTimeTravel' ::
  forall m msg state
  . Debug.DebugWarning
  => MonadEffect m
  => Functor m
  => Keybindings
  -> ComponentDef' m msg state
  -> ComponentDef' m (Message msg) (State msg state)
withTimeTravel' keybindings def = { init, update, view }
  where
    init = do
      subscribe Keydown keydownSub
      state <- def.init # lmap Message
      pure
        { history: History.init state
        , visible: true
        , expanded: false
        , keybindings
        }

    update state = case _ of
      Message msg -> do
        next <- def.update (History.presentState state.history) msg # lmap Message
        pure state { history = History.track state.history msg next }
      Undo ->
        pure state { history = History.undo state.history }
      Redo ->
        pure state { history = History.redo state.history }
      ToggleExpanded ->
        pure state { expanded = not state.expanded }
      Keydown e | state.keybindings.toggle e ->
        pure state { visible = not state.visible }
      Keydown _ ->
        pure state

    view { history, visible, expanded } dispatch =
      H.fragment
      [ def.view (History.presentState history) $ dispatch <<< Message
      , if visible then
          portal
            { id: "tardis-time-machine"
            , content:
                H.div
                  { style: H.css
                      { position: "fixed"
                      , bottom: "1rem"
                      , right: "1rem"
                      , padding: "0.75rem"
                      , border: "1px solid lightgray"
                      , borderRadius: "0.5rem"
                      }
                  , tabIndex: -1
                  }
                  [ H.div
                    { style: H.css { display: "flex", alignItems: "center" }
                    }
                    [ button
                        { onClick: dispatch <| Undo
                        , disabled: not History.hasPast history
                        }
                        "↩️"
                    , H.em { style: H.css { color: "gray" } } $
                        formatMessage false $
                          History.presentMessage history
                    , button
                        { onClick: dispatch <| Redo
                        , disabled: not History.hasFuture history
                        }
                        "↪️"
                    , button
                        { onClick: dispatch <| ToggleExpanded
                        , disabled: false
                        }
                        if expanded then "▼" else "▶"
                    ]
                  , if expanded then
                      H.div {}
                      [ H.h6 {} "Last Message"
                      , H.pre {} $
                          formatMessage true $ History.presentMessage history
                      , H.h6 {} "Current State"
                      , H.pre {} $
                          formatState $ History.presentState history
                      ]
                    else
                      H.empty
                  ]
            }
        else
          H.empty
      ]
      where
        button :: forall c. ReactChildren c => _ -> c -> _
        button { onClick, disabled } label =
          H.button
            { onClick
            , disabled
            , style: H.css
                { display: "inline-block"
                , textAlign: "center"
                , textDecoration: "none"
                , verticalAlign: "middle"
                , cursor: if disabled then "default" else "pointer"
                , backgroundColor: "transparent"
                , border: "none"
                , padding: "0.375rem 0.75rem"
                , fontSize: "1rem"
                , borderRadius: "0.25rem"
                }
            }
            label

    keydownSub = Subscription \dispatch -> liftEffect do
      listener <- eventListener \e -> case KeyboardEvent.fromEvent e of
        Just ke -> dispatch ke
        _ -> pure unit

      W.window <#> toEventTarget >>= addEventListener keydown listener false

      pure $
         liftEffect $ W.window <#> toEventTarget >>= removeEventListener keydown listener false

-- Utils

portal :: { id :: String, content :: ReactElement } -> ReactElement
portal = wrapWithLocalState (ComponentName "Portal") \{ id, content } ->
  { init: do
      forks \{ dispatch } -> liftEffect $ do
        mContainer <- elementById id
        case mContainer of
          Just container ->
            dispatch container
          Nothing -> do
            doc <- W.document =<< W.window
            mBody <- W.body doc
            for_ mBody \b -> do
              container <- DOM.createElement "div" $ W.toDocument doc
              Element.setId id container
              DOM.appendChild (Element.toNode container) (HTMLElement.toNode b)
              dispatch container
      pure Nothing
  , update: \_ container -> pure $ Just container
  , view: \container _ ->
      case container of
        Just c ->
          createPortal content c
        Nothing ->
          H.empty
  }
  where
    elementById :: String -> Effect (Maybe Element)
    elementById id =
      W.window
      >>= W.document
      <#> W.toNonElementParentNode
      >>= W.getElementById id

createPortal :: ReactElement -> Element -> ReactElement
createPortal = runFn2 createPortal_

foreign import createPortal_ :: Fn2 ReactElement Element ReactElement

formatMessage :: forall a. Boolean -> a -> String
formatMessage = runFn2 formatMessage_

foreign import formatMessage_ :: forall a. Fn2 Boolean a String

formatState :: forall a. a -> String
formatState = runFn1 formatState_

foreign import formatState_ :: forall a. Fn1 a String
