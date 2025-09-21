module TimeTravel.History
  ( History
  , Message
  , future
  , hasFuture
  , hasPast
  , init
  , past
  , present
  , presentMessage
  , presentState
  , redo
  , track
  , undo
  )
  where

import Prelude

import Data.List (List(..), (:))
import Data.List as List
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

newtype History msg s = History
  { past :: List (Message msg /\ s)
  , present :: Message msg /\ s
  , future :: List (Message msg /\ s)
  }

data Message msg
  = Message msg
  | Init

-- Constructors

init :: forall msg s. s -> History msg s
init s = History
  { past: Nil
  , present: Init /\ s
  , future: Nil
  }

-- Accessors

present :: forall msg s. History msg s -> Message msg /\ s
present (History h) = h.present

presentState :: forall msg s. History msg s -> s
presentState = snd <<< present

presentMessage :: forall msg s. History msg s -> Message msg
presentMessage = fst <<< present

past :: forall msg s. History msg s -> List (Message msg /\ s)
past (History h) = h.past

future :: forall msg s. History msg s -> List (Message msg /\ s)
future (History h) = h.future

hasPast :: forall msg s. History msg s -> Boolean
hasPast (History h) = not List.null h.past

hasFuture :: forall msg s. History msg s -> Boolean
hasFuture (History h) = not List.null h.future

-- Controls

undo :: forall msg s. History msg s -> History msg s
undo (History h) = case h.past of
  present' : past' ->
    History
      { past: past'
      , present: present'
      , future: h.present : h.future
      }
  Nil ->
    History h

redo :: forall msg s. History msg s -> History msg s
redo (History h) = case h.future of
  present' : future' ->
    History
      { past: h.present : h.past
      , present: present'
      , future: future'
      }
  Nil ->
    History h

track :: forall msg s. History msg s -> msg -> s -> History msg s
track (History h) msg next = History
  { past: h.present : h.past
  , present: Message msg /\ next
  , future: Nil
  }
