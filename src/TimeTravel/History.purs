module TimeTravel.History
  ( History
  , empty
  , future
  , hasFuture
  , hasPast
  , past
  , present
  , redo
  , track
  , undo
  )
  where

import Prelude

import Data.List (List(..), (:))
import Data.List as List

newtype History s = History
  { past :: List s
  , present :: s
  , future :: List s
  }

-- Constructors

empty :: forall s. s -> History s
empty s = History
  { past: Nil
  , present: s
  , future: Nil
  }

-- Accessors

present :: forall s. History s -> s
present (History h) = h.present

past :: forall s. History s -> List s
past (History h) = h.past

future :: forall s. History s -> List s
future (History h) = h.future

hasPast :: forall s. History s -> Boolean
hasPast (History h) = not List.null h.past

hasFuture :: forall s. History s -> Boolean
hasFuture (History h) = not List.null h.future

-- Controls

undo :: forall s. History s -> History s
undo (History h) = case h.past of
  present' : past' ->
    History
      { past: past'
      , present: present'
      , future: h.present : h.future
      }
  Nil ->
    History h

redo :: forall s. History s -> History s
redo (History h) = case h.future of
  present' : future' ->
    History
      { past: h.present : h.past
      , present: present'
      , future: future'
      }
  Nil ->
    History h

track :: forall s. History s -> s -> History s
track (History h) next = History
  { past: h.present : h.past
  , present: next
  , future: Nil
  }
