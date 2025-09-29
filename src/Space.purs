module Space
  ( (|*|)
  , Acceleration(..)
  , Coordinates
  , Direction
  , Position
  , Spin(..)
  , Velocity
  , acceleration
  , coordinates
  , direction
  , down
  , left
  , origin
  , positionAndVelocity
  , resting
  , right
  , rotateCss
  , up
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (cos, pi, pow, sin, (%))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))

-- Coordinates in px where (0, 0) is in the center of the screen
type Position = Coordinates

-- Velocity (x and y) in px/ms
type Velocity = Coordinates

type Coordinates =
  { x :: Number
  , y :: Number
  }

coordinates :: Number -> Number -> Position
coordinates x y = { x, y }

infixl 7 coordinates as |*|

origin :: Position
origin = 0.0 |*| 0.0

resting :: Velocity
resting = 0.0 |*| 0.0

-- Direction in radians where 0 is the positive x direction
type Direction = Number

up :: Direction
up = pi / 2.0

down :: Direction
down = 3.0 * pi / 2.0

left :: Direction
left = 2.0 * pi

right :: Direction
right = 0.0

rotateCss :: Direction -> String
rotateCss dir =
  if rad == 0.0 then
    ""
  else
    "rotate(" <> show rad <> "rad)"
  where
    rad = -(dir - pi / 2.0)

data Acceleration
  = Accelerating
  | Decelerating
  | Stationary

acceleration :: Acceleration -> Number
acceleration = case _ of
  Accelerating -> pxPerSecSq * pow 10.0 (-6.0)
  Decelerating -> -(pxPerSecSq * pow 10.0 (-6.0))
  Stationary -> 0.0
  where
    pxPerSecSq = 50.0

positionAndVelocity ::
  forall r
  . { position :: Position, velocity :: Velocity, acceleration :: Acceleration, direction :: Number | r }
  -> Milliseconds
  -> Position /\ Velocity
positionAndVelocity params (Milliseconds t) =
  x |*| y /\ vx |*| vy
  where
    { position: p0, velocity: v0, direction: θ } = params
    a = acceleration params.acceleration

    ax = a * cos θ
    ay = a * sin θ

    x = p0.x + v0.x * t + 0.5 * ax * pow t 2.0
    y = p0.y + v0.y * t + 0.5 * ay * pow t 2.0

    vx = v0.x + ax * t
    vy = v0.y + ay * t

data Spin
  = Clockwise
  | CounterClockwise

direction :: forall r. { direction :: Direction, spin :: Maybe Spin | r } -> Milliseconds -> Direction
direction params (Milliseconds t) = dir % (2.0 * pi)
  where
    dir = case params.spin of
      Just Clockwise -> params.direction - rotationalVelocity * t
      Just CounterClockwise -> params.direction + rotationalVelocity * t
      Nothing -> params.direction

    rotationalVelocity = pi / 1000.0
