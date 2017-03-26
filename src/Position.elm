module Position
    exposing
        ( Position
        , Positioned
        , positionOf
        , Delta
        , moveBy
        )

{-| This module provides utilities for dealing with positions and positioned things.

@docs Position, Positioned, positionOf, Delta, moveBy

-}


{-| Type for representing positions.
-}
type alias Position =
    { x : Float, y : Float }


{-| Record types that contain a position.
-}
type alias Positioned a =
    { a | x : Float, y : Float }


{-| Obtain the position of a positioned thing.
-}
positionOf : Positioned a -> Position
positionOf { x, y } =
    { x = x, y = y }


{-| Type for representing displacements.
-}
type alias Delta =
    ( Float, Float )


{-| Move a positioned record according to the given displacement.
-}
moveBy : Delta -> Positioned a -> Positioned a
moveBy ( dx, dy ) ({ x, y } as object) =
    { object
        | x = x + dx
        , y = y + dy
    }
