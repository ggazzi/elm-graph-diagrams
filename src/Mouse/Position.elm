module Mouse.Position exposing (pagePosition, positionWithinTarget, viewportPosition)

{-| This module provides JSON decoders that extract positions of `MouseEvents`.

The positions of mouse events may be obtained in different coordinate systems (e.g. relative to the
viewport, relative to the target element...), as defined in the following specifications:

  * [DOM Level 2](https://www.w3.org/TR/DOM-Level-2-Events/events.html#Events-MouseEvent)

  * [DOM Level 3](https://w3c.github.io/uievents/#events-mouseevent) working draft

  * [CSSOM](https://drafts.csswg.org/cssom-view/#extensions-to-the-mouseevent-interface)
    working draft

@docs pagePosition, positionWithinTarget, viewportPosition
-}

import Json.Decode as Json
import Position exposing (Position)


{-| Decodes a `MouseEvent` and obtains its position, in pixels, relative to the whole document.
This position is not affected by scrolling the page.

This reads the `pageX` and `pageY` fields of the `MouseEvent` object,
as defined in the CSSOM draft specification.
-}
pagePosition : Json.Decoder Position
pagePosition =
    Json.map2 Position
        (Json.field "pageX" Json.float)
        (Json.field "pageY" Json.float)


{-| Decodes a `MouseEvent` and obtains its position relative to the target element
(more precisely, to the padding edge of the target node). This position is not affected
by scrolling the page.

This reads the `offsetX` and `offsetY` fields of the `MouseEvent` object,
as defined in the CSSOM draft specfication.
-}
positionWithinTarget : Json.Decoder Position
positionWithinTarget =
    Json.map2 Position
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)


{-| Decodes a `MouseEvent` and obtains its position, in pixels, relative to the client area
(i.e. current viewport). This position is affected by scrolling the page.

This reads the `clientX` and `clientY` fields of the `MouseEvent` object, as defined
in the DOM Level 2 specification and the the CSSOM draft specification.
-}
viewportPosition : Json.Decoder Position
viewportPosition =
    Json.map2 Position
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
