module GraphView
    exposing
        ( State
        , Msg
        , init
        , update
        , subscriptions
        , view
          -- Configuration of Events
        , Config
        , Target(..)
        , config
        , Output(..)
          -- View Model
        , Node
        , Edge
        , Endpoint
        , Shape(..)
        )

{-|
This module provides utilities for displaying interactive graph-based diagrams, and handling user
events such as clicking and dragging elements of the diagram.

# View and View Model

@docs view, Node, Edge, Endpoint, Shape

# Model and Subscriptions

@docs State, init, subscriptions

# Update and Events

@docs update, Msg, Output, Config, config, Target

-}

import Draggable as Draggable exposing (Delta)
import Draggable.Events as Draggable
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Mouse.Modifiers as Mouse
import Mouse.Position as Mouse
import Position exposing (Position, Positioned, positionOf, moveBy)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Svg
import Svg.Keyed


-- MODEL


{-| Opaque type representing the internal state of the graph view.
-}
type State
    = State
        { interaction : InteractionState
        , drag : Draggable.State ()
        }


{-|
Temporarily stores the mouse state when the user has pressed a mouse button, but neither a
click nor a drag were identified yet.
-}
type InteractionState
    = Idle
    | AwaitingInteraction Target Position Mouse.Modifiers


{-| Initial state for the graph view.
-}
init : State
init =
    State
        { interaction = Idle
        , drag = Draggable.init
        }



-- UPDATE


{-| An internal message necessary to update the graph view.
-}
type Msg
    = OnClick
    | OnDragStart
    | OnDragBy Delta
    | OnDragEnd
    | OnMouseUp Target
    | DragMsgWithMouseState (Draggable.Msg ()) ( Target, Position, Mouse.Modifiers )
    | DragMsg (Draggable.Msg ())


{-|
 Possible outputs of updating the graph view: either a message to the parent module,
 a command to be run, or nothing.
-}
type Output msg
    = OutCmd (Cmd msg)
    | OutMsg msg
    | NoOutput


{-|
Handle internal update messages for the view model, returning the updated state and one of the
following:
  * A command that should be sent to the environment
  * A message that should be handled by the containing module
  * No output
-}
update : (Msg -> msg) -> Config msg -> Msg -> State -> ( State, Output msg )
update envelope (Config config) msg (State ({ interaction } as model)) =
    case msg of
        OnClick ->
            case interaction of
                AwaitingInteraction target position modifiers ->
                    ( State model, asOutput <| config.onClick target position modifiers )

                Idle ->
                    ( State model, NoOutput )

        OnDragStart ->
            case interaction of
                AwaitingInteraction target position modifiers ->
                    ( State model, asOutput <| config.onDragStart target position modifiers )

                Idle ->
                    ( State model, NoOutput )

        OnDragBy delta ->
            ( State model, asOutput <| config.onDragBy delta )

        OnDragEnd ->
            ( State model, asOutput <| config.onDragEnd )

        OnMouseUp target ->
            ( State model, asOutput <| config.onMouseUp target )

        DragMsgWithMouseState dragMsg ( target, position, modifiers ) ->
            let
                ( newModel, cmd ) =
                    Draggable.update draggableConfig dragMsg model
            in
                ( State { newModel | interaction = AwaitingInteraction target position modifiers }
                , OutCmd <| Cmd.map envelope cmd
                )

        DragMsg dragMsg ->
            let
                ( newModel, cmd ) =
                    Draggable.update draggableConfig dragMsg model
            in
                ( State newModel, OutCmd <| Cmd.map envelope cmd )


asOutput : Maybe msg -> Output msg
asOutput maybeMsg =
    case maybeMsg of
        Just msg ->
            OutMsg msg

        Nothing ->
            NoOutput


draggableConfig : Draggable.Config () Msg
draggableConfig =
    Draggable.customConfig
        [ Draggable.onClick (\_ -> OnClick)
        , Draggable.onDragStart (\_ -> OnDragStart)
        , Draggable.onDragBy OnDragBy
        , Draggable.onDragEnd OnDragEnd
        ]



-- CONFIG


{-|
Configuration for updates. This should translate low-level `Msg`s describing the mouse events to
high-level `msg`s describing the user intent.
-}
type Config msg
    = Config
        { onClick : Target -> Position -> Mouse.Modifiers -> Maybe msg
        , onDragStart : Target -> Position -> Mouse.Modifiers -> Maybe msg
        , onDragBy : Delta -> Maybe msg
        , onDragEnd : Maybe msg
        , onMouseUp : Target -> Maybe msg
        }


{-| Type representing possible targets of interaction.
-}
type Target
    = OnBackground
    | OnNode Int


{-| Create a configuration for the update function, handling the given events.
-}
config :
    { onClick : Target -> Position -> Mouse.Modifiers -> Maybe msg
    , onDragStart : Target -> Position -> Mouse.Modifiers -> Maybe msg
    , onDragBy : Delta -> Maybe msg
    , onDragEnd : Maybe msg
    , onMouseUp : Target -> Maybe msg
    }
    -> Config msg
config =
    Config



-- SUBSCRIPTIONS


{-| Create any mouse subscriptions used for dragging.
-}
subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions envelope (State { drag }) =
    Draggable.subscriptions (envelope << DragMsg) drag



-- VIEW MODEL


{-| Minimal information needed to display a node.
-}
type alias Node msg =
    { id : Int
    , x : Float
    , y : Float
    , view : List (Svg msg)
    }


{-| Minimal information needed to display an edge.

The edge will be drawn as a series of line segments that starts on the source, ends on the target
and goes through each of the control points in order. Furthermore, each control point will be drawn
as a dot.
-}
type alias Edge =
    { id : ( Int, Int )
    , source : Endpoint
    , target : Endpoint
    , controlPoints : List Position
    }


{-| Endpoint of an edge.
-}
type alias Endpoint =
    { x : Float, y : Float, shape : Shape }


{-| Possible shapes for nodes.
-}
type Shape
    = NoShape
    | Circle Float
    | Rectangle Float Float



-- VIEW


{-| Display a graph-based diagram. The first argument describes how the nodes and edges will be
identified and displayed. The `List node` and `List edge` arguments provide the graph elements to
be displayed. The `List (Svg msg)` argument provides additional elements that should be displayed
alongside the nodes and edges.

__Note:__ The `List Node`, `List Edge` and `List (Svg msg)` arguments should be computed with
information from the model, but generally not stored in it.
-}
view : (Msg -> msg) -> List (Node msg) -> List Edge -> List (Svg msg) -> Html msg
view envelope nodes edges additionalElements =
    Svg.svg
        [ style
            [ ( "margin", "20px" )
            , ( "width", "800px" )
            , ( "height", "600px" )
            ]
        ]
        ([ Svg.defs [] [ arrowhead.svg ]
         , background envelope
         , Svg.Keyed.node "g"
            [ Attr.class "elm-graph-diagrams__edges-view"
            , Attr.stroke "black"
            , Attr.cursor "pointer"
            ]
            (List.map (\edge -> ( toString edge.id, edgeView edge )) edges)
         , Svg.Keyed.node "g"
            [ Attr.class "elm-graph-diagrams__nodes-view" ]
            (List.map (\node -> ( toString node.id, nodeView envelope node )) nodes)
         ]
            ++ additionalElements
        )


background : (Msg -> msg) -> Svg msg
background envelope =
    Svg.rect
        ([ Attr.width "100%"
         , Attr.height "100%"
         , Attr.fill "transparent"
         , Attr.stroke "lightgrey"
         , Attr.strokeWidth "2px"
         , Attr.rx "5px"
         , Attr.ry "5px"
         , Attr.cursor "crosshair"
         ]
            ++ handlerAttributes envelope OnBackground
        )
        []



-- NODES


nodeView : (Msg -> msg) -> Node msg -> Svg msg
nodeView envelope node =
    let
        translate =
            "translate(" ++ toString node.x ++ "," ++ toString node.y ++ ")"
    in
        Svg.g
            ([ Attr.class "elm-graph-diagrams__node"
             , Attr.transform translate
             , Attr.cursor "pointer"
             ]
                ++ handlerAttributes envelope (OnNode node.id)
            )
            node.view



-- EDGES


edgeView : Edge -> Svg msg
edgeView { source, target, controlPoints } =
    let
        segments =
            adjustSegments source target controlPoints
    in
        Svg.g [ Attr.class "elm-graph-diagrams__edge" ]
            ([ Svg.path
                [ Attr.stroke edgeColor
                , Attr.strokeWidth "1.5"
                , Attr.fill "none"
                , Attr.markerEnd ("url(#" ++ arrowhead.id ++ ")")
                , Attr.d (renderSegments segments)
                ]
                []
             ]
                ++ List.map renderControlPoint controlPoints
            )


renderControlPoint : Position -> Svg msg
renderControlPoint { x, y } =
    let
        translate =
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"
    in
        Svg.circle
            [ Attr.class "elm-graph-diagrams__edge__ctrlpoint"
            , Attr.r "2"
            , Attr.fill "black"
            , Attr.transform translate
            ]
            []


renderSegments : List Position -> String
renderSegments segments =
    case segments of
        [] ->
            ""

        start :: rest ->
            String.concat (moveTo start :: List.map lineTo rest)


adjustSegments : Endpoint -> Endpoint -> List Position -> List Position
adjustSegments first last middle =
    case middle of
        [] ->
            let
                segment =
                    segmentFromPoints (positionOf first) (positionOf last)

                adjusted =
                    segment
                        |> shrinkSegmentTo
                            ( intersectAtStart segment first.shape
                            , intersectAtEnd segment last.shape - arrowhead.length / segment.length
                            )
            in
                [ positionOf adjusted.start
                , positionOf adjusted.end
                ]

        second :: _ ->
            let
                segment =
                    segmentFromPoints (positionOf first) second

                adjusted =
                    segment
                        |> shrinkSegmentTo ( intersectAtStart segment first.shape, 1 )
            in
                adjusted.start :: second :: adjustLastSegment last middle


adjustLastSegment : Endpoint -> List Position -> List Position
adjustLastSegment last middle =
    case middle of
        [ secondLast ] ->
            let
                segment =
                    segmentFromPoints secondLast (positionOf last)

                adjusted =
                    segment
                        |> shrinkSegmentTo ( 0, intersectAtEnd segment last.shape - arrowhead.length / segment.length )
            in
                [ secondLast, adjusted.end ]

        point :: rest ->
            point :: adjustLastSegment last rest

        [] ->
            Debug.crash "unreachable code"


type alias Segment =
    { start : Position
    , end : Position
    , dx : Float
    , dy : Float
    , length : Float
    }


segmentFromPoints : Position -> Position -> Segment
segmentFromPoints start end =
    let
        ( dx, dy ) =
            ( end.x - start.x, end.y - start.y )
    in
        { start = start
        , end = end
        , dx = dx
        , dy = dy
        , length = sqrt (dx * dx + dy * dy)
        }


reverseSegment : Segment -> Segment
reverseSegment { start, end, dx, dy, length } =
    { start = end, end = start, dx = -dx, dy = -dy, length = length }


shrinkSegmentTo : ( Float, Float ) -> Segment -> Segment
shrinkSegmentTo ( tStart, tEnd ) ({ start, end, dx, dy, length } as segment) =
    let
        ( tStart_, tEnd_ ) =
            ( clamp 0 1 tStart, clamp 0 1 tEnd )

        dt =
            tEnd_ - tStart_
    in
        { segment
            | start = { start | x = start.x + tStart_ * dx, y = start.y + tStart_ * dy }
            , end = { end | x = start.x + tEnd_ * dx, y = start.y + tEnd_ * dy }
            , dx = dx * dt
            , dy = dy * dt
            , length = length * dt
        }


{-| Given a line segment and a shape centered at the segment's
starting point, calculate the intersection between them.

The intersection is returned as a parameter `t` for the line segment's
equation. The point of intersection may be calculated with
`(segment.start.x + t * segment.dx, segment.start.y + t * segment.dy)`.

The existence of a unique endpoint is guaranteed since all `Shape`s
are convex, the segment's starting point is within the shape.
-}
intersectAtStart : Segment -> Shape -> Float
intersectAtStart segment shape =
    let
        t =
            intersectAtEnd (reverseSegment segment) shape
    in
        1 - t


{-| Given a line segment and a shape centered at the segment's
endpoint, calculate the intersection between them.

The intersection is returned as a parameter `t` for the line segment's
equation. The point of intersection may be calculated with
`(segment.start.x + t * segment.dx, segment.start.y + t * segment.dy)`.

The existence of a unique endpoint is guaranteed since all `Shape`s
are convex, the segment's endpoint is within the shape.
-}
intersectAtEnd : Segment -> Shape -> Float
intersectAtEnd { start, end, dx, dy, length } shape =
    case shape of
        NoShape ->
            1

        Circle radius ->
            1 - (radius / length)

        Rectangle width height ->
            let
                intersect1D length start end delta =
                    let
                        halfLength =
                            length / 2

                        ( min, max ) =
                            ( end - halfLength, end + halfLength )
                    in
                        if delta > 1.0e-5 then
                            (min - start) / delta
                        else if delta < -1.0e-5 then
                            (max - start) / delta
                        else
                            1
            in
                max (intersect1D width start.x end.x dx) (intersect1D height start.y end.y dy)



-- UTILITIES


handlerAttributes : (Msg -> msg) -> Target -> List (Svg.Attribute msg)
handlerAttributes envelope target =
    let
        parseMouseState =
            Json.map2 (\position modifiers -> ( target, position, modifiers ))
                Mouse.positionWithinTarget
                Mouse.modifiers
    in
        [ Draggable.customMouseTrigger parseMouseState
            (\dragMsg mouseState -> envelope <| DragMsgWithMouseState dragMsg mouseState)
        , Svg.on "mouseup" (Json.succeed <| envelope <| OnMouseUp target)
        ]


type alias Marker a =
    { id : String
    , length : Float
    , svg : Svg a
    }


type alias PathFragment =
    String


moveTo : Position -> PathFragment
moveTo { x, y } =
    "M" ++ toString x ++ " " ++ toString y


lineTo : Position -> PathFragment
lineTo { x, y } =
    "L" ++ toString x ++ " " ++ toString y



-- VIEW CONSTANTS


edgeColor : String
edgeColor =
    "black"


arrowhead : Marker a
arrowhead =
    { id = "arrowhead"
    , length = 2
    , svg =
        Svg.marker
            [ Attr.id "arrowhead"
            , Attr.viewBox "0 -5 10 10"
            , Attr.refX "8"
            , Attr.markerWidth "4"
            , Attr.markerHeight "6"
            , Attr.orient "auto"
            ]
            [ Svg.path
                [ Attr.d "M0,-5L10,0L0,5"
                , Attr.fill "transparent"
                , Attr.stroke edgeColor
                , Attr.strokeWidth "2"
                ]
                []
            ]
    }
